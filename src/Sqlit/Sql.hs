{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Sqlit.Sql
  ( Sql (..),
    stringToSqlQ,
  )
where

import Control.Monad.Combinators
import Data.Char (isDigit, isLower, isUpper)
import Data.Foldable
import Data.Text qualified as Text
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax qualified as TH
import Sqlit.Prelude
import Sqlit.Row (RowEncoder, columnEncoder)
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char qualified as Megaparsec

data Sql
  = Sql Text RowEncoder

instance Monoid Sql where
  mappend = (<>)
  mempty = Sql mempty mempty

instance Semigroup Sql where
  (<>) :: Sql -> Sql -> Sql
  Sql s0 e0 <> Sql s1 e1 =
    Sql (s0 <> s1) (e0 <> e1)

data Segment
  = Segment'Raw Text
  | Segment'Var Text
  deriving stock (Show)

stringToSqlQ :: String -> TH.Q TH.Exp
stringToSqlQ =
  textToSqlQ . Text.pack

textToSqlQ :: Text -> TH.Q TH.Exp
textToSqlQ =
  segmentsToSql . textToSegments

textToSegments :: Text -> [Segment]
textToSegments text =
  case Megaparsec.parse parser "" text of
    Left err -> error (Megaparsec.errorBundlePretty err)
    Right segments -> segments
  where
    parser :: Megaparsec.Parsec Void Text [Segment]
    parser =
      many segmentParser

    segmentParser :: Megaparsec.Parsec Void Text Segment
    segmentParser =
      asum
        [ Segment'Raw <$> rawSegmentParser,
          Segment'Var <$> varSegmentParser
        ]

    rawSegmentParser :: Megaparsec.Parsec Void Text Text
    rawSegmentParser =
      asum
        [ Megaparsec.takeWhile1P Nothing (\c -> c /= ':' && c /= '\'' && c /= '"'),
          stringParser '\'',
          stringParser '"'
        ]
      where
        stringParser :: Char -> Megaparsec.Parsec Void Text Text
        stringParser ch = do
          _ <- Megaparsec.char ch
          ss <- many (Megaparsec.takeWhile1P Nothing (/= ch) <|> Megaparsec.string doubleQuote $> singleQuote)
          _ <- Megaparsec.char ch
          pure (quoted (Text.concat ss))
          where
            singleQuote :: Text
            singleQuote =
              Text.singleton ch
            doubleQuote :: Text
            doubleQuote =
              Text.pack [ch, ch]
            quoted :: Text -> Text
            quoted =
              Text.cons ch . flip Text.snoc ch . Text.replace singleQuote doubleQuote

    varSegmentParser :: Megaparsec.Parsec Void Text Text
    varSegmentParser = do
      _ <- Megaparsec.char ':'
      x <- Megaparsec.satisfy isLower
      xs <- Megaparsec.takeWhile1P Nothing (\c -> isLower c || isUpper c || isDigit c || c == '\'')
      pure (Text.cons x xs)

segmentsToSql :: [Segment] -> TH.Q TH.Exp
segmentsToSql segments =
  TH.varE 'mconcat `TH.appE` (TH.listE (map segmentToSql segments))

segmentToSql :: Segment -> TH.Q TH.Exp
segmentToSql = \case
  Segment'Raw s -> TH.conE 'Sql `TH.appE` TH.lift s `TH.appE` TH.varE 'mempty
  Segment'Var s -> varSegmentToSql s

-- TODO Maybe -> nullableColumnEncoder
varSegmentToSql :: Text -> TH.Q TH.Exp
varSegmentToSql (Text.unpack -> s) =
  TH.lookupValueName s >>= \case
    Nothing -> fail ("Unbound variable: " ++ s)
    Just name -> TH.conE 'Sql `TH.appE` TH.lift ("?" :: Text) `TH.appE` (TH.varE 'columnEncoder `TH.appE` TH.varE name)
