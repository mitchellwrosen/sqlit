{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Sqlit
  ( withConnection,

    -- * Writing queries
    query,
    scalarQuery,
    sql,
    (:.) (..),
    (:?) (..),
    GroupConcat (..),

    -- * Transaction
    Transaction,
    runTransaction,
    transactionIO,

    -- * Table
    FromTable (..),
    TableDecoder,

    -- * Row

    -- ** Encoding
    ToRow (..),
    columnEncoder,
    nullableColumnEncoder,

    -- ** Decoding
    FromRow (..),
    columnDecoder,
    nullableColumnDecoder,

    -- * Value

    -- ** Encoding
    ToValue (..),
    ValueEncoder,

    -- ** Decoding
    FromValue (..),
    ValueDecoder,
  )
where

import Data.Text qualified as Text
import Data.Tuple.Only
import Database.SQLite3 qualified as Sqlite
import Language.Haskell.TH.Quote qualified as TH
import Sqlit.GroupConcat
import Sqlit.Prelude
import Sqlit.Row
import Sqlit.Table
import Sqlit.Transaction
import Sqlit.Value

withConnection :: Text -> (Sqlite.Database -> IO a) -> IO a
withConnection connstr action =
  bracket (Sqlite.open connstr) Sqlite.close \db -> do
    bracket (Sqlite.prepareUtf8 db "PRAGMA foreign_keys") Sqlite.finalize \_ -> pure ()
    action db

data a :. b
  = a :. b
  deriving stock (Eq, Ord, Show)

infixr 9 :.

instance (FromValue a, FromRow b) => FromRow (a :. b) where
  rowDecoder :: RowDecoder (a :. b)
  rowDecoder =
    (:.) <$> columnDecoder <*> rowDecoder

instance (ToValue a, ToRow b) => ToRow (a :. b) where
  rowEncoder :: RowEncoder (a :. b)
  rowEncoder =
    divide (\(a :. b) -> (a, b)) columnEncoder rowEncoder

data a :? b
  = Maybe a :? b
  deriving stock (Eq, Ord, Show)

infixr 9 :?

instance (FromValue a, FromRow b) => FromRow (a :? b) where
  rowDecoder :: RowDecoder (a :? b)
  rowDecoder =
    (:?) <$> nullableColumnDecoder <*> rowDecoder

instance (ToValue a, ToRow b) => ToRow (a :? b) where
  rowEncoder :: RowEncoder (a :? b)
  rowEncoder =
    divide (\(a :? b) -> (a, b)) nullableColumnEncoder rowEncoder

query :: (ToRow a, FromTable b) => Text -> a -> Transaction b
query string params =
  Transaction \db -> do
    bracket (Sqlite.prepare db string) Sqlite.finalize \statement -> do
      bindRow params statement
      unTableDecoder tableDecoder statement

scalarQuery :: (ToRow a, FromValue b) => Text -> a -> Transaction b
scalarQuery string params = do
  Only (value :. ()) <- query string params
  pure value

sql :: TH.QuasiQuoter
sql =
  TH.QuasiQuoter
    { TH.quoteExp = \string -> [|Text.unwords (Text.words (Text.pack string))|],
      TH.quoteDec = undefined,
      TH.quotePat = undefined,
      TH.quoteType = undefined
    }
