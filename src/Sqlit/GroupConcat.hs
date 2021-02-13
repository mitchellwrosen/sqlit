{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MagicHash #-}

module Sqlit.GroupConcat
  ( GroupConcat (..),
  )
where

import Data.Text qualified as Text
import GHC.Exts (proxy#)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal')
import Sqlit.Prelude
import Sqlit.Value

newtype GroupConcat (s :: Symbol) a = GroupConcat
  {unGroupConcat :: [a]}

instance (FromValue a, KnownSymbol s) => FromValue (GroupConcat s a) where
  valueDecoder :: ValueDecoder (GroupConcat s a)
  valueDecoder =
    mapMaybe f valueDecoder
    where
      f :: Text -> Maybe (GroupConcat s a)
      f =
        case valueDecoder @a of
          ValueDecoder (TextValue (D parse)) ->
            coerce
              @(Text -> Maybe [a])
              (traverse parse . Text.splitOn (Text.pack (symbolVal' (proxy# @s))))
          _ -> const Nothing
