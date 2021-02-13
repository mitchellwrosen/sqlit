{-# LANGUAGE ImportQualifiedPost #-}

module Sqlit.Column
  ( -- * Decoding
    ColumnDecoder (..),
    decodeColumn,
  )
where

import Database.SQLite3 qualified as Sqlite
import Sqlit.Prelude
import Sqlit.Value

-- Decoding

data ColumnDecoder a where
  NonNullableColumnDecoder :: FromValue a => ColumnDecoder a
  NullableColumnDecoder :: FromValue a => ColumnDecoder (Maybe a)

decodeColumn :: ColumnDecoder a -> Sqlite.Statement -> Sqlite.ColumnIndex -> IO a
decodeColumn decoder statement index = do
  columnType <- Sqlite.columnType statement index
  case decoder of
    NonNullableColumnDecoder -> decodeValue columnType statement index
    NullableColumnDecoder ->
      case columnType of
        Sqlite.NullColumn -> pure Nothing
        _ -> Just <$> decodeValue columnType statement index
