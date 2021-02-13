{-# LANGUAGE ImportQualifiedPost #-}

module Sqlit.Value
  ( -- * Value
    Value (..),

    -- * Encoding
    ToValue (..),
    ValueEncoder (..),
    bindValue,

    -- * Decoding
    FromValue (..),
    ValueDecoder (..),
    D (..),
    decodeValue,
  )
where

import Database.SQLite3 qualified as Sqlite
import Sqlit.Prelude

data Value (f :: Type -> Type)
  = BlobValue (f ByteString)
  | FloatValue (f Double)
  | IntegerValue (f Int64)
  | TextValue (f Text)

mapValue :: (forall x. f x -> g x) -> Value f -> Value g
mapValue f = \case
  BlobValue x -> BlobValue (f x)
  FloatValue x -> FloatValue (f x)
  IntegerValue x -> IntegerValue (f x)
  TextValue x -> TextValue (f x)

-- Encoding

class ToValue a where
  valueEncoder :: ValueEncoder a

instance ToValue ByteString where
  valueEncoder :: ValueEncoder ByteString
  valueEncoder =
    ValueEncoder (BlobValue id)

instance ToValue Double where
  valueEncoder :: ValueEncoder Double
  valueEncoder =
    ValueEncoder (FloatValue id)

instance ToValue Int64 where
  valueEncoder :: ValueEncoder Int64
  valueEncoder =
    ValueEncoder (IntegerValue id)

instance ToValue Text where
  valueEncoder :: ValueEncoder Text
  valueEncoder =
    ValueEncoder (TextValue id)

newtype ValueEncoder a
  = ValueEncoder (Value ((->) a))

bindValue :: ToValue a => Sqlite.Statement -> Sqlite.ParamIndex -> a -> IO ()
bindValue statement index value =
  case valueEncoder of
    ValueEncoder (BlobValue encode) -> Sqlite.bindBlob statement index (encode value)
    ValueEncoder (FloatValue encode) -> Sqlite.bindDouble statement index (encode value)
    ValueEncoder (IntegerValue encode) -> Sqlite.bindInt64 statement index (encode value)
    ValueEncoder (TextValue encode) -> Sqlite.bindText statement index (encode value)

-- Decoding

class FromValue a where
  valueDecoder :: ValueDecoder a

instance FromValue ByteString where
  valueDecoder :: ValueDecoder ByteString
  valueDecoder =
    ValueDecoder (BlobValue (D Just))

instance FromValue Double where
  valueDecoder :: ValueDecoder Double
  valueDecoder =
    ValueDecoder (FloatValue (D Just))

instance FromValue Int64 where
  valueDecoder :: ValueDecoder Int64
  valueDecoder =
    ValueDecoder (IntegerValue (D Just))

instance FromValue Text where
  valueDecoder :: ValueDecoder Text
  valueDecoder =
    ValueDecoder (TextValue (D Just))

newtype ValueDecoder a
  = ValueDecoder (Value (D a))

instance Filterable ValueDecoder where
  mapMaybe :: forall a b. (a -> Maybe b) -> ValueDecoder a -> ValueDecoder b
  mapMaybe =
    coerce g
    where
      g :: (a -> Maybe b) -> Value (D a) -> Value (D b)
      g f = \case
        BlobValue (D parse) -> BlobValue (D (parse >=> f))
        FloatValue (D parse) -> FloatValue (D (parse >=> f))
        IntegerValue (D parse) -> IntegerValue (D (parse >=> f))
        TextValue (D parse) -> TextValue (D (parse >=> f))

instance Functor ValueDecoder where
  fmap :: (a -> b) -> ValueDecoder a -> ValueDecoder b
  fmap f (ValueDecoder value) =
    ValueDecoder (mapValue (\(D parse) -> D (fmap f . parse)) value)

newtype D a b
  = D (b -> Maybe a)

decodeValue :: FromValue a => Sqlite.ColumnType -> Sqlite.Statement -> Sqlite.ColumnIndex -> IO a
decodeValue actualType statement index =
  case valueDecoder of
    ValueDecoder (BlobValue (D parse)) -> go Sqlite.BlobColumn Sqlite.columnBlob parse
    ValueDecoder (FloatValue (D parse)) -> go Sqlite.FloatColumn Sqlite.columnDouble parse
    ValueDecoder (IntegerValue (D parse)) -> go Sqlite.IntegerColumn Sqlite.columnInt64 parse
    ValueDecoder (TextValue (D parse)) -> go Sqlite.TextColumn Sqlite.columnText parse
  where
    go :: Sqlite.ColumnType -> (Sqlite.Statement -> Sqlite.ColumnIndex -> IO a) -> (a -> Maybe b) -> IO b
    go expectedType fetch parse = do
      when (expectedType /= actualType) (error "bad type")
      value <- fetch statement index
      case parse value of
        Nothing -> error "bad parse"
        Just x -> pure x
