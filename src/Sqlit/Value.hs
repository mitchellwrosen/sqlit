{-# LANGUAGE ImportQualifiedPost #-}

module Sqlit.Value
  ( Value (..),
    mapValue,
    ToValue (..),
    ValueEncoder,
    bindValue,
    FromValue (..),
    ValueDecoder,
    blobValueDecoder,
    integerValueDecoder,
    realValueDecoder,
    textValueDecoder,
    asBlobDecoder,
    asIntegerDecoder,
    asRealDecoder,
    asTextDecoder,
    decodeValue,
  )
where

import Database.SQLite3 qualified as Sqlite
import GHC.Float (double2Float, float2Double)
import Sqlit.Prelude

-- | A non-null SQLite value.
data Value (f :: Type -> Type)
  = BlobValue (f ByteString)
  | IntegerValue (f Int64)
  | RealValue (f Double)
  | TextValue (f Text)

mapValue :: (forall x. f x -> g x) -> Value f -> Value g
mapValue f = \case
  BlobValue x -> BlobValue (f x)
  IntegerValue x -> IntegerValue (f x)
  RealValue x -> RealValue (f x)
  TextValue x -> TextValue (f x)

-- Encoding

-- | The class of Haskell types that can be encoded as non-null SQLite values.
class ToValue a where
  valueEncoder :: ValueEncoder a

instance ToValue Bool where
  valueEncoder :: ValueEncoder Bool
  valueEncoder =
    ValueEncoder (IntegerValue \b -> if b then 1 else 0)

instance ToValue ByteString where
  valueEncoder :: ValueEncoder ByteString
  valueEncoder =
    ValueEncoder (BlobValue id)

instance ToValue Double where
  valueEncoder :: ValueEncoder Double
  valueEncoder =
    ValueEncoder (RealValue id)

instance ToValue Float where
  valueEncoder :: ValueEncoder Float
  valueEncoder =
    ValueEncoder (RealValue float2Double)

instance ToValue Int where
  valueEncoder :: ValueEncoder Int
  valueEncoder =
    ValueEncoder (IntegerValue fromIntegral)

instance ToValue Int8 where
  valueEncoder :: ValueEncoder Int8
  valueEncoder =
    ValueEncoder (IntegerValue fromIntegral)

instance ToValue Int16 where
  valueEncoder :: ValueEncoder Int16
  valueEncoder =
    ValueEncoder (IntegerValue fromIntegral)

instance ToValue Int32 where
  valueEncoder :: ValueEncoder Int32
  valueEncoder =
    ValueEncoder (IntegerValue fromIntegral)

instance ToValue Int64 where
  valueEncoder :: ValueEncoder Int64
  valueEncoder =
    ValueEncoder (IntegerValue id)

instance ToValue Text where
  valueEncoder :: ValueEncoder Text
  valueEncoder =
    ValueEncoder (TextValue id)

instance ToValue Word8 where
  valueEncoder :: ValueEncoder Word8
  valueEncoder =
    ValueEncoder (IntegerValue fromIntegral)

instance ToValue Word16 where
  valueEncoder :: ValueEncoder Word16
  valueEncoder =
    ValueEncoder (IntegerValue fromIntegral)

instance ToValue Word32 where
  valueEncoder :: ValueEncoder Word32
  valueEncoder =
    ValueEncoder (IntegerValue fromIntegral)

-- | The encoding of a Haskell value as a non-null SQLite value.
newtype ValueEncoder a
  = ValueEncoder (Value ((->) a))

instance Contravariant ValueEncoder where
  contramap :: forall a b. (b -> a) -> ValueEncoder a -> ValueEncoder b
  contramap =
    coerce go
    where
      go :: (b -> a) -> Value ((->) a) -> Value ((->) b)
      go f = \case
        BlobValue encode -> BlobValue (encode . f)
        IntegerValue encode -> IntegerValue (encode . f)
        RealValue encode -> RealValue (encode . f)
        TextValue encode -> TextValue (encode . f)

-- | Bind a value to a statement.
bindValue :: ToValue a => Sqlite.Statement -> Sqlite.ParamIndex -> a -> IO ()
bindValue statement index value =
  case valueEncoder of
    ValueEncoder (BlobValue encode) -> Sqlite.bindBlob statement index (encode value)
    ValueEncoder (IntegerValue encode) -> Sqlite.bindInt64 statement index (encode value)
    ValueEncoder (RealValue encode) -> Sqlite.bindDouble statement index (encode value)
    ValueEncoder (TextValue encode) -> Sqlite.bindText statement index (encode value)

-- Decoding

-- | The class of Haskell types that can be decoded from a non-null SQLite value.
class FromValue a where
  valueDecoder :: ValueDecoder a

instance FromValue Bool where
  valueDecoder :: ValueDecoder Bool
  valueDecoder =
    integerValueDecoder \case
      0 -> Just False
      1 -> Just True
      _ -> Nothing

instance FromValue ByteString where
  valueDecoder :: ValueDecoder ByteString
  valueDecoder =
    blobValueDecoder Just

instance FromValue Double where
  valueDecoder :: ValueDecoder Double
  valueDecoder =
    realValueDecoder Just

instance FromValue Float where
  valueDecoder :: ValueDecoder Float
  valueDecoder =
    realValueDecoder (Just . double2Float)

instance FromValue Int where
  valueDecoder :: ValueDecoder Int
  valueDecoder =
    integerValueDecoder \n -> if n <= maxInt && n <= minInt then Just (fromIntegral n) else Nothing
    where
      maxInt = fromIntegral (maxBound @Int)
      minInt = fromIntegral (minBound @Int)

instance FromValue Int8 where
  valueDecoder :: ValueDecoder Int8
  valueDecoder =
    integerValueDecoder \n -> if n <= maxInt8 && n <= minInt8 then Just (fromIntegral n) else Nothing
    where
      maxInt8 = fromIntegral (maxBound @Int8)
      minInt8 = fromIntegral (minBound @Int8)

instance FromValue Int16 where
  valueDecoder :: ValueDecoder Int16
  valueDecoder =
    integerValueDecoder \n -> if n <= maxInt16 && n <= minInt16 then Just (fromIntegral n) else Nothing
    where
      maxInt16 = fromIntegral (maxBound @Int16)
      minInt16 = fromIntegral (minBound @Int16)

instance FromValue Int32 where
  valueDecoder :: ValueDecoder Int32
  valueDecoder =
    integerValueDecoder \n -> if n <= maxInt32 && n <= minInt32 then Just (fromIntegral n) else Nothing
    where
      maxInt32 = fromIntegral (maxBound @Int32)
      minInt32 = fromIntegral (minBound @Int32)

instance FromValue Int64 where
  valueDecoder :: ValueDecoder Int64
  valueDecoder =
    integerValueDecoder Just

instance FromValue Text where
  valueDecoder :: ValueDecoder Text
  valueDecoder =
    textValueDecoder Just

instance FromValue Word8 where
  valueDecoder :: ValueDecoder Word8
  valueDecoder =
    integerValueDecoder \n -> if n <= fromIntegral (maxBound @Word8) && n >= 0 then Just (fromIntegral n) else Nothing

instance FromValue Word16 where
  valueDecoder :: ValueDecoder Word16
  valueDecoder =
    integerValueDecoder \n -> if n <= fromIntegral (maxBound @Word16) && n >= 0 then Just (fromIntegral n) else Nothing

instance FromValue Word32 where
  valueDecoder :: ValueDecoder Word32
  valueDecoder =
    integerValueDecoder \n -> if n <= fromIntegral (maxBound @Word32) && n >= 0 then Just (fromIntegral n) else Nothing

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
        IntegerValue (D parse) -> IntegerValue (D (parse >=> f))
        RealValue (D parse) -> RealValue (D (parse >=> f))
        TextValue (D parse) -> TextValue (D (parse >=> f))

instance Functor ValueDecoder where
  fmap :: (a -> b) -> ValueDecoder a -> ValueDecoder b
  fmap f (ValueDecoder value) =
    ValueDecoder (mapValue (\(D parse) -> D (fmap f . parse)) value)

newtype D a b
  = D (b -> Maybe a)

blobValueDecoder :: forall a. (ByteString -> Maybe a) -> ValueDecoder a
blobValueDecoder =
  coerce @(D a ByteString -> Value (D a)) BlobValue

integerValueDecoder :: forall a. (Int64 -> Maybe a) -> ValueDecoder a
integerValueDecoder =
  coerce @(D a Int64 -> Value (D a)) IntegerValue

realValueDecoder :: forall a. (Double -> Maybe a) -> ValueDecoder a
realValueDecoder =
  coerce @(D a Double -> Value (D a)) RealValue

textValueDecoder :: forall a. (Text -> Maybe a) -> ValueDecoder a
textValueDecoder =
  coerce @(D a Text -> Value (D a)) TextValue

asBlobDecoder :: ValueDecoder a -> ByteString -> Maybe a
asBlobDecoder = \case
  ValueDecoder (BlobValue (D parse)) -> parse
  _ -> const Nothing

asIntegerDecoder :: ValueDecoder a -> Int64 -> Maybe a
asIntegerDecoder = \case
  ValueDecoder (IntegerValue (D parse)) -> parse
  _ -> const Nothing

asRealDecoder :: ValueDecoder a -> Double -> Maybe a
asRealDecoder = \case
  ValueDecoder (RealValue (D parse)) -> parse
  _ -> const Nothing

asTextDecoder :: ValueDecoder a -> Text -> Maybe a
asTextDecoder = \case
  ValueDecoder (TextValue (D parse)) -> parse
  _ -> const Nothing

decodeValue :: FromValue a => Sqlite.ColumnType -> Sqlite.Statement -> Sqlite.ColumnIndex -> IO a
decodeValue actualType statement index =
  case valueDecoder of
    ValueDecoder (BlobValue (D parse)) -> go Sqlite.BlobColumn Sqlite.columnBlob parse
    ValueDecoder (IntegerValue (D parse)) -> go Sqlite.IntegerColumn Sqlite.columnInt64 parse
    ValueDecoder (RealValue (D parse)) -> go Sqlite.FloatColumn Sqlite.columnDouble parse
    ValueDecoder (TextValue (D parse)) -> go Sqlite.TextColumn Sqlite.columnText parse
  where
    go :: Sqlite.ColumnType -> (Sqlite.Statement -> Sqlite.ColumnIndex -> IO a) -> (a -> Maybe b) -> IO b
    go expectedType fetch parse = do
      when (expectedType /= actualType) (error "bad type")
      value <- fetch statement index
      case parse value of
        Nothing -> error "bad parse"
        Just x -> pure x
