{-# LANGUAGE ImportQualifiedPost #-}

module Sqlit.Row
  ( -- * Encoding
    ToRow (..),
    RowEncoder (..),
    columnEncoder,
    nullableColumnEncoder,
    bindRow,

    -- * Decoding
    FromRow (..),
    RowDecoder (..),
    columnDecoder,
    nullableColumnDecoder,
    decodeRow,
  )
where

import Database.SQLite3 qualified as Sqlite
import Sqlit.Ap
import Sqlit.Column
import Sqlit.Prelude
import Sqlit.Value

-- Encoding

class ToRow a where
  rowEncoder :: RowEncoder a

instance ToRow () where
  rowEncoder :: RowEncoder ()
  rowEncoder =
    RowEncoder \_ _ -> pure

newtype RowEncoder a = RowEncoder
  {unRowEncoder :: a -> Sqlite.Statement -> Sqlite.ParamIndex -> IO Sqlite.ParamIndex}

instance Contravariant RowEncoder where
  contramap :: (b -> a) -> RowEncoder a -> RowEncoder b
  contramap f (RowEncoder encode) =
    RowEncoder (encode . f)

instance Decidable RowEncoder where
  lose :: (a -> Void) -> RowEncoder a
  lose f =
    RowEncoder \x _ _ -> absurd (f x)

  choose :: forall a b c. (a -> Either b c) -> RowEncoder b -> RowEncoder c -> RowEncoder a
  choose f =
    coerce go
    where
      go ::
        (b -> Sqlite.Statement -> Sqlite.ParamIndex -> IO Sqlite.ParamIndex) ->
        (c -> Sqlite.Statement -> Sqlite.ParamIndex -> IO Sqlite.ParamIndex) ->
        (a -> Sqlite.Statement -> Sqlite.ParamIndex -> IO Sqlite.ParamIndex)
      go encode1 encode2 =
        either encode1 encode2 . f

instance Divisible RowEncoder where
  conquer :: RowEncoder a
  conquer =
    mempty

  divide :: forall a b c. (c -> (a, b)) -> RowEncoder a -> RowEncoder b -> RowEncoder c
  divide f =
    coerce go
    where
      go ::
        (a -> Sqlite.Statement -> Sqlite.ParamIndex -> IO Sqlite.ParamIndex) ->
        (b -> Sqlite.Statement -> Sqlite.ParamIndex -> IO Sqlite.ParamIndex) ->
        (c -> Sqlite.Statement -> Sqlite.ParamIndex -> IO Sqlite.ParamIndex)
      go encode1 encode2 c statement =
        encode1 a statement >=> encode2 b statement
        where
          (a, b) = f c

instance Monoid (RowEncoder a) where
  mempty :: RowEncoder a
  mempty =
    RowEncoder \_ _ -> pure

instance Semigroup (RowEncoder a) where
  (<>) :: RowEncoder a -> RowEncoder a -> RowEncoder a
  (<>) =
    coerce append
    where
      append ::
        (a -> Sqlite.Statement -> Sqlite.ParamIndex -> IO Sqlite.ParamIndex) ->
        (a -> Sqlite.Statement -> Sqlite.ParamIndex -> IO Sqlite.ParamIndex) ->
        (a -> Sqlite.Statement -> Sqlite.ParamIndex -> IO Sqlite.ParamIndex)
      append encode1 encode2 value statement =
        encode1 value statement >=> encode2 value statement

columnEncoder :: ToValue a => RowEncoder a
columnEncoder =
  RowEncoder \value statement index -> do
    bindValue statement index value
    pure (index + 1)

nullableColumnEncoder :: ToValue a => RowEncoder (Maybe a)
nullableColumnEncoder =
  RowEncoder \maybeValue statement index -> do
    case maybeValue of
      Nothing -> Sqlite.bindNull statement index
      Just value -> bindValue statement index value
    pure (index + 1)

bindRow :: ToRow a => a -> Sqlite.Statement -> IO ()
bindRow row statement =
  void (unRowEncoder rowEncoder row statement 1)

-- * Decoding

class FromRow a where
  rowDecoder :: RowDecoder a

instance FromRow () where
  rowDecoder :: RowDecoder ()
  rowDecoder =
    RowDecoder (Pure ())

newtype RowDecoder a = RowDecoder
  {unRowDecoder :: Ap ColumnDecoder a}
  deriving newtype (Applicative, Functor)

columnDecoder :: FromValue a => RowDecoder a
columnDecoder =
  RowDecoder (liftF NonNullableColumnDecoder)

nullableColumnDecoder :: FromValue a => RowDecoder (Maybe a)
nullableColumnDecoder =
  RowDecoder (liftF NullableColumnDecoder)

decodeRow :: forall a. FromRow a => Sqlite.Statement -> IO a
decodeRow =
  decodeRow_ (unRowDecoder (rowDecoder @a))

decodeRow_ :: Ap ColumnDecoder a -> Sqlite.Statement -> IO a
decodeRow_ decoder0 statement =
  loop 0 decoder0
  where
    -- TODO make tail-recursive
    loop :: Sqlite.ColumnIndex -> Ap ColumnDecoder a -> IO a
    loop index = \case
      Pure x -> pure x
      Ap decoder next ->
        (&)
          <$> decodeColumn decoder statement index
          <*> loop (index + 1) next
