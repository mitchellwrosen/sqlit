{-# LANGUAGE ImportQualifiedPost #-}

module Sqlit.Row
  ( FromRow (..),
    RowEncoder,
    columnEncoder,
    nullableColumnEncoder,
    bindRow,
    RowDecoder,
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

newtype RowEncoder
  = RowEncoder (Sqlite.Statement -> Sqlite.ParamIndex -> IO Sqlite.ParamIndex)

instance Monoid RowEncoder where
  mempty :: RowEncoder
  mempty =
    RowEncoder \_ -> pure

instance Semigroup RowEncoder where
  (<>) :: RowEncoder -> RowEncoder -> RowEncoder
  (<>) =
    coerce append
    where
      append ::
        (Sqlite.Statement -> Sqlite.ParamIndex -> IO Sqlite.ParamIndex) ->
        (Sqlite.Statement -> Sqlite.ParamIndex -> IO Sqlite.ParamIndex) ->
        (Sqlite.Statement -> Sqlite.ParamIndex -> IO Sqlite.ParamIndex)
      append encode1 encode2 statement =
        encode1 statement >=> encode2 statement

columnEncoder :: ToValue a => a -> RowEncoder
columnEncoder value =
  RowEncoder \statement index -> do
    bindValue statement index value
    pure (index + 1)

nullableColumnEncoder :: ToValue a => Maybe a -> RowEncoder
nullableColumnEncoder maybeValue =
  RowEncoder \statement index -> do
    case maybeValue of
      Nothing -> Sqlite.bindNull statement index
      Just value -> bindValue statement index value
    pure (index + 1)

bindRow :: RowEncoder -> Sqlite.Statement -> IO ()
bindRow (RowEncoder rowEncoder) statement =
  void (rowEncoder statement 1)

-- * Decoding

-- TODO GFromRow

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
