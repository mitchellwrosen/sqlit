module Sqlit
  ( Connection,
    withConnection,

    -- * Writing queries
    Sql,
    sql,
    (:.) (..),
    (:?) (..),
    GroupConcat (..),

    -- * Transaction
    Transaction,
    runTransaction,
    dupableTransactionIO,

    -- * Table
    FromTable (..),
    TableDecoder,

    -- * Row

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

import Sqlit.Connection
import Sqlit.GroupConcat
import Sqlit.Prelude
import Sqlit.Row
import Sqlit.Sql
import Sqlit.Table
import Sqlit.Transaction
import Sqlit.Value

data a :. b
  = a :. b
  deriving stock (Eq, Ord, Show)

infixr 9 :.

instance (FromValue a, FromRow b) => FromRow (a :. b) where
  rowDecoder :: RowDecoder (a :. b)
  rowDecoder =
    (:.) <$> columnDecoder <*> rowDecoder

data a :? b
  = Maybe a :? b
  deriving stock (Eq, Ord, Show)

infixr 9 :?

instance (FromValue a, FromRow b) => FromRow (a :? b) where
  rowDecoder :: RowDecoder (a :? b)
  rowDecoder =
    (:?) <$> nullableColumnDecoder <*> rowDecoder
