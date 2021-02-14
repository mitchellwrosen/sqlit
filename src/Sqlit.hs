module Sqlit
  ( Connection,
    withConnection,

    -- * Writing queries
    exec,
    only,
    Sqlit.QQ.maybe,
    list,
    Sqlit.QQ.seq,
    set,
    hashSet,
    (:.) (..),
    (:?) (..),
    GroupConcat (..),

    -- * Transaction
    Transaction,
    runTransaction,
    dupableTransactionIO,

    -- * Table

    -- TableDecoder,

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
import Sqlit.QQ
import Sqlit.Row
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
