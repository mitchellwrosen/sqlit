{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Sqlit.QQ
  ( exec,
    hashSet,
    list,
    Sqlit.QQ.maybe,
    only,
    Sqlit.QQ.seq,
    set,
    sqlQQ,
  )
where

import Database.SQLite3 qualified as Sqlite
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Quote qualified as TH
import Sqlit.Prelude
import Sqlit.Row (bindRow)
import Sqlit.Sql
import Sqlit.Table
import Sqlit.Transaction

-- | @
-- exec :: 'Sql' -> 'Transaction' ()
-- @
exec :: TH.QuasiQuoter
exec =
  sqlQQ 'noRowsTableDecoder

-- | @
-- only :: 'FromRow' a => 'Sql' -> 'Transaction' a
-- @
only :: TH.QuasiQuoter
only =
  sqlQQ 'oneRowTableDecoder

-- | @
-- maybe :: 'FromRow' a => 'Sql' -> 'Transaction' (Maybe a)
-- @
maybe :: TH.QuasiQuoter
maybe =
  sqlQQ 'maybeRowTableDecoder

-- | @
-- hashSet :: (Eq a, 'FromRow' a, Hashable a) => 'Sql' -> 'Transaction' (HashSet a)
-- @
hashSet :: TH.QuasiQuoter
hashSet =
  sqlQQ 'rowsHashSetTableDecoder

-- | @
-- list :: 'FromRow' a => 'Sql' -> 'Transaction' [a]
-- @
list :: TH.QuasiQuoter
list =
  sqlQQ 'rowsListTableDecoder

-- | @
-- seq :: 'FromRow' a => 'Sql' -> 'Transaction' (Seq a)
-- @
seq :: TH.QuasiQuoter
seq =
  sqlQQ 'rowsSeqTableDecoder

-- | @
-- set :: ('FromRow' a, Ord a) => 'Sql' -> 'Transaction' (Set a)
-- @
set :: TH.QuasiQuoter
set =
  sqlQQ 'rowsSetTableDecoder

sqlQQ :: TH.Name -> TH.QuasiQuoter
sqlQQ decoderName =
  TH.QuasiQuoter
    { TH.quoteExp = \string -> TH.varE 'query `TH.appE` TH.varE decoderName `TH.appE` stringToSqlQ string,
      TH.quoteDec = undefined,
      TH.quotePat = undefined,
      TH.quoteType = undefined
    }

query :: TableDecoder a -> Sql -> Transaction a
query (TableDecoder tableDecoder) (Sql string rowEncoder) =
  Transaction \database _gen -> do
    bracket (Sqlite.prepare database string) Sqlite.finalize \statement -> do
      bindRow rowEncoder statement
      tableDecoder statement
