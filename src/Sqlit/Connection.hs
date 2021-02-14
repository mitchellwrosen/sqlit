{-# LANGUAGE ImportQualifiedPost #-}

module Sqlit.Connection
  ( Connection (..),
    withConnection,
  )
where

import Database.SQLite3 qualified as Sqlite
import Sqlit.Prelude

-- | A thread-safe connection to a SQLite database.
newtype Connection
  = Connection (MVar Sqlite.Database)

-- | Perform an IO action with a connection to the given SQLite database.
withConnection :: Text -> (Connection -> IO a) -> IO a
withConnection connstr action =
  bracket (Sqlite.open connstr) Sqlite.close \database -> do
    connectionVar <- newMVar database
    Sqlite.exec database "PRAGMA foreign_keys"
    action (Connection connectionVar)
