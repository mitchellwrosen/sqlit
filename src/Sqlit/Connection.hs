{-# LANGUAGE ImportQualifiedPost #-}

module Sqlit.Connection
  ( Connection (..),
    withConnection,
  )
where

import Database.SQLite3 qualified as Sqlite
import Sqlit.Prelude
import System.IO.Unsafe (unsafePerformIO)
import System.Random.SplitMix (SMGen, initSMGen)
import System.Random.Stateful

-- | A thread-safe connection to a SQLite database.
data Connection
  = Connection (MVar Sqlite.Database) (IOGenM SMGen)

-- | Perform an IO action with a connection to the given SQLite database.
withConnection :: Text -> (Connection -> IO a) -> IO a
withConnection connstr action =
  bracket (Sqlite.open connstr) Sqlite.close \database -> do
    gen <- newIOGenM =<< splitGenM globalGen
    connectionVar <- newMVar database
    Sqlite.exec database "PRAGMA foreign_keys"
    action (Connection connectionVar gen)

globalGen :: AtomicGenM SMGen
globalGen =
  unsafePerformIO (initSMGen >>= newAtomicGenM)
{-# NOINLINE globalGen #-}
