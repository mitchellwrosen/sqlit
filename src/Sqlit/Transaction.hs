{-# LANGUAGE ImportQualifiedPost #-}

module Sqlit.Transaction
  ( Transaction (..),
    runTransaction,
    dupableTransactionIO,
  )
where

import Control.Monad.Trans.Reader
import Database.SQLite3 qualified as Sqlite
import Sqlit.Prelude

newtype Transaction a
  = Transaction (Sqlite.Database -> IO a)
  deriving (Applicative, Functor, Monad) via (ReaderT Sqlite.Database IO)

-- TODO transactionTry
-- TODO MonadIO with type error

runTransaction :: Sqlite.Database -> Transaction a -> IO a
runTransaction db (Transaction transaction :: Transaction a) =
  uninterruptibleMask \restore ->
    fix \again -> do
      let retryIfBusy :: SomeException -> IO a
          retryIfBusy exception =
            case fromException exception of
              Just (Sqlite.SQLError Sqlite.ErrorBusy _ _) -> again
              _ -> throwIO exception
      restore (Sqlite.exec db "BEGIN")
      -- + If the action fails with some exception. No matter what kind of exception, we do want to attempt to roll back
      --   the transaction, and we do so with asynchronous exceptions masked.
      --
      --   If the rollback fails, we ignore the exception, because the SQLite documentation recommends this pattern:
      --
      --     It is recommended that applications respond to the errors listed above by explicitly issuing a ROLLBACK
      --     command. If the transaction has already been rolled back automatically by the error response, then the
      --     ROLLBACK command will fail with an error, but no harm is caused by this."
      --
      --   Then, retry if the action failed with SQLITE_BUSY.
      --
      -- + If the action succeeds, try committing, and retry if SQLITE_BUSY.
      try (restore (transaction db)) >>= \case
        Left actionException -> do
          _ <- try @IO @Sqlite.SQLError (Sqlite.exec db "ROLLBACK")
          retryIfBusy actionException
        Right result -> do
          try (restore (Sqlite.exec db "COMMIT")) >>= \case
            Left sqlError -> retryIfBusy sqlError
            Right () -> pure result

dupableTransactionIO :: IO a -> Transaction a
dupableTransactionIO action =
  Transaction \_ -> action
