{-# LANGUAGE ImportQualifiedPost #-}

module Sqlit.Transaction
  ( Transaction (..),
    runTransaction,
    transactionTry,
    dupableTransactionIO,
  )
where

import Control.Monad.Trans.Reader
import Database.SQLite3 qualified as Sqlite
import Sqlit.Connection
import Sqlit.Prelude

-- | A SQLite transaction.
newtype Transaction a
  = Transaction (Sqlite.Database -> IO a)
  -- TODO drop transformers dep, just write these instances
  deriving (Applicative, Functor, Monad) via (ReaderT Sqlite.Database IO)

-- TODO MonadIO with type error

-- | Run a transaction, retrying if any statement fails with @SQLITE_BUSY@.
runTransaction :: Connection -> Transaction a -> IO a
runTransaction (Connection connectionVar) (Transaction transaction :: Transaction a) =
  uninterruptibleMask \restore ->
    withMVar connectionVar \database ->
      fix \again -> do
        let retryIfBusy :: SomeException -> IO a
            retryIfBusy exception =
              case fromException exception of
                Just (Sqlite.SQLError Sqlite.ErrorBusy _ _) -> again
                _ -> throwIO exception
        restore (Sqlite.exec database "BEGIN")
        -- + If the action fails with some exception. No matter what kind of exception, we do want to attempt to roll
        --   back the transaction, and we do so with asynchronous exceptions masked.
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
        try (restore (transaction database)) >>= \case
          Left actionException -> do
            _ <- try @IO @Sqlite.SQLError (Sqlite.exec database "ROLLBACK")
            retryIfBusy actionException
          Right result -> do
            try (restore (Sqlite.exec database "COMMIT")) >>= \case
              Left sqlError -> retryIfBusy sqlError
              Right () -> pure result

transactionTry :: Transaction a -> Transaction (Either Sqlite.SQLError a)
transactionTry (Transaction transaction) =
  Transaction \database -> try (transaction database)

-- | Perform an IO action within a transaction. The action may be duplicated if the transaction retries.
dupableTransactionIO :: IO a -> Transaction a
dupableTransactionIO action =
  Transaction \_ -> action
