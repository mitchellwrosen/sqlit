{-# LANGUAGE ImportQualifiedPost #-}

module Sqlit.Table
  ( TableDecoder (..),
    makeTableDecoder,
    noRowsTableDecoder,
    oneRowTableDecoder,
    maybeRowTableDecoder,
    rowsHashSetTableDecoder,
    rowsListTableDecoder,
    rowsSeqTableDecoder,
    rowsSetTableDecoder,
    -- rowsVectorTableDecoder,
  )
where

import Control.Monad.Trans.Reader (ReaderT (..))
import Data.HashSet qualified as HashSet
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Database.SQLite3 qualified as Sqlite
import Sqlit.Prelude
import Sqlit.Row

newtype TableDecoder a = TableDecoder
  {unTableDecoder :: Sqlite.Statement -> IO a}
  deriving (Functor) via (ReaderT Sqlite.Statement IO)

noRowsTableDecoder :: TableDecoder ()
noRowsTableDecoder =
  TableDecoder \statement ->
    Sqlite.stepNoCB statement >>= \case
      Sqlite.Done -> pure ()
      Sqlite.Row -> error "too many rows"

oneRowTableDecoder :: FromRow a => TableDecoder a
oneRowTableDecoder =
  makeTableDecoder
    ( \case
        Nothing -> error "too few rows"
        Just row -> pure row
    )
    ( \case
        Nothing -> pure . Just
        Just _ -> error "too many rows"
    )
    Nothing

maybeRowTableDecoder :: FromRow a => TableDecoder (Maybe a)
maybeRowTableDecoder =
  makeTableDecoder
    pure
    ( \case
        Nothing -> pure . Just
        Just _ -> error "too many rows"
    )
    Nothing

rowsHashSetTableDecoder :: (Eq a, FromRow a, Hashable a) => TableDecoder (HashSet a)
rowsHashSetTableDecoder =
  makeTableDecoder pure (\rows row -> pure (HashSet.insert row rows)) HashSet.empty

rowsListTableDecoder :: FromRow a => TableDecoder [a]
rowsListTableDecoder =
  makeTableDecoder (pure . ($ [])) (\rows row -> pure (rows . (row :))) id

rowsSeqTableDecoder :: FromRow a => TableDecoder (Seq a)
rowsSeqTableDecoder =
  makeTableDecoder pure (\rows row -> pure (rows Seq.|> row)) Seq.empty

rowsSetTableDecoder :: (FromRow a, Ord a) => TableDecoder (Set a)
rowsSetTableDecoder =
  makeTableDecoder pure (\rows row -> pure (Set.insert row rows)) Set.empty

makeTableDecoder :: FromRow a => (s -> IO b) -> (s -> a -> IO s) -> s -> TableDecoder b
makeTableDecoder onDone onRow acc0 =
  TableDecoder \statement ->
    (`fix` acc0) \loop acc ->
      Sqlite.stepNoCB statement >>= \case
        Sqlite.Done -> onDone acc
        Sqlite.Row -> do
          row <- decodeRow statement
          acc1 <- onRow acc row
          loop $! acc1
