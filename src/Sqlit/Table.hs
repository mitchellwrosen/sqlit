{-# LANGUAGE ImportQualifiedPost #-}

module Sqlit.Table
  ( FromTable (..),
    TableDecoder (..),
    makeTableDecoder,
  )
where

import Data.HashSet qualified as HashSet
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Tuple.Only
import Database.SQLite3 qualified as Sqlite
import Sqlit.Prelude
import Sqlit.Row

class FromTable a where
  tableDecoder :: TableDecoder a

instance FromTable () where
  tableDecoder :: TableDecoder ()
  tableDecoder =
    TableDecoder \statement ->
      Sqlite.stepNoCB statement >>= \case
        Sqlite.Done -> pure ()
        Sqlite.Row -> error "too many rows"

instance (Eq a, FromRow a, Hashable a) => FromTable (HashSet a) where
  tableDecoder :: TableDecoder (HashSet a)
  tableDecoder =
    makeTableDecoder pure (\rows row -> pure (HashSet.insert row rows)) HashSet.empty

instance FromRow a => FromTable (Maybe a) where
  tableDecoder :: TableDecoder (Maybe a)
  tableDecoder =
    makeTableDecoder
      pure
      ( \case
          Nothing -> pure . Just
          Just _ -> error "too many rows"
      )
      Nothing

instance FromRow a => FromTable (Only a) where
  tableDecoder :: TableDecoder (Only a)
  tableDecoder =
    makeTableDecoder
      ( \case
          Nothing -> error "too few rows"
          Just row -> pure (Only row)
      )
      ( \case
          Nothing -> pure . Just
          Just _ -> error "too many rows"
      )
      Nothing

instance FromRow a => FromTable (Seq a) where
  tableDecoder :: TableDecoder (Seq a)
  tableDecoder =
    makeTableDecoder pure (\rows row -> pure (rows Seq.|> row)) Seq.empty

instance (FromRow a, Ord a) => FromTable (Set a) where
  tableDecoder :: TableDecoder (Set a)
  tableDecoder =
    makeTableDecoder pure (\rows row -> pure (Set.insert row rows)) Set.empty

data TableDecoder a = TableDecoder
  {unTableDecoder :: Sqlite.Statement -> IO a}

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
