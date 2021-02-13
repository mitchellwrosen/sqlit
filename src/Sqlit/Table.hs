{-# LANGUAGE ImportQualifiedPost #-}

module Sqlit.Table
  ( FromTable (..),
    TableDecoder (..),
  )
where

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

instance FromRow a => FromTable (Maybe a) where
  tableDecoder :: TableDecoder (Maybe a)
  tableDecoder =
    TableDecoder \statement ->
      (`fix` Nothing) \loop maybeRow ->
        Sqlite.stepNoCB statement >>= \case
          Sqlite.Done -> pure maybeRow
          Sqlite.Row ->
            case maybeRow of
              Nothing -> do
                row <- decodeRow statement
                loop (Just row)
              Just _ -> error "too many rows"

instance FromRow a => FromTable (Only a) where
  tableDecoder :: TableDecoder (Only a)
  tableDecoder =
    TableDecoder \statement ->
      (`fix` Nothing) \loop maybeRow ->
        Sqlite.stepNoCB statement >>= \case
          Sqlite.Done ->
            case maybeRow of
              Nothing -> error "too few rows"
              Just row -> pure (Only row)
          Sqlite.Row ->
            case maybeRow of
              Nothing -> do
                row <- decodeRow statement
                loop (Just row)
              Just _ -> error "too many rows"

instance FromRow a => FromTable (Seq a) where
  tableDecoder :: TableDecoder (Seq a)
  tableDecoder =
    TableDecoder \statement ->
      (`fix` Seq.empty) \loop rows ->
        Sqlite.stepNoCB statement >>= \case
          Sqlite.Done -> pure rows
          Sqlite.Row -> do
            row <- decodeRow statement
            loop $! rows Seq.|> row

instance (FromRow a, Ord a) => FromTable (Set a) where
  tableDecoder :: TableDecoder (Set a)
  tableDecoder =
    TableDecoder \statement ->
      (`fix` Set.empty) \loop rows ->
        Sqlite.stepNoCB statement >>= \case
          Sqlite.Done -> pure rows
          Sqlite.Row -> do
            row <- decodeRow statement
            loop $! Set.insert row rows

data TableDecoder a = TableDecoder
  {unTableDecoder :: Sqlite.Statement -> IO a}
