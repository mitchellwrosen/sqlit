module Sqlit.Ap
  ( Ap (..),
    liftF,
  )
where

import Sqlit.Prelude

data Ap f a where
  Pure :: a -> Ap f a
  Ap :: f a -> Ap f (a -> b) -> Ap f b

instance Applicative (Ap f) where
  pure :: a -> Ap f a
  pure =
    Pure

  (<*>) :: Ap f (a -> b) -> Ap f a -> Ap f b
  f <*> x =
    case f of
      Pure g -> fmap g x
      Ap decoder g -> Ap decoder (flip <$> g <*> x)

instance Functor (Ap f) where
  fmap :: (a -> b) -> Ap f a -> Ap f b
  fmap f = \case
    Pure x -> Pure (f x)
    Ap mx mf -> Ap mx (fmap (f .) mf)

liftF :: f a -> Ap f a
liftF decoder =
  Ap decoder (Pure id)
