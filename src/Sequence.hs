module Sequence where

import Control.Monad

data Sequence a = End a | Next (Sequence a) deriving (Show)

msqs ::
  Int ->
  a ->
  Sequence a
msqs 0 a = End a
msqs n a = Next (msqs (n - 1) a)

instance Functor Sequence where
  fmap f (End a) = End (f a)
  fmap f (Next s) = fmap f s

instance Applicative Sequence where

  pure a = End a

  (<*>) (Next fa) (Next a) = fa <*> a
  (<*>) (End fa) (Next a) = (End fa) <*> a
  (<*>) (Next fa) (End a) = fa <*> (End a)
  (<*>) (End fa) (End a) = End (fa a)
