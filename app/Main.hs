{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Arrow
import Control.Comonad
import Control.Comonad.Store
import Control.Monad
import Data.Either
import Data.Functor.Day
import Data.Functor.Identity
import Data.IORef
import Data.Map
import Lib
import Sequence
import Stream

main :: IO ()
main = someFunc

instance (StreamComonad w) => SequenceMonad (Co w) where
  seqm 0 a = Co $ \w -> (extract w) a
  seqm n a = Co $ \w -> ((runCo (seqm (n - 1) a)) . next) w

-- handler
-- component

-- type Handler a = a -> IO ()
type UI a = (a -> IO ()) -> IO ()

type Component w = w (UI (Co w ()))

f :: Int -> Component Stream
f n = Cons (render n) (f (n + 1))
  where
    render :: Int -> (UI (Co Stream ()))
    render n send = do
      v <- getLine 
      case v of
        "a" -> do 
                print n
                send s3
        _ -> print n

s3 :: Co Stream ()
s3 = seqm 15 ()

explore :: (Comonad w) => Component w -> IO ()
explore component = do
  state <- newIORef component
  replicateM_ 3 $ do
    let send action = modifyIORef state (\s -> (runCo action (extend const s)))
    comp <- readIORef state
    extract comp send

combine ::
  (Comonad w1, Comonad w2) =>
  Component w1 ->
  Component w2 ->
  Component (Day w1 w2)
combine comp1 comp2 = Day comp1 comp2 build
  where
    -- build :: UI w1 -> UI w2 -> UI w
    build a b = (\h -> (a (h . liftLeft)) <> (b (h . liftRight)) )

liftLeft :: Co w1 () -> Co (Day w1 w2) ()
liftLeft (Co cow) =  Co $ \ (Day wa wb f) -> undefined

liftRight :: Co w2 () -> Co (Day w1 w2) ()
liftRight c = undefined


--------------------------------------------------------
class Contravar f where
  contrmap :: (b -> a) -> f a -> f b

newtype Predicate a = Predicate {getPredicate :: a -> Bool}

instance Contravar Predicate where
  contrmap f p = Predicate ((getPredicate p) . f)

-- liftLeft :: (Comonad w1, Functor w2) => w1 -> Day

isOdd :: Predicate Int
isOdd = Predicate odd

veryOdd :: Predicate Int
veryOdd = contrmap (`div` 4) isOdd

newtype Comporision a = Comporision (a -> a -> Ordering)

gc :: Comporision a -> (a -> a -> Ordering)
gc (Comporision f) = f

instance Contravar Comporision where
  contrmap g (Comporision comp) = Comporision $ comp `on` g

on :: (a -> a -> c) -> (b -> a) -> (b -> b -> c)
on f g = flip (flip (f . g) . g)

class Bifunctor f where
  bimap :: (a -> c) -> (b -> d) -> f a b -> f c d

instance Bifunctor Either where
  bimap f g = either (Left . f) (Right . g)

instance Bifunctor (,) where
  bimap = (***)

class Profunctor f where
  dimap :: (c -> a) -> (b -> d) -> f a b -> f c d

instance Profunctor (->) where
  dimap g f a = f . a . g

type Limits a = Limits' a a

data Limits' a b
  = Limits
      { step :: a -> (b, b),
        check :: a -> a -> Bool
      }

instance Profunctor Limits' where
  dimap g h (Limits s c) = Limits ((h *** h) . s . g) (c `on` g)

newtype Indexed i a b = Indexed {runIndexed :: i -> a -> b}

instance Profunctor (Indexed i) where
  dimap g h (Indexed f) = Indexed (dimap g h . f)

class Indexable i p where
  indexed :: p a b -> i -> a -> b

instance Indexable i (Indexed i) where
  indexed = runIndexed

instance Indexable i (->) where
  indexed = const

mapIndexable :: Indexable i p => p a b -> Map i a -> Map i b
mapIndexable = mapWithKey . indexed

