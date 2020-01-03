{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Arrow
import Control.Comonad
import Control.Comonad.Store
import Control.Monad.State
import Control.Monad

import Data.Either
import Data.Function ((&))
import Data.Functor.Day
import Data.Functor.Identity
import Data.IORef
import Data.Map
import Lib
import Sequence
import Stream
import Mezzolens
import Mezzolens.Optics
import Mezzolens.Profunctor

main :: IO ()
main = someFunc


instance (StreamComonad w) => SequenceMonad (Co w) where
  seqm 0 a = Co $ \w -> (extract w) a
  seqm n a = Co $ \w -> ((runCo (seqm (n - 1) a)) . next) w

instance (ComonadStore s w) => MonadState s (Co w) where
  state f = Co $ \ w -> case f (pos w) of
                          (a , s) -> peek s w a

  -- state = undefined

c1 :: Component (Store Int)
c1 = store render 10 where
    render s send = do
      print "test"
      print s
      getLine
      send (modify' (+1))



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

liftLeft :: (Comonad w1, Comonad w2) => Co w1 () -> Co (Day w1 w2) ()       -- w1 ( a -> r )
liftLeft (Co cow) = Co $ \ (Day wa wb f) -> cow ( wa =>> ( \ wf -> f (extract wf) (extract wb) ))

liftRight :: (Comonad w1, Comonad w2) => Co w2 () -> Co (Day w1 w2) ()
liftRight (Co cow) = Co $ \ (Day wa wb f) -> cow ( wb =>> ( \ wf -> f (extract wa) (extract wf) ))



class (Profunctor p) => Convoluted p where
  convoluted :: (Comonad f, Comonad g, Comonad w)
             => p (f a) (g b)
             -> p (Day f w a) (Day g w b)

instance Convoluted (->) where
  convoluted pab = undefined

s4 :: StoreT Int w ()
s4 = undefined

sp :: Store Int () -> Store Int ()
sp w = w =>> peeks (+1)

type Blur s t a b = forall p. Convoluted p  => p a b -> p s t

stored :: (Comonad w) => Blur (StoreT s w a) (StoreT t w a) (StoreT s Identity a) (StoreT t Identity a)
stored pab = dimap t2 t1 (convoluted pab) where
  t1 :: Day (StoreT t Identity) w a -> StoreT t w a
  t1 = undefined

  t2 :: StoreT s w a -> Day (StoreT s Identity) w a
  t2 = undefined
  






  --convoluted = undefined

{-
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
  dimap g h f = h . f . g


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


class (Profunctor p) => Strong p where
  _1 :: p a b -> p (a, c) (b, c)
  _2 :: p a b -> p (c, a) (c, b)

class Profunctor p => Choice p where
  _Left :: p a b -> p (Either a c) (Either b c)
  _Right :: p a b -> p (Either c a) (Either c b)

instance Strong (->) where
  _1 f = ( \ (a, c) -> (f a, c) )
  _2 f = fmap f 

instance Choice (->) where
  _Left f = either (Left . f) Right
  _Right f = either Left (Right . f)

type Optical p ta tb a b = p a b -> p ta tb

type Lens ta tb a b = forall p. Strong p => Optical p ta tb a b

type Lens' ta a = Lens ta ta a a

type Prism ta tb a b = forall p. Choice p => Optical p ta tb a b

type Prism' ta a = Prism ta ta a a

lens :: (ta -> a) -> (b -> ta -> tb) -> Lens ta tb a b  
lens getter setter = (dimap (getter &&& id) (uncurry setter)) . _1

-- _Head :: Prism' [a] a
-- (a -> a) -> ()

-- let headM :: [a] -> Either [a] a; headM [] = Left []; headM (x:_) = Right x

prism :: (ta -> Either tb a) -> (b -> tb) -> Prism ta tb a b
prism match build = dimap match (id ||| build ) . _Right
-}