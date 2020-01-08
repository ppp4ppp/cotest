{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Arrow
import Control.Comonad
import Control.Comonad.Store
import Control.Comonad.Traced
import Control.Monad
import Control.Monad.State
import Data.Either
import Data.Function ((&))
import Data.Functor.Day
import Data.Functor.Identity
import Data.IORef
import Data.Map
import Lib
import Sequence
import Stream

-- import Mezzolens
-- import Mezzolens.Optics
-- import Mezzolens.Profunctor

main :: IO ()
main = someFunc

instance (StreamComonad w) => SequenceMonad (Co w) where
  seqm 0 a = Co $ \w -> (extract w) a
  seqm n a = Co $ \w -> ((runCo (seqm (n - 1) a)) . next) w

instance (ComonadStore s w) => MonadState s (Co w) where
  state f = Co $ \w -> case f (pos w) of
    (a, s) -> peek s w a

-- state = undefined

c1 :: Component (Store Int)
c1 = store render 10
  where
    render s send = do
      print "test"
      print s
      getLine
      send (modify' (+ 1))

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
    build a b = (\h -> (a (h . liftLeft)) <> (b (h . liftRight)))

liftLeft :: (Comonad w1, Comonad w2) => Co w1 () -> Co (Day w1 w2) () -- w1 ( a -> r )
liftLeft (Co cow) = Co $ \(Day wa wb f) -> cow (wa =>> (\wf -> f (extract wf) (extract wb)))

liftRight :: (Comonad w1, Comonad w2) => Co w2 () -> Co (Day w1 w2) ()
liftRight (Co cow) = Co $ \(Day wa wb f) -> cow (wb =>> (\wf -> f (extract wa) (extract wf)))

type (~>) f g = forall a. f a -> g a

data Natural f g = Natural (f ~> g)

class Profunctor p where
  dimap ::
    forall a b c d.
    (Functor a, Functor b, Functor c, Functor d) =>
    (b ~> d) ->
    (c ~> a) ->
    p a b ->
    p c d

unNatural :: forall f g. Natural f g -> f ~> g
unNatural (Natural f) = f

n1 :: Natural Maybe []
n1 = Natural wrap
  where
    wrap (Just a) = [a]
    wrap Nothing = []

h1 Nothing = []
h1 (Just a) = [a]

h2 [] = Nothing
h2 (x : _) = Just x

instance Profunctor Natural where
  dimap f g (Natural h) = Natural (f . h . g)

class (Profunctor p) => Convoluted p where
  convoluted ::
    forall f g w.
    (Comonad f, Comonad g, Comonad w) =>
    p f g ->
    p (Day f w) (Day g w)

instance Convoluted Natural where
  convoluted (Natural pfg) = Natural $ \(Day a b f) -> (Day (pfg a) b f)

s4 :: StoreT Int w ()
s4 = undefined

sp :: Store Int () -> Store Int ()
sp w = w =>> peeks (+ 1)

type Blur s t a b = forall p. Convoluted p => p a b -> p s t

stored :: (Comonad w) => Blur (StoreT s w) (StoreT t w) (Store s) (Store t)
stored pab = dimap t1 t2 (convoluted pab)
  where
    t1 :: (Functor w, Comonad w) => forall a t. Day (StoreT t Identity) w a -> StoreT t w a
    t1 (Day (StoreT (Identity g) s) w f) = StoreT ((\a s' -> f (g s') a) <$> w) s
    t2 :: StoreT s w a -> Day (StoreT s Identity) w a
    t2 (StoreT w_s_a s) = Day (store id s) w_s_a (\s' sa' -> sa' s')

today :: (Comonad w) => Blur (Day (Store s) w) (Day (Store s) w) (Store s) (Store s)
today pab = dimap t1 t2 (convoluted pab)
  where
    t1 :: (Functor w, Comonad w) => forall a s. Day (Store s) w a -> Day (Store s) w a
    t1 = id
    t2 :: (Functor w, Comonad w) => forall a s. Day (Store s) w a -> Day (Store s) w a
    t2 = id

-- fs

d2 :: Day (Store Int) (Day (Store Int) (Store String)) String
d2 = Day (store id 1) d1 ((<>) . show)

tomorrow' ::
  (Comonad w) =>
  Blur (Day Identity (Day (Store s) w)) (Day Identity (Day (Store s) w)) (Store s) (Store s)
tomorrow' pab = dimap t1 t2 (convoluted pab)
  where
    t1 ::
      (Functor w, Comonad w) =>
      forall a s.
      Day (Store s) (Day Identity w) a ->
      (Day Identity (Day (Store s) w)) a
    t1 (Day s (Day i w f_i_w) f_s_d) = undefined -- (Day i (Day s w ( \ i' -> (f_i_w  i') ) f_s_d)

    t2 ::
      (Functor w, Comonad w) =>
      forall a s.
      (Day Identity (Day (Store s) w)) a ->
      Day (Store s) (Day Identity w) a
    t2 = undefined

tomorrow ::
  (Comonad w) =>
  Blur (Day Identity (Day (Store s) w)) (Day Identity (Day (Store s) w)) (Day (Store s) w) (Day (Store s) w)
tomorrow pab = dimap t1 t2 (convoluted pab)
  where
    t1 ::
      (Functor w, Comonad w) =>
      forall a s.
      Day (Day (Store s) w) Identity a ->
      (Day Identity (Day (Store s) w)) a
    t1 (Day (Day s w f_s_w) i f_d_i) = (Day i (Day s w gather) ( \ i' (s', w') -> f_d_i (f_s_w s' w') i'))
    t2 ::
      (Functor w, Comonad w) =>
      forall a s.
      (Day Identity (Day (Store s) w)) a ->
      Day (Day (Store s) w) Identity a
    t2 (Day i (Day s w f_s_w)  f_i_d) = (Day (Day s w gather) i ( \ (s', w') i' -> f_i_d i' (f_s_w s' w') ))
    
nn :: Natural (Store Int) (Store Int)
nn = Natural (extend (peeks (+ 100)))

ns :: Natural (Store String) (Store String)
ns = Natural (extend (peeks (<> "-100")))


s4d :: Day (Store Int) (Store String) String
s4d = undefined

type W = Identity

s7 :: Day (Store String) (Day Identity W) Bool 
s7 = Day (store id "s1") (Day (Identity 11) (Identity 12.4 ) f_i_w) f_s_d

f_s_d :: String -> () -> Bool
f_s_d s _ = (length s) > 1

f_i_w :: Int -> Float -> ()
f_i_w a b = ()

gather :: a -> b -> (a, b)
gather a b = (a, b)

s78 :: Day (Store String) (Day Identity W) Bool 
    -> Day Identity (Day (Store String) W) Bool
s78 (Day s (Day i w fiw) fsd) = Day i (Day s w gather) ( \ i' (s', w') -> fsd s' (fiw i' w') )  -- (Day s (Day i w fiw) fsd)

s8 :: Day Identity (Day (Store Int) W) Int 
s8 = Day (Identity 11) (Day (store id 10)  (Identity 12) (+)) (+)

s8s :: Day Identity (Day (Store String) W) String 
s8s = Day (Identity " 1 ") (Day (store id " 2 ")  (Identity " 3 ") (<>)) (<>)


-- st :: Day (Store s) (Day Identity w) a ->
   --    (Day Identity (Day (Store s) w)) a

tt :: Traced String ()
tt = traced (\_ -> ())

s5 :: StoreT Int (Traced String) String
s5 = StoreT (TracedT (Identity (\_ -> (show)))) 0

d1 :: Day (Store Int) (Store String) String
d1 = Day (store id 11) (store id "one") ((<>) . (<> " ") . (" " <>) . show)
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
