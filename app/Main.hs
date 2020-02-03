{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}



module Main where

import System.IO.Unsafe
import Control.Arrow
import Control.Comonad
import Control.Comonad.Store
import Control.Comonad.Traced hiding (Sum)
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
import qualified Data.ByteString as B

import Foreign.Ptr
import Foreign.C
import Foreign.Marshal.Array
import Data.Word
import Data.Hex
import qualified Data.Text as T

foreign import ccall "plus_ten" plusTen :: Int -> IO Int
foreign import ccall "parse_edid" parseEdid :: Ptr Word8 -> IO Int


-- import Mezzolens
-- import Mezzolens.Optics
-- import Mezzolens.Profunctor

main :: IO ()
main = someFunc

main1 :: B.ByteString -> IO ()
main1 s = do
    s' <- unhex s
    rs <- withArray (B.unpack s') return >>= parseEdid
    -- rs <- withArray (B.unpack s4k) return >>= parseEdid
    print rs



instance (StreamComonad w) => SequenceMonad (Co w) where
  seqm 0 a = Co $ \w -> (extract w) a
  seqm n a = Co $ \w -> ((runCo (seqm (n - 1) a)) . next) w

instance (ComonadStore s w) => MonadState s (Co w) where
  state f = Co $ \w -> case f (pos w) of
    (a, s) -> peek s w a

-- state = undefined

c1 :: String -> Component (Store Int)
c1 tag = store render 10
  where
    render s send = do
      print ("test " <> tag)
      print s
      r <- getLine
      case r of
        "add1" -> send (modify' (+ 1))
        "add10" -> send (modify' (+ 10))
        _ -> pure ()

ccc = combine (c1 "t0") $ combine (c1 "t1") (c1 "t2")
ccc1 = combine (c1 "t01") $ combine (c1 "t11") (c1 "t21")

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

exploreinit :: (Comonad w) => Co w () -> Component w -> IO ()
exploreinit m component = do
  state <- newIORef component
  modifyIORef state (\s -> (runCo m (extend const s)))
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

tt :: Traced String ()
tt = traced (\_ -> ())

s5 :: StoreT Int (Traced String) String
s5 = StoreT (TracedT (Identity (\_ -> (show)))) 0

d1 :: Day (Store Int) (Store String) String
d1 = Day (store id 11) (store id "one") ((<>) . (<> " ") . (" " <>) . show)
--convoluted = undefined


class Profunctor' p where
  dimap' ::
    forall a b c d.
    (b -> d) ->
    (c -> a) ->
    p a b ->
    p c d

instance Profunctor' (->) where
  dimap' f g h = f . h . g

class (Profunctor' p) => Choice p where
  _Left :: p a b -> p (Either a c) (Either b c)
  _Right :: p a b -> p (Either c a) (Either c b)

instance Choice (->) where
  _Left pab = either (Left . pab) Right
  _Right pab = either Left (Right . pab)

type Prism ta tb a b = forall p. (Choice p) => p a b -> p ta tb

-- prism generator
prism :: (ta -> (Either tb a)) -> (b -> tb) -> Prism ta tb a b
prism f m = (dimap' ( (|||) id m ) f) . _Right

-- Sum 

-- type S2 w a = Sum Identity (Day w (List w)) a
data List w a = List (Sum Identity (Day w (List w)) a) deriving (Functor)



instance (Comonad w) => Comonad (List w) where
  extract (List w) = extract w
  -- (w a -> b) -> w a -> w b
  extend f (List w) = List ( w =>> ( f . List) )


l1 :: List Identity String
l1 = List (Sum False (return ">") (Day (return "a") l2 (<>)))

l2 :: List Identity String
l2 = List (Sum False (return ">") (Day (return "b") nl (<>)))

l3 :: List Identity String
l3 = (List (Sum False (return ">") (Day (return "b") (ff (return "c") l1) (<>))))

ff w nl = (List (Sum False (return ">") (Day w nl (<>))))

nl :: List Identity String
nl = List (Sum True (return ">") (Day (return "1") nl  (<>) ))

aa :: List w a -> List w a
aa (List (Sum b i d@(Day w l@(List (Sum b' i' d'@(Day w' l' f' ))) f ))) = (List (Sum b i (Day w (List (Sum b' i' (Day w' l' f' ))) f )))


nextd :: (Comonad w) => List w  ~> List w
nextd  (List (Sum _ _ d)) = lowerDay1 d

lowerDay1 :: (Comonad w0, Functor w1) => Day w0 w1 ~> w1 
lowerDay1 (Day w0 w1 f) = fmap (f (extract w0)) w1

lowerDay0 :: (Functor w0, Comonad w1) => Day w0 w1 ~> w0
lowerDay0 (Day w0 w1 f) = fmap ( \ a0 -> f a0 (extract w1)) w0 

herel :: (Comonad w) => List w ~> w
herel (List (Sum _ _ d)) = lowerDay0 d

nextl :: (Comonad w) => List w ~> List w
nextl (List (Sum _ _ d)) = lowerDay1 d

push :: (Comonad w) => Co (List w) ()
push = Co go where
  go :: (Comonad w) => forall r. List w (() -> r) -> r
  go (List (Sum True _ f)) = (extract f) ()
  go l = do
      go (unsafePerformIO (do print ">>>> next"; return (nextl l)))

sliftLeft :: forall f g a. Co f a -> Co (Sum f g) a
sliftLeft x = Co $ \(Sum _ fa _) -> runCo x fa

-- | Lift an action to act on the right state.
sliftRight :: forall f g a. Co g a -> Co (Sum f g) a
sliftRight x = Co $ \(Sum _ _ ga) -> runCo x ga


comsum :: (Comonad w, Comonad w1) => Component w -> Component w1 -> Component (Sum w w1)
comsum a b =  (Sum True (fmap ( \ render send -> render (send . sliftLeft)  ) a) (fmap ( \ render send -> render (send . sliftRight)) b))


-- | Move to the right state.
moveRight :: Comonad g => Co (Sum f g) ()
moveRight = Co $ \ (Sum _ _ ga) -> extract ga (unsafePerformIO (do print "call mr"; return ()))

moveLeft :: Comonad f => Co (Sum f g) ()
moveLeft = Co $ \ (Sum _ ga _) -> extract ga ()

t5 = explore $ (comsum (c1 "t1") (c1 "t2")) 
      =>> ( \ wa -> ( \ h -> (extract wa) ( \ a -> h a )) )

gg :: (Comonad w) => Identity ((Co (List w) () -> IO ()) -> IO ())
gg = Identity ( \ send -> do
    print "gg"
    r <- getLine 
    case r of
      "push" -> send push
      _ -> pure ()
    pure ())

tt2 :: (Comonad w) => w (UI (Co w ())) -> (List w ~> List w) -> w (UI (Co (List w) ()))
tt2 c f = fmap (rr f) c 

rr :: (Comonad w) => (List w ~> List w) -> UI (Co w ()) -> UI (Co (List w) ())
rr f r = ( \ v -> (r (v . (liftWith (herel . f)) ))  )

  
liftWith :: (List w ~> w) -> Co w ~> Co (List w)
liftWith f x = Co $ \ l -> runCo x (f l)

listOf :: (Comonad w) => w (UI (Co w ())) -> (Component (List w))
listOf c = build c id where
  build :: (Comonad w) => w (UI (Co w ())) -> (List w ~> List w) -> (List w (((Co (List w) ()) -> IO ()) -> IO ()))
  build c f = (List (Sum True gg (Day (tt2 c f) (build c (nextl . f)) (<>) )))


listOfN :: (Comonad w) => Int -> w (UI (Co w ())) -> (Component (List w))
listOfN n c = build n c id where
  build :: (Comonad w) => Int -> w (UI (Co w ())) -> (List w ~> List w) -> (List w (((Co (List w) ()) -> IO ()) -> IO ()))
  build 0 c f = (List (Sum True gg (Day (tt2 c f) (build 0 c (nextl . f)) (<>) )))
  build n c f = (List (Sum False gg (Day (tt2 c f) (build (n - 1) c (nextl . f)) (<>) )))




  -- d :: (Comonad w1) => Day w1 (List w1) ((Co (List w1) () -> IO ()) -> IO ())
  -- d = 

  -- liftWith :: (List w ~> w) -> Co w ~> Co (List w)
  -- liftWith = undefined

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

s :: B.ByteString
s = "00ffffffffffff001a6e4100010101010011010280000078efe4c6a3594a9723124f54a38c7c314aa9408180d1c081c0010101010101023a801871382d4028408208c06c0000001ec02b408460b00c4010204400d33d0100001e413c80a070b0234030202600d33d0100001a000000fc0046532d4c32343031440a20202001de0203234146810203111204230907078301000067030c002100e87867d85dc4017880078c0ad08a20e02d10103e96000403000000188c0ad08a20e02d10103e96001009000000188c0ad090204031200c4055000403000000188c0ad090204031200c405500100900000018011d007251d01e206e28550010090000001e00002c"

s4k :: B.ByteString
s4k = "00ffffffffffff001a6e4100000000000e1c010480462578ee1779af5036b8260d4d51a54f808180a940d1c07140010101010101010108e80030f2705a80b0588a00ba712100001a023a801871382d40582c4500b9242000001a000000fc00464d2d443538303144560a2020000000fd001e4c1ea03c000a20202020202001cb020336f053101f0102030405060711121314151620615e5f23090707830100006c030c00210028782000400102e3050301e40f000001023a801871382d40582c450010090000001e023a80d072382d40102c458010090000001e8c0ad08a20e02d10103e96000403000000188c0ad08a20e02d10103e96001009000000180010"