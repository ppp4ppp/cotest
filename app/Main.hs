{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveTraversable #-}

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where

import System.IO.Unsafe
import Control.Arrow
import Control.Comonad
import Control.Monad.Cont
import Control.Comonad.Store
import Control.Comonad.Traced hiding (Sum)
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer hiding (Sum)
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
import Control.Comonad.Cofree

import Foreign.Ptr
import Foreign.C
import Foreign.Marshal.Array
import Data.Word
import Data.Hex
import qualified Data.Text as T

import Control.Comonad.Trans.Cofree


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

instance (Monoid s, ComonadTraced s w) => MonadWriter s (Co w) where
  tell s = Co $ \ w -> trace s w ()

--- li :: List (Store String) (((Co (List (Store String)) ()) -> String) -> String)
li = listOf' (c1 "test")

fftest :: (Co (List (Store String)) ()) -> String
fftest (Co l) = undefined -- extract 

lilili = runCo (push >> push) (extend const (runCo (push >> push) (extend const li)))




coi :: Co (Store String) ()
coi = modify' ( \ s ->  ( s <> "|coi|" <> (show (length s)) ))

i1io :: (Store String) (((Co (Store String) ()) -> String) -> String)
i1io = store render "[i1io]" where  
  render s send = s <> (send coi)

rrr = runCo coi (extend const (runCo coi (extend const (runCo coi (extend const i1io)))))

corrr :: (Co (Store String) () -> String) 
corrr f = ""

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

c1m :: String -> Component (Store (Maybe Int))
c1m tag = undefined
  {-
  store render (Just 10)
  where
    render s send = do
      print ("test " <> tag)
      print s
      r <- getLine
      case r of
        "add1" -> send (modify' (fmap (+ 1))  )
        "add10" -> send (modify' (fmap (+ 10)))
        _ -> pure ()
-}

ccc = combine (c1 "t0") $ combine (c1 "t1") (c1 "t2")
ccc1 = combine (c1 "t01") $ combine (c1 "t11") (c1 "t21")

s13 :: Store Int (() -> ())
s13 = store ( \ s -> id ) 10

cc2 :: Co (Store Int) ()
cc2 = do
  a <- get 
  case a of
    11 -> modify' (+1000)
    _ -> modify' id

lll :: Co w a -> Co (List w) ([a])
lll = undefined

ci :: Co Identity ()
ci = Co $ \ l -> extract l ()

cs :: Co (Store String) ()
cs = Co $ \ l -> extract l ()

cc2a :: Co (Store Int) Int
cc2a = get
  
dd2 :: Co (Day
            (StoreT Int Identity)
            (Day (StoreT Int Identity) (StoreT Int Identity))) ()
dd2 = do
  v1 <- liftLeft' cc2a
  v2 <- liftRight' (liftRight' cc2a)
  case v1 of
    11 -> liftLeft cc2
    _ -> liftLeft cc2

-- type Handler a = a -> IO ()
type UI a = (a -> IO ()) -> IO ()

--                 w ((Co w ()) -> IO () -> IO () )
type Component w = w (UI (Co w ()))

fs :: Int -> Component Stream
fs n = Cons (render n) (fs (n + 1))
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
combine comp1 comp2 = Day (comp1 =>> ( \ wa -> (10, (extract wa) ) ) ) comp2 build
  where
    -- build :: UI w1 -> UI w2 -> UI w
    build a b = (\h -> ((snd a) (h . liftLeft)) <> (b (h . liftRight)))

combineCo ::
  (Comonad w1, Comonad w2) =>
  (Component w1 -> Bool) ->
  Component w1 ->
  Component w2 ->
  Component (Day w1 w2)
combineCo a comp1 comp2 = Day (comp1 =>> ( \ wa -> ( extract (wa =>> a) , (extract wa)) ) )  comp2 build
  where
    -- build :: UI w1 -> UI w2 -> UI w
    build a b = (\h -> if (fst a) then ((snd a) (h . liftLeft)) else (b (h . liftRight)))


liftLeft :: (Comonad w1, Comonad w2) => Co w1 () -> Co (Day w1 w2) () -- w1 ( a -> r )
liftLeft (Co cow) = Co $ \(Day wa wb f) -> cow (wa =>> (\wf -> f (extract wf) (extract wb)))

liftLeft' :: (Comonad w1, Comonad w2) => Co w1 a -> Co (Day w1 w2) a -- w1 ( a -> r )
liftLeft' (Co cow) = Co $ \(Day wa wb f) -> cow (wa =>> (\wf -> f (extract wf) (extract wb)))

liftRight :: (Comonad w1, Comonad w2) => Co w2 () -> Co (Day w1 w2) ()
liftRight (Co cow) = Co $ \(Day wa wb f) -> cow (wb =>> (\wf -> f (extract wa) (extract wf)))

liftRight' :: (Comonad w1, Comonad w2) => Co w2 a -> Co (Day w1 w2) a
liftRight' (Co cow) = Co $ \(Day wa wb f) -> cow (wb =>> (\wf -> f (extract wa) (extract wf)))


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

--
data R1 a = R1 (Either a [R1 a]) deriving Show

data CoR w a = CoR (Sum w (List (CoR w)) a) deriving (Functor)

instance (Comonad w) => Comonad (CoR w) where
  extract (CoR w) = extract w
  -- (w a -> b) -> w a -> w b
  extend f (CoR w) = CoR ( w =>> ( f . CoR) )

r1 :: R1 String 
r1 = R1 $ Left "test"

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

lowerDayL1 :: (Comonad w, Monoid b) => Day w (List w) (b -> r) -> (List w) (b -> r)
lowerDayL1 (Day w0 w1@(List (Sum True i d)) f) = fmap (f (extract w0)) w1
  

seqCo :: (Comonad w, Monoid b) => Co w b -> Co (List w) b 
seqCo (Co cow) = Co ( \ (List (Sum b i d)) -> cow (lowerDayL0 d)) where
  lowerDayL0 :: (Comonad w, Monoid b) => Day w (List w) (b -> r) -> w (b -> r)
  lowerDayL0 (Day w0 w1@(List (Sum False i d)) f) = fmap ( \ a0 -> ( \ bb -> (f a0 (extract (d))) bb ) ) w0
  lowerDayL0 (Day w0 w1@(List (Sum True  i d)) f) = fmap ( \ a0 -> ( \ bb -> (f a0 (extract (d))) mempty ) ) w0 
  
nextCo :: (Comonad w, Monoid b) => Co (List w) b -> Co (List w) b
nextCo (Co cow) = Co $ \ l -> cow (nextl l)

herecor :: (Comonad w) => CoR w ~> w
herecor (CoR (Sum b w l)) = w

nextcor :: (Comonad w) => CoR w ~> CoR w
nextcor (CoR (Sum b w l)) = herel l

nextcorl1 :: (Comonad w) => CoR w ~> CoR w
nextcorl1 (CoR (Sum b w l)) = (herel <<< nextl) l


  -- lowerDayL0 (Day w0 w1 f) = fmap ( \ a0 -> f a0 (extract w1)) w0 
herel :: (Comonad w) => List w ~> w
herel (List (Sum _ _ d)) = lowerDay0 d

herell :: (Comonad w) => List w a -> w (Bool, a)
herell (List (Sum b _ d)) = ((,) b) <$> lowerDay0 d

nextl :: (Comonad w) => List w ~> List w
nextl (List (Sum _ _ d)) = lowerDay1 d

push :: (Comonad w) => Co (List w) ()
push = Co go where
  go :: (Comonad w) => forall r. List w (() -> r) -> r
  go (List (Sum True _ f)) = (extract f) ()
  go l = go (nextl l)

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

gg :: (Comonad w) => (Co (List w) ()) -> Identity ((Co (List w) () -> IO ()) -> IO ())
gg pp = Identity ( \ send -> do
    print "gg"
    r <- getLine 
    case r of
      "push" -> send pp
      _ -> pure ()
    pure ())

tt2 :: (Comonad w) => w (UI (Co w ())) -> (List w ~> List w) -> w (UI (Co (List w) ()))
tt2 c f = fmap (rr f) c 

rr :: (Comonad w) => (List w ~> List w) -> UI (Co w ()) -> UI (Co (List w) ())
rr f r = ( \ v -> (r (v . (liftWith (herel . f)) ))  )

  
liftWith :: (Comonad w) => (t w ~> w) -> Co w ~> Co (t w)
liftWith f (Co x) = (Co (\ l ->  (x . f) l))


liftWithll :: forall a r w. (Comonad w) => (List w ~> w) -> Co w ~> Co (List w)
liftWithll f x = (Co (\ l -> runCo x (f l))) 


mapWith :: forall w. (Comonad w) => Co w () -> Co (List w) ()
mapWith x = (Co wrap)  where
  -- wrap :: forall w r. (Comonad w) => List w (() -> r) -> r
  wrap l@(List (Sum True i d)) = runCo x (herel l)
  wrap l@(List (Sum False i d)) = runCo x (herel (List (Sum False i (d =>> ( \ wa -> extract (lowerDay1 wa ) ) )          )))
  
  -- (d =>> ( \ wa -> extract (lowerDay1 wa ) ) ) )


listOf :: (Comonad w) => Co (List w) () -> w (UI (Co w ())) -> (Component (List w))
listOf t1t c = build t1t c id where
  build :: (Comonad w) => Co (List w) () -> w (UI (Co w ())) -> (List w ~> List w) -> (List w (((Co (List w) ()) -> IO ()) -> IO ()))
  build t1t c f = (List (Sum True (gg t1t) (Day (tt2 c f) (build t1t c (nextl . f)) (<>) )))

listOf' :: (Comonad w) =>  w (UI (Co w ())) -> (Component (List w))
listOf' c = build c id where
  build :: (Comonad w) 
        =>  w (UI (Co w ())) 
        -> (List w ~> List w) 
        -> (List w (((Co (List w) ()) -> IO ()) -> IO ()))
  build c f = 
    (List 
      (Sum True (Identity (\ _ -> pure ()))
      (Day (fmap 
        ( \ h -> ( \ coe -> (h (coe . (liftWith (herel . f)))))) c ) 
        (build c (nextl . f)) (<>) )))

corOf :: (Comonad w) =>  w (UI (Co w ())) -> (Component (CoR w))
corOf c = build c id where
  build :: (Comonad w) 
        =>  w (UI (Co w ())) 
        -> (CoR w ~> CoR w) 
        -> (CoR w (((Co (CoR w) ()) -> IO ()) -> IO ()))
  build c f = 
    (CoR 
      (Sum True (cw c f) 
        (build2 c f)))
  build2 :: (Comonad w) 
        =>  w (UI (Co w ())) 
        -> (CoR w ~> CoR w) 
        -> (List (CoR w)) (((Co (CoR w) ()) -> IO ()) -> IO ())
  build2 c f = 
    (List 
      (Sum True 
        (Identity (\ _ -> pure ()))
        (dddd c f)))
  dddd :: (Comonad w) 
          => w (UI (Co w ())) 
          -> (CoR w ~> CoR w)
          -> Day (CoR w)  (List (CoR w)) ((Co (CoR w) () -> IO ()) -> IO ())
  dddd c f = Day (build c f) (build2 c f) (<>)

corcor = (corOf (c1 ""))

pushcor :: (Comonad w) => Co (CoR w) ()
pushcor = Co go where
  go :: (Comonad w) => forall r. CoR w (() -> r) -> r
  go (CoR (Sum True _ f)) = (extract f) ()
  go l = go (nextcor (unsafePerformIO (print "go 1" >> (pure l))))

pushcorl1 :: (Comonad w) => Co (CoR w) ()
pushcorl1 = Co go where
  go :: (Comonad w) => forall r. CoR w (() -> r) -> r
  go (CoR (Sum True _ f)) = (extract f) ()
  go l = go (nextcorl1 (unsafePerformIO (print "go 1" >> (pure l))))


cccc = (runCo (pushcorl1 >> pushcorl1 >> pushcorl1 >> pushcorl1   ) (extend const corcor))
cccc1 = (runCo (pure ()) (extend const corcor))

cw :: (Comonad w1) => w1 (UI (Co w1 ())) -> (CoR w1 ~> CoR w1) -> w1 ((Co (CoR w1) () -> IO ()) -> IO ())
cw c f = fmap 
          ( \ h -> ( \ coe -> (h (coe . (liftWith (herecor . f)))))) c 

cl :: w1 (UI (Co w1 ())) -> (CoR w1 ~> CoR w1) -> (List (CoR w1) ((Co (CoR w1) () -> IO ()) -> IO ())) 
cl c f = undefined


ii5 :: Identity ((((Co (CoR w) ()) -> IO ()) -> IO ()))
ii5 = undefined
-- corOf :: 

-- t1t = do 
  -- rs <- seqCo (get >>= ( \ x -> pure [x]))
 -- push 

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

ttt :: (Applicative m) => (a -> m b) -> [a] -> m ([b])
ttt f [x] = ( \ v -> [v]) <$> f x
ttt f (x:xs) =  (:)  <$> f x <*> (ttt f xs)

ss :: String -> IO ()
ss = print

ii :: Int -> IO ()
ii = print

data Con r a = Con ( (a -> r) -> r )
instance Functor (Con r) where
  fmap f (Con g) = Con ( \ h -> g (h . f) )

data ConA r a = ConA [( (a -> r) -> r )]
runCon (Con g) = g

-- data List w a = List (Sum Identity (Day w (List w)) a) deriving (Functor)
-- trav :: (Applicative f) => (a -> f b) -> Co (List w) a -> f (Co (List w) b)
-- trav f c = 

seqA :: Applicative f => [f a] -> f [a]
seqA [x] =  pure <$> x
seqA (x:xs) =  (:) <$> x <*> (seqA xs)

f1 ::(f b -> r) -> (List w) (f b -> r) 
f1 = undefined

aaa :: (Applicative f, Comonad w) => List w (b -> r) -> List w (f b -> r) 
aaa l@(List (Sum b i d)) = cc1 l

cc1 :: (Applicative f, Comonad w) => w (b -> r) -> w (f b -> r) 
cc1 = undefined

-- \ List w ( f b -> r ) -> r === f \ List w ( b -> r ) -> r

-- instance Foldable (Co (List w))

-- instance (Comonad w) => Traversable (Co (List w)) where
  -- sequenceA = seqCo

tra :: (Applicative f) => (a -> f b) -> [a] -> f [b]
tra f [x] = (  \ v -> [v] ) <$> f x
tra f (x:xs) = (:) <$> f x <*> (tra f xs)

f2 :: w (f b -> r) -> (List w (f b -> r))
f2 = undefined

f3 :: f (w (b -> r)) -> (w (f b -> r))
f3 = undefined

f4 :: (Applicative f) => f (List w (b -> r)) -> f (w (b -> r))
f4 = undefined

f5 :: f ( w ( a -> r) -> r ) -> ( f ( w ( b -> r) ) -> r )
f5 = undefined

-- type LCo w a = LCo (forall r. w (a -> r) -> r)
co :: forall w a. (forall r. w (a -> r) -> r) -> Co w a
co = Co

iii :: forall w a. (forall r. List w (a -> r) -> r) -> Co (List w) a
iii = co

-- fco :: (Applicative f) => f ((List w (b -> r)) -> r ) -> f (Co (List w) b)
-- fco = fmap Co

-- List w ( Maybe -> r ) -> r = Maybe (List w ( a -> r) -> r)
-- List Sum _ i d = (extract d)


-- Co (List w) (Maybe ())    ===    Maybe (Co (List w) ())
-- (List w ( Maybe () -> r) ) -> r === Maybe (List w ( () -> r)) -> r
-- List (Sum Ident (Day w (List w)))

class Functor g => Distributive g where
  -- | The dual of 'Data.Traversable.mapM'
  cotraverse :: Comonad w => (w a -> b) -> w (g a) -> g b

  -- | The dual of 'Data.Traversable.sequence'
  distribute :: Comonad w => w (g a) -> g (w a)

  cotraverse f = fmap f . distribute
  distribute = cotraverse id
  
instance Distributive Identity where
  cotraverse f = Identity . f . fmap runIdentity
  distribute = Identity . fmap runIdentity

instance Distributive ((->)e) where
  distribute w e = fmap ($e) w

instance Distributive (Store w) where
  distribute w = extract w =>> (fmap (\ v -> fmap extract v) (const w))

-- | Every 'Distributive' is a 'Functor'. This is a valid default definition.
fmapDefault :: Distributive g => (a -> b) -> g a -> g b
fmapDefault f = cotraverse (f . runIdentity) . Identity

ts :: Traced String (Store (Int, Int) Int)
ts = traced ( \ s -> (store ((+ (length s)) .  fst) (10,20)))

instance (Comonad f, Comonad g) => Distributive (Sum f g) where
  distribute w = extract w =>> (fmap (\ v -> fmap extract v) (const w))

grep :: (Comonad w) => Co (List w) [()]
grep = Co go where
  go :: (Comonad w) => forall r. List w ([()] -> r) -> r
  go (List (Sum True i _)) = (extract i) []
  go l = go ((nextl l) =>> ( \ wa -> ((extract wa ) . (<> [()]) ) ) )

grepa :: (Comonad w) => Co w [a] -> Co (List w) [a]
grepa (Co extwa) = Co go where
  -- go :: (Comonad w) => forall r. List w ([a] -> r) -> r
  go (List (Sum True i _)) = (extract i) []
  go l = go ((nextl l) =>> ( \ wa -> ( (extract wa) . (<> []) ) ) )

grepst :: (ComonadStore s w) => Co (List w) [s]
grepst = Co go where
  -- go :: (Comonad w) => forall r. List w ([a] -> r) -> r
  go (List (Sum True i _)) = (extract i) []
  go l = go ((nextl l) =>> ( \ wa -> ( (extract wa) . (<> [ pos (herel wa) ]) ) ) )

uplast :: (Comonad w) => Co w () -> Co (List w) ()
uplast c = do
  xs <- grep 
  case xs of
    [x] -> liftWith (herel . nextl) c
    _ -> pure ()

cft :: CofreeT ((->) Int) Identity [Int]
cft = coiterT ( \ (Identity xs) -> ( \ (i::Int) -> (Identity xs))) (pure [0,0])

cfts :: CofreeT ((->) Int) (Store Int) [Int]
cfts = coiterT ( \ (s::(Store Int [Int])) -> ( \ (i::Int) -> (s::(Store Int [Int])))) (store ( \ a -> [a]) 0)

pedalt :: CofreeT ((->) Int) (Store Int) [Int] -> IO ()
pedalt cfts = do
  v <- getLine >>= pure . read
  print v
  cfts' <- pure $ unwrap cfts  v 
  print (extract cfts')
  pedalt cfts'

copedalt :: Component (CofreeT ((->) Int) (Store Int))
copedalt = undefined

{-
cf :: Cofree ((->) Int) [Int]
cf = coiter f [0,0] where
  f :: [Int] -> Int -> [Int]
  f [1,1] _ = [0,0]
  f [0, b] 1 = [1, b]
  f [0, b] 2 = [0, 1]
  f xs _ = xs

pedal cf = do
  v <- getLine >>= pure . read
  print v
  cf' <- pure $ unwrap cf v
  print (extract cf')
  pedal cf'
-}

{-
hoistTransitionT :: v ~> w -> TransitionT w m ~> TransitionT v m
hoistTransitionT nt (TransitionT t) = TransitionT (t . nt)

-- | TODO: write documentation
liftUI ::
  (Comonad w,
  Functor m)
  => wp ~> w
  -> UI m w ~> UI m wp
liftUI lower ui = \send -> ui (send . hoistTransitionT lower)

-- | TODO: write documentation
liftUIT ::
  (Comonad w,
  Functor m)
  => ComonadTrans wp
  => UI m w ~> UI m (wp w)
liftUIT = liftUI lower

-- | TODO: write documentation
liftComponent ::
  (Comonad w
  , Functor m)
  => wp ~> w
  -> Component m w f a -> UI m wp (f a)
liftComponent lower = liftUI lower . extract
-}


storeTexample :: Component (StoreT String (Store Int) )
storeTexample = StoreT (fmap stateToProp (fmap coTo ( c1 "c1"))) "test"
-- storeTexample = StoreT (store  ( \ a b -> se' ) 0 ) "test"
  -- w a -> w (s -> a)

stateToProp :: ((( Co (StoreT String (Store Int)) ()) -> IO ()) -> IO ()) 
            -> (String -> ((( Co (StoreT String (Store Int)) ()) -> IO ()) -> IO ()))
stateToProp  a = const ( \ h -> a (h . gg1 ))

gg1 :: ( Co (StoreT String (Store Int)) ()) -> ( Co (StoreT String (Store Int)) ())
gg1 a = do 
    a
    v <- (fa get)
    case v == 11 of
      True -> (f (put 1000))
      False -> pure ()

coTo :: ((( Co ((Store Int)) ()) -> IO ()) -> IO ())
      -> (((( Co (StoreT String (Store Int)) ()) -> IO ()) -> IO ())) 
coTo ui = \ send -> ui (send . f) 

fa :: ( Co ((Store Int)) a) -> ( Co (StoreT String (Store Int)) a)
fa (Co cow) = Co $ \ w -> (cow (f' w))

f :: ( Co ((Store Int)) ()) -> ( Co (StoreT String (Store Int)) ())
f (Co cow) = Co $ \ w -> (cow (f' w))

f' :: StoreT String (Store Int) ~> (Store Int)
f' tt' = (\ f -> f (snd (runStoreT tt'))) <$> (fst (runStoreT tt'))

c2 :: Store Int ((( Co (StoreT String (Store Int)) ()) -> IO ()) -> IO ())
c2 = undefined


-- ui :: ((( Co (StoreT String (Store Int)) ()) -> IO ()) -> IO ())
-- ui = undefined

tt' :: StoreT String (Store Int) (IO ())
tt'  = StoreT (store ( \ i st -> print ((length st)) ) 12) "test"


