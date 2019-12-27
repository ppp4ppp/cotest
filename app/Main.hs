{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Comonad
import Control.Comonad.Store
import Control.Monad
import Data.Functor.Day
import Data.Functor.Identity
import Data.IORef
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
      send s3
      print n

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
combine comp1 comp2 = Day comp1 comp2 build where
  -- build :: UI w1 -> UI w2 -> UI w
  build a b = ( \ h -> (a ( h . liftLeft ) ) )

liftLeft :: Co w1 () -> Co (Day w1 w2) ()
liftLeft c = undefined

-- liftLeft :: (Comonad w1, Functor w2) => w1 -> Day 
