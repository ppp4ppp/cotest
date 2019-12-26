
{-#LANGUAGE  FlexibleContexts #-}

module Main where

import Data.Functor.Identity
import Control.Comonad
import Control.Monad
import Data.IORef
import Lib
import Sequence
import Stream

import Control.Comonad.Store

main :: IO ()
main = someFunc

f :: Stream (Int -> IO ())
f =  unflds (\ i -> print i) ( . (+1))

instance (StreamComonad w) => SequenceMonad (Co w)  where
  seqm 0 a = Co $ \ w -> (extract w) a
  seqm n a = Co $ \ w -> ( (runCo (seqm (n - 1) a)) . next ) w

s3 :: Co Stream Int
s3 = seqm 10 1000

-- handler
-- component

-- type Handler a = a -> IO ()
-- type UI w a = w (Co w a)
type Component w a = w (((Co w ()) -> IO ()) -> IO ())

explore :: (Comonad w) => Component w a -> IO ()
explore component = do
  -- let send :: Co w () -> IO ()
     -- send action = 
  pure ()
