{-# LANGUAGE RankNTypes #-}

module Lib where


import Control.Comonad
import Control.Comonad.Identity
import Data.Maybe

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype Co w a = Co (forall r. w (a -> r) -> r)

runCo (Co cow) = cow

instance (Functor w) => Functor (Co w) where
  fmap f (Co cow) = Co (\w -> cow (fmap ( . f) w))

instance (Comonad w) => Applicative (Co w) where
  pure a = Co $ \ w -> extract w a 
  -- (<*>) (Co fab) (Co a) = Co $ \ w -> fab ( w =>> ( \ wb -> ( \ a' ->   ) ) )

instance (Comonad w) => Monad (Co w) where
  return = pure
  ma >>= f = undefined

-- 
  
  
