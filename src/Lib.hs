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
  fmap f (Co cow) = Co (\w -> cow (fmap ( \ a -> a . f ) w))

instance (Comonad w) => Applicative (Co w) where
  pure a = Co $ \ w -> extract w a 
  (<*>) (Co fab) (Co a) = Co $ \ w -> fab (w =>> ( \ wb -> ( \ ab -> a (fmap ( \ b' -> (b' . ab) ) wb)  ) ) )  
                                      
instance (Comonad w) => Monad (Co w) where
  return = pure
  (Co ma) >>= f = Co $ \ w -> ma ( w =>> ( \ wb -> \ a -> runCo (f a) wb ) )
  
  
data Sum f g a = Sum Bool (f a) (g a)

sum1 (Sum _ f _) = f
sum2 (Sum _ _ f) = f
mv (Sum b f g) = (Sum (not b) f g)

s1 = Sum True (Identity 1) (Identity 2) 


instance (Functor f, Functor g) => Functor (Sum f g) where
  fmap f (Sum b h k) = (Sum b (fmap f h) (fmap f k))

instance (Comonad w1, Comonad w2) => Comonad (Sum w1 w2) where
  extract (Sum True a _) = extract a
  extract (Sum False _ b) = extract b
  extend f (Sum b fa ga) = Sum b (extend (f . ( flip (Sum b) ga) ) fa) (extend (f . ( (Sum b) fa) ) ga)

