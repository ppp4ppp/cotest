{-#LANGUAGE MultiParamTypeClasses #-}

module Stream where

import Control.Comonad
import Data.Functor.Identity

data Stream a = Cons a (Stream a)


-- next :: StreamComonad w => w a -> w a
-- next = undefined

class (Comonad w) => StreamComonad w where
  stream :: a -> (a -> a) -> w a
  next :: w a -> w a


instance StreamComonad Stream where
  stream v f = unflds v f
  next (Cons _ s) = s

  

unflds ::
  a ->
  (a -> a) ->
  Stream a
unflds v f = Cons v (unflds (f v) f)

instance (Show a) => Show (Stream a) where
  show s = shown 5 s
    where
      shown 0 (Cons a _) = " :> " <> (show a)
      shown n (Cons a s) = " :> " <> (show a) <> (shown (n - 1) s)

instance Functor Stream where
  fmap f (Cons a s) = Cons (f a) (fmap f s)

instance Comonad Stream where

  extract (Cons a _) = a

  duplicate stream@(Cons a s) = Cons stream (duplicate s)

  extend f = fmap f . duplicate
