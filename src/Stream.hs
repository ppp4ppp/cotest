module Stream where

import Control.Comonad

data Stream a = Cons a (Stream a)

class StreamComonad a where
  stream :: a -> Stream a

instance StreamComonad Int where
  stream v = unflds v (+ 1)

unflds ::
  (Show a) =>
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
