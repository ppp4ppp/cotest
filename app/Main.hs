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
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Control.Comonad


data Stream a = Cons a (Stream a)

instance  (Show a ) => Show (Stream a) where
  show (Cons a1 (Cons a2 (Cons a3 _) )) = (show a1 ) ++ ":<" ++ (show a2 ) ++ ":<" ++ (show a3 )

instance Functor Stream where
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Comonad Stream where
  extract (Cons a _) = a
  duplicate stream@(Cons a as) = Cons stream (duplicate as)

streama :: Stream String
streama = Cons "a" streamb

streamb :: Stream String
streamb = Cons "b" streamc

streamc :: Stream String
streamc = Cons "c" streama

stream1 :: Stream String
stream1 = Cons "1" stream2

stream2 :: Stream String
stream2 = Cons "2" stream3

stream3 :: Stream String
stream3 = Cons "3" stream1

{-



-}

takeone :: Stream a -> Stream a
takeone (Cons a s) = s

main = do 
  print ""

{-

a - b - c



-}