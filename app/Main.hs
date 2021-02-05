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

type S = Int -> String

streama :: Stream S
streama = Cons (const "a") streamb

streamb :: Stream S
streamb = Cons (const "b") streamc

streamc :: Stream S
streamc = Cons (const "c") streama

stream1 :: Stream S
stream1 = Cons (const "1") stream2

stream2 :: Stream S
stream2 = Cons (const "2") stream3

stream3 :: Stream S
stream3 = Cons (const "3") stream1


loop :: Stream S -> IO ()
loop s = do
  i <- getLine >>= (pure . read)
  f <- pure $ extract s
  print (f i)
  loop $ takeone s
  

{-



-}

takeone :: Stream a -> Stream a
takeone (Cons a s) = s

main = do 
  print ""

{-

a - b - c



-}