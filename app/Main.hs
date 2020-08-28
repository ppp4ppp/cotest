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

import Foreign.Ptr
import Foreign.C
import Data.Word
import Data.Hex

import  qualified         Data.ByteString as B

import qualified Foreign.Marshal.Array as M

foreign import ccall "parse_refresh_rate" parseRefreshRate :: Ptr Word8 -> IO Int


main = do 
  p <- M.mallocArray 100 
  M.pokeArray p (B.unpack "11111111")
  f p
  f p
  f p

f p = do
  parseRefreshRate p
  rs <- M.peekArray 5 p
  print rs
  pure ()
