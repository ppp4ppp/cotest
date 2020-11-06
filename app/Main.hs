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
import Foreign.C.Types
import Data.Word
import Data.Hex
import Control.Monad

import qualified Data.Text as T

import Control.Concurrent.Async (race_, race, concurrently)
import Control.Concurrent (forkIO, threadDelay, ThreadId(..), killThread)



import  qualified         Data.ByteString as B

import qualified Foreign.Marshal.Array as M
import Foreign.Ptr
import Foreign.C
import Foreign.C.String
import Foreign.Storable
import qualified Foreign.Marshal.Array as M
import qualified Foreign.Marshal.Alloc as M
import Data.Word


type JpegBuffer = (Ptr CInt , Ptr Int , Ptr Int , Ptr Int , Ptr Word8 , Ptr Word8 , Ptr Word8)

foreign import ccall "mainloop" mainLoop :: Ptr CLong -> CString -> CInt -> Ptr CInt -> Ptr Int -> Ptr Int -> Ptr Int -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO Int
foreign import ccall "stope" stopPipe :: Ptr CLong -> IO Int



main = do 
  (idx, s0, s1, s2, d0, d1, d2) <- mallocnew 
  cip <- newCString (T.unpack "224.1.1.14")
  l <- M.malloc
  l2 <- M.malloc
  forkIO $ void $ mainLoop l cip 6973 idx s0 s1 s2 d0 d1 d2
  forkIO $ void $ mainLoop l2 cip 6973 idx s0 s1 s2 d0 d1 d2
  getLine
  stopPipe l
  getLine
  stopPipe l2
  forkIO $ void $ mainLoop l cip 6973 idx s0 s1 s2 d0 d1 d2
  forkIO $ void $ mainLoop l2 cip 6973 idx s0 s1 s2 d0 d1 d2
  getLine
  stopPipe l
  getLine
  stopPipe l2
  forkIO $ void $ mainLoop l cip 6973 idx s0 s1 s2 d0 d1 d2
  forkIO $ void $ mainLoop l2 cip 6973 idx s0 s1 s2 d0 d1 d2
  getLine
  stopPipe l
  getLine
  stopPipe l2
  
  
  pure ()


mallocnew :: IO JpegBuffer
mallocnew = do
  a <- M.malloc
  s0 <- M.malloc
  s1 <- M.malloc
  s2 <- M.malloc
  c <-  M.mallocArray 100000
  d <-  M.mallocArray 100000
  e <-  M.mallocArray 100000
  pure (a, s0, s1, s2, c, d, e)