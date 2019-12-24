module Main where

import Control.Comonad
import Control.Monad
import Lib
import Sequence
import Stream

main :: IO ()
main = someFunc

-- handler
-- component

-- type Handler a = a -> IO ()
-- type UI w a = w (Co w a)
type Component w a = w (((Co w ()) -> IO ()) -> IO ())
