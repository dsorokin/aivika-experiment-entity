
-- |
-- Module     : Simulation.Aivika.Experiment.Entity.UUID
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module allows generating unique identifiers.
--

module Simulation.Aivika.Experiment.Entity.UUID
       (UUID,
        newRandomUUID) where

import Numeric

import Data.Int

import System.Random

import Simulation.Aivika

-- | The unique identifier.
type UUID = Int64

-- | Generate an unique identifier.
newRandomUUID :: IO UUID
newRandomUUID = getStdRandom $ randomR (0, maxBound)

-- -- | Generate an unique identifier.
-- newRandomUUID :: IO UUID
-- newRandomUUID =
--   do x1 <- getStdRandom $ randomR (0, (16 ^ 4 - 1))
--      x2 <- getStdRandom $ randomR (0, (16 ^ 4 - 1))
--      x3 <- getStdRandom $ randomR (0, (16 ^ 4 - 1))
--      x4 <- getStdRandom $ randomR (0, (16 ^ 4 - 1))
--      x5 <- getStdRandom $ randomR (0, (16 ^ 4 - 1))
--      x6 <- getStdRandom $ randomR (0, (16 ^ 4 - 1))
--      x7 <- getStdRandom $ randomR (0, (16 ^ 4 - 1))
--      x8 <- getStdRandom $ randomR (0, (16 ^ 2 - 1))
--      let f = showHexWithLeadingZeros 4 x1 .
--              showHexWithLeadingZeros 4 x2 .
--              showChar '-' .
--              showHexWithLeadingZeros 4 x3 .
--              showChar '-' .
--              showHexWithLeadingZeros 4 x4 .
--              showChar '-' .
--              showHexWithLeadingZeros 4 x5 .
--              showChar '-' .
--              showHexWithLeadingZeros 4 x6 .
--              showHexWithLeadingZeros 4 x7 .
--              showHexWithLeadingZeros 2 x8
--      return (f [])

-- | Show the hexadecimal number with leading zeros if required.
showHexWithLeadingZeros :: Int -> Int -> ShowS
showHexWithLeadingZeros n v = addLeadingZeros n (showHex v [])

-- | Add the leading zeros if required.
addLeadingZeros :: Int -> String -> ShowS
addLeadingZeros n xs = loop n xs id
  where loop 0 xs acc      = acc . showString xs
        loop i [] acc      = showChar '0' . loop (i - 1) [] acc
        loop i (x: xs) acc = loop (i - 1) xs (acc . showChar x)
