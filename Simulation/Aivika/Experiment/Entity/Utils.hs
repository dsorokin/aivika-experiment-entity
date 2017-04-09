
-- |
-- Module     : Simulation.Aivika.Experiment.Entity.Utils
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines utilits
--

module Simulation.Aivika.Experiment.Entity.Utils
       (retryAction,
        divideBy) where

import Control.Concurrent

-- | Retry the specified action.
retryAction :: Int
               -- ^ the maximum number of retries
               -> Int
               -- ^ the delay in microseconds between retries
               -> IO (Maybe a)
               -- ^ the action
               -> IO a
retryAction n delay action =
  do x <- action
     case x of
       Just a  ->
         return a
       Nothing | n <= 0 ->
         error "Could not perform the specified action: retryAction"
       Nothing ->
         do threadDelay delay
            retryAction (n - 1) delay action

-- | Divide the list by the specified number of items.
divideBy :: Int -> [a] -> [[a]]
divideBy n xs = loop ys ys'
  where loop [] zs' = []
        loop zs zs' = zs : divideBy n zs'
        (ys, ys') = splitAt n xs
        
