
-- |
-- Module     : Simulation.Aivika.Experiment.Entity.UUID
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module allows generating unique identifiers..
--

module Simulation.Aivika.Experiment.Entity.UUID
       (UUID,
        newRandomUUID) where
 
import Simulation.Aivika

-- | The unique identifier.
type UUID = String

-- | Generate an unique identifier.
newRandomUUID :: IO UUID
newRandomUUID = undefined
