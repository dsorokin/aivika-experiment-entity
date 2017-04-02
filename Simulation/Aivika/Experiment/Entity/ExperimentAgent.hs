
-- |
-- Module     : Simulation.Aivika.Experiment.Entity.ExperimentAgent
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines the experiment agent.
--

module Simulation.Aivika.Experiment.Entity.ExperimentAgent
       (ExperimentAgent(..)) where
 
import Simulation.Aivika
import Simulation.Aivika.Experiment.Entity.Types

-- | The experiment agent.
data ExperimentAgent =
  ExperimentAgent {
    writeExperimentEntity :: ExperimentEntity -> IO (),
    -- ^ Write the experiment entity.
    writeVarEntity :: VarEntity -> IO (),
    -- ^ Write the variable entity.
    writeVarEntities :: [VarEntity] -> IO (),
    -- ^ Write the variable entities.
    writeTimeSeriesEntity :: TimeSeriesEntity -> IO (),
    -- ^ Write the time series entity.
    writeLastValueEntities :: [LastValueEntity] -> IO (),
    -- ^ Write the last value entities.
    writeSamplingStatsEntity :: SamplingStatsEntity -> IO (),
    -- ^ Write the sample-based statistics entity.
    writeFinalSamplingStatsEntities :: [FinalSamplingStatsEntity] -> IO (),
    -- ^ Write the entities of sample-based statistics in final time point.
    writeTimingStatsEntity :: TimingStatsEntity -> IO (),
    -- ^ Write the time-dependent statistics entity.
    writeFinalTimingStatsEntity :: FinalTimingStatsEntity -> IO (),
    -- ^ Write the entities of time-dependent statistics in final time point.
    writeValueListEntity :: ValueListEntity -> IO (),
    -- ^ Write the value list entity.
    writeLastValueListEntities :: [LastValueListEntity] -> IO (),
    -- ^ Write the last value list entities.
    writeDeviationEntity :: DeviationEntity -> IO (),
    -- ^ Write the deviation entity.
    writeFinalDeviationEntity :: [FinalDeviationEntity] -> IO (),
    -- ^ Write the final deviation entities.
    readExperimentEntity :: Int -> IO ExperimentEntity,
    -- ^ Read the experiment entity by its identifier.
    readExperimentEntities :: IO [ExperimentEntity],
    -- ^ Read the experiment entities.
    readVarEntity :: Int -> Int -> IO VarEntity,
    -- ^ Read the variable entity by experiment and variable identifiers.
    readVarEntities :: Int -> IO [VarEntity],
    -- ^ Read the variable entities by the experiment identifier.
    readOrCreateVarEntities :: Int -> [(String, String)] -> IO [VarEntity],
    -- ^ Requests the variable entities by the specified experiment identifier
    -- and pairs of the variable names and descriptions, creating
    -- the variable entities if needed.
    readTimeSeriesEntities :: Int -> Int -> Int -> IO [IO TimeSeriesEntity],
    -- ^ Read the time series entities by experiment and
    -- source identifiers, run index.
    readLastValueEntities :: Int -> Int -> Int -> IO [LastValueEntity],
    -- ^ Read the last value entities by experiment and
    -- source identifiers, run index.
    readSamplingStatsEntities :: Int -> Int -> Int -> IO [IO SamplingStatsEntity],
    -- ^ Read the sample-based statistics entities by experiment and
    -- source identifiers, run index.
    readFinalSamplingStatsEntities :: Int -> Int -> Int -> IO [FinalSamplingStatsEntity],
    -- ^ Read the entities of sample-based statistics in final points by
    -- experiment and source identifiers, run index.
    readTimingStatsEntities :: Int -> Int -> Int -> IO [IO TimingStatsEntity],
    -- ^ Read the time-dependent statistics entities by
    -- experiment and source identifiers, run index.
    readFinalTimingStatsEntities :: Int -> Int -> Int -> IO [FinalTimingStatsEntity],
    -- ^ Read the entities of time-dependent statistics in final points by
    -- experiment and source identifiers. run index.
    readValueListEntities :: Int -> Int -> IO [IO ValueListEntity],
    -- ^ Read the value list entities by experiment and
    -- source identifiers.
    readLastValueListEntities :: Int -> Int -> IO [IO LastValueListEntity],
    -- ^ Read the last value list entities by experiment and
    -- source identifiers.
    readDeviationEntity :: Int -> Int -> IO [IO DeviationEntity],
    -- ^ Read the deviation entity by experiment and
    -- source identifiers.
    readFinalDeviationEntity :: Int -> Int -> IO [FinalDeviationEntity]
    -- ^ Read the final deviation entity by experiment and
    -- source identifiers.
    }
