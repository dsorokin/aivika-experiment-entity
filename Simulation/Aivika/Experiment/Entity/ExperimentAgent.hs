
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
       (ExperimentAgent(..),
        readOrCreateVarEntityByName,
        readOrCreateSourceEntityByKey,
        retryAgentAction) where

import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.MVar

import Simulation.Aivika
import Simulation.Aivika.Experiment.Entity.Types
import Simulation.Aivika.Experiment.Entity.UUID
import Simulation.Aivika.Experiment.Entity.Utils

-- | The experiment agent.
data ExperimentAgent =
  ExperimentAgent {
    agentRetryCount :: Int,
    -- ^ The number of retries to apply the transaction.
    agentRetryDelay :: Int,
    -- ^ The delay in microseconds before the retry.
    writeExperimentEntity :: ExperimentEntity -> IO (),
    -- ^ Write the experiment entity.
    tryWriteSourceEntity :: SourceEntity -> IO Bool,
    -- ^ Try to write the source entity.
    tryWriteVarEntity :: VarEntity -> IO Bool,
    -- ^ Try to write the variable entity.
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
    readExperimentEntity :: ExperimentUUID -> IO (Maybe ExperimentEntity),
    -- ^ Read the experiment entity by its identifier.
    readExperimentEntities :: IO [ExperimentEntity],
    -- ^ Read the experiment entities.
    readVarEntity :: ExperimentUUID -> VarUUID -> IO (Maybe VarEntity),
    -- ^ Read the variable entity by experiment and variable identifiers.
    readVarEntityByName :: ExperimentUUID -> String -> IO (Maybe VarEntity),
    -- ^ Read the variable entity by experiment and variable name.
    readVarEntities :: ExperimentUUID -> IO [VarEntity],
    -- ^ Read the variable entities by the experiment identifier.
    readSourceEntity :: ExperimentUUID -> SourceUUID -> IO (Maybe SourceEntity),
    -- ^ Read the source entity by the experiment and source identifier.
    readSourceEntityByKey :: ExperimentUUID -> SourceKey -> IO (Maybe SourceEntity),
    -- ^ Read the source entity by the experiment identifier and source key.
    readSourceEntities :: ExperimentUUID -> IO [SourceEntity],
    -- ^ Read the source entities by the experiment identifier.
    readTimeSeriesEntities :: ExperimentUUID -> SourceUUID -> Int -> IO [IO TimeSeriesEntity],
    -- ^ Read the time series entities by experiment and
    -- source identifiers, run index.
    readLastValueEntities :: ExperimentUUID -> SourceUUID -> Int -> IO [LastValueEntity],
    -- ^ Read the last value entities by experiment and
    -- source identifiers, run index.
    readSamplingStatsEntities :: ExperimentUUID -> SourceUUID -> Int -> IO [IO SamplingStatsEntity],
    -- ^ Read the sample-based statistics entities by experiment and
    -- source identifiers, run index.
    readFinalSamplingStatsEntities :: ExperimentUUID -> SourceUUID -> Int -> IO [FinalSamplingStatsEntity],
    -- ^ Read the entities of sample-based statistics in final points by
    -- experiment and source identifiers, run index.
    readTimingStatsEntities :: ExperimentUUID -> SourceUUID -> Int -> IO [IO TimingStatsEntity],
    -- ^ Read the time-dependent statistics entities by
    -- experiment and source identifiers, run index.
    readFinalTimingStatsEntities :: ExperimentUUID -> SourceUUID -> Int -> IO [FinalTimingStatsEntity],
    -- ^ Read the entities of time-dependent statistics in final points by
    -- experiment and source identifiers. run index.
    readValueListEntities :: ExperimentUUID -> SourceUUID -> IO [IO ValueListEntity],
    -- ^ Read the value list entities by experiment and
    -- source identifiers.
    readLastValueListEntities :: ExperimentUUID -> SourceUUID -> IO [IO LastValueListEntity],
    -- ^ Read the last value list entities by experiment and
    -- source identifiers.
    readDeviationEntity :: ExperimentUUID -> SourceUUID -> IO [IO DeviationEntity],
    -- ^ Read the deviation entity by experiment and
    -- source identifiers.
    readFinalDeviationEntity :: ExperimentUUID -> SourceUUID -> IO [FinalDeviationEntity]
    -- ^ Read the final deviation entity by experiment and
    -- source identifiers.
    }

-- | Retry the action for the specified number of times.
retryAgentAction :: ExperimentAgent -> IO (Maybe a) -> IO a
retryAgentAction agent = retryAction (agentRetryCount agent) (agentRetryDelay agent)

-- | Read or create a variable entity by the specified experiment identifier,
-- variable name and description.
readOrCreateVarEntityByName :: ExperimentAgent
                               -- ^ the agent
                               -> ExperimentUUID
                               -- ^ the experiment identifier
                               -> String
                               -- ^ the variable name
                               -> String
                               -- ^ the variable description
                               -> IO VarEntity
readOrCreateVarEntityByName agent expId name descr =
  retryAgentAction agent $
  do x <- readVarEntityByName agent expId name
     case x of
       Nothing ->
         do id <- newRandomUUID
            let varEntity = VarEntity { varId = id,
                                        varExperimentId = expId,
                                        varName = name,
                                        varDescription = descr }
            f <- tryWriteVarEntity agent varEntity
            case f of
              False -> return Nothing
              True  -> return (Just varEntity)
       Just varEntity ->
         do when (varDescription varEntity /= descr) $
              error "Variable description mismatch: readOrCreateVarEntityByName"
            return (Just varEntity)

-- | Read or create a source entity by the specified experiment identifier,
-- source key, title, description and a list of pairs of variable names and descriptions.
readOrCreateSourceEntityByKey :: ExperimentAgent
                                 -- ^ the agent
                                 -> ExperimentUUID
                                 -- ^ the experiment identifier
                                 -> SourceKey
                                 -- ^ the source key
                                 -> String
                                 -- ^ the title
                                 -> String
                                 -- ^ the description
                                 -> [(String, String)]
                                 -- ^ the pairs of variable names and descriptions
                                 -> IO SourceEntity
readOrCreateSourceEntityByKey agent expId srcKey srcTitle srcDescr varNs =
  retryAgentAction agent $
  do x <- readSourceEntityByKey agent expId srcKey
     case x of
       Nothing ->
         do varEntities <-
              forM varNs $ \(varName, varDescr) ->
              readOrCreateVarEntityByName agent expId varName varDescr
            srcId <- newRandomUUID
            let srcEntity = SourceEntity { sourceId = srcId,
                                           sourceExperimentId = expId,
                                           sourceKey = srcKey,
                                           sourceTitle = srcTitle,
                                           sourceDescription = srcDescr,
                                           sourceVarEntities = varEntities }
            f <- tryWriteSourceEntity agent srcEntity
            case f of
              False -> return Nothing
              True  -> return (Just srcEntity)
       Just srcEntity ->
         do let varNs' = map (\x -> (varName x, varDescription x)) (sourceVarEntities srcEntity)
            when (sourceTitle srcEntity /= srcTitle) $
              error "Source title mismatch: readOrCreateSourceEntityByKey"
            when (sourceDescription srcEntity /= srcDescr) $
              error "Source description mismatch: readOrCreateSourceEntityByKey"
            when (varNs' /= varNs) $
              error "Source variable mismatch: readOrCreateSourceEntityByKey"
            return (Just srcEntity)
