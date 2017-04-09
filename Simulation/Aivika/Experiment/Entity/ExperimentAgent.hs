
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
       (ExperimentAgentConstructor(..),
        ExperimentAgent(..),
        writeValueListEntity,
        writeLastValueListEntities,
        readValueListEntities,
        readLastValueListEntities,
        readOrCreateVarEntityByName,
        readOrCreateSourceEntityByKey,
        retryAgentAction) where

import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.MVar

import Data.List

import Simulation.Aivika
import Simulation.Aivika.Experiment.Entity.Types
import Simulation.Aivika.Experiment.Entity.UUID
import Simulation.Aivika.Experiment.Entity.Utils

-- | A class type of experiment agent constructors.
class ExperimentAgentConstructor c where

  -- | Create a new experiment agent.
  newExperimentAgent :: c -> IO ExperimentAgent

-- | The experiment agent.
data ExperimentAgent =
  ExperimentAgent {
    agentRetryCount :: Int,
    -- ^ The number of retries to apply the transaction.
    agentRetryDelay :: Int,
    -- ^ The delay in microseconds before the retry.
    initialiseAgent :: IO (),
    -- ^ Initialise the agent.
    finaliseAgent :: IO (),
    -- ^ Finalise the agent.
    writeExperimentEntity :: ExperimentEntity -> IO (),
    -- ^ Write the experiment entity.
    tryWriteExperimentEntity :: ExperimentEntity -> IO Bool,
    -- ^ Try to write the experiment entity.
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
    writeMultipleValueEntities :: [MultipleValueEntity] -> IO (),
    -- ^ Write the multiple value entities.
    writeDeviationEntity :: DeviationEntity -> IO (),
    -- ^ Write the deviation entity.
    writeFinalDeviationEntities :: [FinalDeviationEntity] -> IO (),
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
    readMultipleValueEntities :: ExperimentUUID -> SourceUUID -> IO [IO MultipleValueEntity],
    -- ^ Read the multiple value entities by experiment and
    -- source identifiers.
    readDeviationEntities :: ExperimentUUID -> SourceUUID -> IO [IO DeviationEntity],
    -- ^ Read the deviation entity by experiment and
    -- source identifiers.
    readFinalDeviationEntities :: ExperimentUUID -> SourceUUID -> IO [FinalDeviationEntity]
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
            let varEntity = VarEntity { varEntityId = id,
                                        varEntityExperimentId = expId,
                                        varEntityName = name,
                                        varEntityDescription = descr }
            f <- tryWriteVarEntity agent varEntity
            case f of
              False -> return Nothing
              True  -> return (Just varEntity)
       Just varEntity ->
         do when (varEntityDescription varEntity /= descr) $
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
                                 -> SourceEntityType
                                 -- ^ the source entity type
                                 -> IO SourceEntity
readOrCreateSourceEntityByKey agent expId srcKey srcTitle srcDescr varNs srcType =
  retryAgentAction agent $
  do x <- readSourceEntityByKey agent expId srcKey
     case x of
       Nothing ->
         do varEntities <-
              forM varNs $ \(varName, varDescr) ->
              readOrCreateVarEntityByName agent expId varName varDescr
            srcId <- newRandomUUID
            let srcEntity = SourceEntity { sourceEntityId = srcId,
                                           sourceEntityExperimentId = expId,
                                           sourceEntityKey = srcKey,
                                           sourceEntityTitle = srcTitle,
                                           sourceEntityDescription = srcDescr,
                                           sourceEntityVarEntities = varEntities,
                                           sourceEntityType = srcType }
            f <- tryWriteSourceEntity agent srcEntity
            case f of
              False -> return Nothing
              True  -> return (Just srcEntity)
       Just srcEntity ->
         do let varNs' = map (\x -> (varEntityName x, varEntityDescription x)) (sourceEntityVarEntities srcEntity)
            when (sourceEntityTitle srcEntity /= srcTitle) $
              error "Source title mismatch: readOrCreateSourceEntityByKey"
            when (sourceEntityDescription srcEntity /= srcDescr) $
              error "Source description mismatch: readOrCreateSourceEntityByKey"
            when (sort varNs' /= sort varNs) $
              error "Source variable mismatch: readOrCreateSourceEntityByKey"
            when (sourceEntityType srcEntity /= srcType) $
              error "Source entity type mismatch: readOrCreateSourceEntityByKey"
            return (Just srcEntity)

-- | Write the value list entity.
writeValueListEntity :: ExperimentAgent -> ValueListEntity -> IO ()
writeValueListEntity agent e = writeMultipleValueEntities agent [convertEntity e]
  where convertEntity e   = e { multipleDataEntityItem = mconcat $ map convertDataItem (multipleDataEntityItem e) }
        convertDataItem i = flip map (dataItemValue i) $ \v -> i { dataItemValue = v }

-- | Write the last value list entities.
writeLastValueListEntities :: ExperimentAgent -> [LastValueListEntity] -> IO ()
writeLastValueListEntities agent es = writeMultipleValueEntities agent $ map convertEntity es
  where convertEntity e   = e { multipleDataEntityItem = convertDataItem (multipleDataEntityItem e) }
        convertDataItem i = flip map (dataItemValue i) $ \v -> i { dataItemValue = v }

-- | Read the value list entities by experiment and source identifiers.
readValueListEntities :: ExperimentAgent -> ExperimentUUID -> SourceUUID -> IO [IO ValueListEntity]
readValueListEntities agent expId srcId = fmap (map $ fmap convertEntity) $ readMultipleValueEntities agent expId srcId
  where convertEntity e = e { multipleDataEntityItem = groupDataItems (multipleDataEntityItem e) }
        groupDataItems = concatDataItems . groupBy (\x y -> dataItemTime x == dataItemTime y)
        concatDataItems = map packDataItems
        packDataItems [x] = x { dataItemValue = [dataItemValue x] }
        packDataItems xs@(x : _) = x { dataItemValue = map dataItemValue xs }

-- | Read the last value list entities by experiment and source identifiers.
readLastValueListEntities :: ExperimentAgent -> ExperimentUUID -> SourceUUID -> IO [IO LastValueListEntity]
readLastValueListEntities agent expId srcId = fmap (map $ fmap lastEntity) $ readValueListEntities agent expId srcId
  where lastEntity e = e { multipleDataEntityItem = lastDataItem (multipleDataEntityItem e) }
        lastDataItem []  = error "Expected a last value list: readLastValueListEntities"
        lastDataItem [x] = x
        lastDataItem (x : xs) = lastDataItem xs
