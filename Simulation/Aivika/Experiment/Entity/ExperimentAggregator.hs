
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Entity.ExperimentAggregator
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines the experiment aggregator.
--

module Simulation.Aivika.Experiment.Entity.ExperimentAggregator
       (ExperimentAggregator,
        newExperimentAggregator,
        experimentAggregatorAgent,
        experimentAggregatorRunCount,
        aggregateInDeviationEntity,
        aggregateInFinalDeviationEntity,
        aggregateInMultipleValueListEntity,
        aggregateInMultipleLastValueListEntity) where

import GHC.Generics (Generic)

import Control.Monad
import Control.Concurrent.STM
import Control.DeepSeq

import Data.Monoid
import qualified Data.Map as M

import Simulation.Aivika
import Simulation.Aivika.Experiment.Entity.Types
import Simulation.Aivika.Experiment.Entity.UUID
import Simulation.Aivika.Experiment.Entity.ExperimentAgent

-- | The experiment aggregator.
data ExperimentAggregator =
  ExperimentAggregator { experimentAggregatorAgent :: ExperimentAgent,
                         -- ^ Return the underlying experiment agent.
                         experimentAggregatorRunCount :: Int,
                         -- ^ Return the run count used in the experiment.
                         experimentAggregatorDeviationEntities :: TVar (M.Map AggregateKey (AggregateValue DeviationDataItem)),
                         -- ^ The deviation entities.
                         experimentAggregatorFinalDeviationEntities :: TVar (M.Map AggregateKey (AggregateValue FinalDeviationDataItem)),
                         -- ^ The final deviation entities.
                         experimentAggregatorValueListEntities :: TVar (M.Map AggregateKey (AggregateValue ValueListDataItem)),
                         -- ^ The value list entities.
                         experimentAggregatorLastValueListEntities :: TVar (M.Map AggregateKey (AggregateValue LastValueListDataItem))
                         -- ^ The last value list entities.
                       }

-- | The aggregate key.
data AggregateKey =
  AggregateKey { aggregateKeyExperimentId :: UUID,
                 -- ^ the experiment identifier
                 aggregateKeyVarId :: UUID,
                 -- ^ the variable identifier
                 aggregateKeySourceId :: UUID
                 -- ^ the source identifier
               } deriving (Eq, Ord, Show)

-- | The aggregate value.
data AggregateValue a = AccumulatedAggregateValue !Int a
                      | FlushedAggregateValue
                      deriving (Eq, Ord, Show, Generic)

instance NFData a => NFData (AggregateValue a)

-- | The deviation data item.
type DeviationDataItem = [DataItem (SamplingStats Double)]

-- | The final deviation data item.
type FinalDeviationDataItem = DataItem (SamplingStats Double)

-- | The value list data item.
type ValueListDataItem = [DataItem [Double]]

-- | The last value list data item.
type LastValueListDataItem = DataItem [Double]

-- | Create a new experiment aggregator by the specified agent and run count.
newExperimentAggregator :: ExperimentAgent -> Int -> IO ExperimentAggregator
newExperimentAggregator agent runCount =
  do deviationEntities <- newTVarIO M.empty
     finalDeviationEntities <- newTVarIO M.empty
     valueListEntities <- newTVarIO M.empty
     finalValueListEntities <- newTVarIO M.empty
     return ExperimentAggregator { experimentAggregatorAgent = agent,
                                   experimentAggregatorRunCount = runCount,
                                   experimentAggregatorDeviationEntities = deviationEntities,
                                   experimentAggregatorFinalDeviationEntities = finalDeviationEntities,
                                   experimentAggregatorValueListEntities = valueListEntities,
                                   experimentAggregatorLastValueListEntities = finalValueListEntities
                                 }

-- | Aggregate the data in a deviation entity.
aggregateInDeviationEntity :: ExperimentAggregator -> SamplingStatsEntity -> IO ()
aggregateInDeviationEntity aggregator entity0 =
  join $
  atomically $
  do let k = AggregateKey { aggregateKeyExperimentId = dataEntityExperimentId entity0,
                            aggregateKeyVarId = dataEntityVarId entity0,
                            aggregateKeySourceId = dataEntitySourceId entity0 }
         agent    = experimentAggregatorAgent aggregator
         runCount = experimentAggregatorRunCount aggregator
         tvar     = experimentAggregatorDeviationEntities aggregator
     m <- readTVar tvar
     case M.lookup k m of
       Nothing | runCount == 1 ->
         return $
         do entity' <- samplingStatsToDeviationEntity entity0
            writeDeviationEntity agent entity'
       Nothing ->
         do let i = dataEntityItem entity0
                v = AccumulatedAggregateValue 1 i
            modifyTVar tvar $
              M.insert k v
            return $
              return ()
       Just z@(AccumulatedAggregateValue runCount' i') | runCount' == runCount - 1 ->
         do let i   = dataEntityItem entity0
                i'' = appendDeviationDataItem i i'
                v   = FlushedAggregateValue
            modifyTVar tvar $
              M.insert k v
            return $
              do entity' <- samplingStatsToDeviationEntity $ entity0 { dataEntityItem = i'' } 
                 writeDeviationEntity agent entity'
       Just z@(AccumulatedAggregateValue runCount' i') ->
         do let i   = dataEntityItem entity0
                i'' = appendDeviationDataItem i i'
                v   = AccumulatedAggregateValue (runCount' + 1) i''
            modifyTVar tvar $
              M.insert k v
            return $
              deepseq v $
              return ()
       Just FlushedAggregateValue ->
         return $
         error "The sample-based statistics is already flushed: aggregateInDeviationEntity"

-- | Aggregate the data in a final deviation entity.
aggregateInFinalDeviationEntity :: ExperimentAggregator -> FinalSamplingStatsEntity -> IO ()
aggregateInFinalDeviationEntity aggregator entity0 =
  join $
  atomically $
  do let k = AggregateKey { aggregateKeyExperimentId = dataEntityExperimentId entity0,
                            aggregateKeyVarId = dataEntityVarId entity0,
                            aggregateKeySourceId = dataEntitySourceId entity0 }
         agent    = experimentAggregatorAgent aggregator
         runCount = experimentAggregatorRunCount aggregator
         tvar     = experimentAggregatorFinalDeviationEntities aggregator
     m <- readTVar tvar
     case M.lookup k m of
       Nothing | runCount == 1 ->
         return $
         do entity' <- finalSamplingStatsToFinalDeviationEntity entity0
            writeFinalDeviationEntities agent [entity']
       Nothing ->
         do let i = dataEntityItem entity0
                v = AccumulatedAggregateValue 1 i
            modifyTVar tvar $
              M.insert k v
            return $
              return ()
       Just z@(AccumulatedAggregateValue runCount' i') | runCount' == runCount - 1 ->
         do let i   = dataEntityItem entity0
                i'' = appendFinalDeviationDataItem i i'
                v   = FlushedAggregateValue
            modifyTVar tvar $
              M.insert k v
            return $
              do entity' <- finalSamplingStatsToFinalDeviationEntity $ entity0 { dataEntityItem = i'' } 
                 writeFinalDeviationEntities agent [entity']
       Just z@(AccumulatedAggregateValue runCount' i') ->
         do let i   = dataEntityItem entity0
                i'' = appendFinalDeviationDataItem i i'
                v   = AccumulatedAggregateValue (runCount' + 1) i''
            modifyTVar tvar $
              M.insert k v
            return $
              deepseq v $
              return ()
       Just FlushedAggregateValue ->
         return $
         error "The sample-based statistics is already flushed: aggregateInFinalDeviationEntity"

-- | Aggregate the data in a multiple value list entity.
aggregateInMultipleValueListEntity :: ExperimentAggregator -> ValueListEntity -> IO ()
aggregateInMultipleValueListEntity aggregator entity0 =
  join $
  atomically $
  do let k = AggregateKey { aggregateKeyExperimentId = dataEntityExperimentId entity0,
                            aggregateKeyVarId = dataEntityVarId entity0,
                            aggregateKeySourceId = dataEntitySourceId entity0 }
         agent    = experimentAggregatorAgent aggregator
         runCount = experimentAggregatorRunCount aggregator
         tvar     = experimentAggregatorValueListEntities aggregator
     m <- readTVar tvar
     case M.lookup k m of
       Nothing | runCount == 1 ->
         return $
         do entity' <- valueListToMultipleEntity entity0
            writeMultipleValueListEntity agent entity'
       Nothing ->
         do let i = dataEntityItem entity0
                v = AccumulatedAggregateValue 1 i
            modifyTVar tvar $
              M.insert k v
            return $
              return ()
       Just z@(AccumulatedAggregateValue runCount' i') | runCount' == runCount - 1 ->
         do let i   = dataEntityItem entity0
                i'' = appendValueListDataItem i i'
                v   = FlushedAggregateValue
            modifyTVar tvar $
              M.insert k v
            return $
              do entity' <- valueListToMultipleEntity $ entity0 { dataEntityItem = i'' } 
                 writeMultipleValueListEntity agent entity'
       Just z@(AccumulatedAggregateValue runCount' i') ->
         do let i   = dataEntityItem entity0
                i'' = appendValueListDataItem i i'
                v   = AccumulatedAggregateValue (runCount' + 1) i''
            modifyTVar tvar $
              M.insert k v
            return $
              deepseq v $
              return ()
       Just FlushedAggregateValue ->
         return $
         error "The multiple value list is already flushed: aggregateInMultipleValueListEntity"

-- | Aggregate the data in a multiple last value list entity.
aggregateInMultipleLastValueListEntity :: ExperimentAggregator -> LastValueListEntity -> IO ()
aggregateInMultipleLastValueListEntity aggregator entity0 =
  join $
  atomically $
  do let k = AggregateKey { aggregateKeyExperimentId = dataEntityExperimentId entity0,
                            aggregateKeyVarId = dataEntityVarId entity0,
                            aggregateKeySourceId = dataEntitySourceId entity0 }
         agent    = experimentAggregatorAgent aggregator
         runCount = experimentAggregatorRunCount aggregator
         tvar     = experimentAggregatorLastValueListEntities aggregator
     m <- readTVar tvar
     case M.lookup k m of
       Nothing | runCount == 1 ->
         return $
         do entity' <- lastValueListToMultipleEntity entity0
            writeMultipleLastValueListEntities agent [entity']
       Nothing ->
         do let i = dataEntityItem entity0
                v = AccumulatedAggregateValue 1 i
            modifyTVar tvar $
              M.insert k v
            return $
              return ()
       Just z@(AccumulatedAggregateValue runCount' i') | runCount' == runCount - 1 ->
         do let i   = dataEntityItem entity0
                i'' = appendLastValueListDataItem i i'
                v   = FlushedAggregateValue
            modifyTVar tvar $
              M.insert k v
            return $
              do entity' <- lastValueListToMultipleEntity $ entity0 { dataEntityItem = i'' } 
                 writeMultipleLastValueListEntities agent [entity']
       Just z@(AccumulatedAggregateValue runCount' i') ->
         do let i   = dataEntityItem entity0
                i'' = appendLastValueListDataItem i i'
                v   = AccumulatedAggregateValue (runCount' + 1) i''
            modifyTVar tvar $
              M.insert k v
            return $
              deepseq v $
              return ()
       Just FlushedAggregateValue ->
         return $
         error "The multiple last value list is already flushed: aggregateInMultipleLastValueListEntity"

-- | A conversion.                  
samplingStatsToDeviationEntity :: SamplingStatsEntity -> IO DeviationEntity
samplingStatsToDeviationEntity entity0 =
  do entityId <- newRandomUUID
     return MultipleDataEntity {
       multipleDataEntityId = entityId,
       multipleDataEntityExperimentId = dataEntityExperimentId entity0,
       multipleDataEntityVarId = dataEntityVarId entity0,
       multipleDataEntitySourceId = dataEntitySourceId entity0,
       multipleDataEntityItem = dataEntityItem entity0 }

-- | A conversion.                  
finalSamplingStatsToFinalDeviationEntity :: FinalSamplingStatsEntity -> IO FinalDeviationEntity
finalSamplingStatsToFinalDeviationEntity entity0 =
  do entityId <- newRandomUUID
     return MultipleDataEntity {
       multipleDataEntityId = entityId,
       multipleDataEntityExperimentId = dataEntityExperimentId entity0,
       multipleDataEntityVarId = dataEntityVarId entity0,
       multipleDataEntitySourceId = dataEntitySourceId entity0,
       multipleDataEntityItem = dataEntityItem entity0 }

-- | A conversion.                  
valueListToMultipleEntity :: ValueListEntity -> IO MultipleValueListEntity
valueListToMultipleEntity entity0 =
  do entityId <- newRandomUUID
     return MultipleDataEntity {
       multipleDataEntityId = entityId,
       multipleDataEntityExperimentId = dataEntityExperimentId entity0,
       multipleDataEntityVarId = dataEntityVarId entity0,
       multipleDataEntitySourceId = dataEntitySourceId entity0,
       multipleDataEntityItem = dataEntityItem entity0 }

-- | A conversion.                  
lastValueListToMultipleEntity :: LastValueListEntity -> IO MultipleLastValueListEntity
lastValueListToMultipleEntity entity0 =
  do entityId <- newRandomUUID
     return MultipleDataEntity {
       multipleDataEntityId = entityId,
       multipleDataEntityExperimentId = dataEntityExperimentId entity0,
       multipleDataEntityVarId = dataEntityVarId entity0,
       multipleDataEntitySourceId = dataEntitySourceId entity0,
       multipleDataEntityItem = dataEntityItem entity0 }

-- | Append the accumulated data.
appendDeviationDataItem :: DeviationDataItem -> DeviationDataItem -> DeviationDataItem
appendDeviationDataItem [] [] = []
appendDeviationDataItem [] _  = error "Sample-based statistics size mismatch: appendDeviationDataItem"
appendDeviationDataItem _  [] = error "Sample-based statistics size mismatch: appendDeviationDataItem"
appendDeviationDataItem (x1 : xs1) (y1 : ys1)
  | dataItemIteration x1 /= dataItemIteration y1 = error "Sample-based statistics item iteration mismatch: appendDeviationDataItem"
  | otherwise = let z = x1 { dataItemValue = dataItemValue x1 <> dataItemValue y1 }
                in z : appendDeviationDataItem xs1 ys1

-- | Append the accumulated data.
appendFinalDeviationDataItem :: FinalDeviationDataItem -> FinalDeviationDataItem -> FinalDeviationDataItem
appendFinalDeviationDataItem x y
  | dataItemIteration x /= dataItemIteration y = error "Sample-based statistics item iteration mismatch: appendFinalDeviationDataItem"
  | otherwise = x { dataItemValue = dataItemValue x <> dataItemValue y }

-- | Append the accumulated data.
appendValueListDataItem :: ValueListDataItem -> ValueListDataItem -> ValueListDataItem
appendValueListDataItem [] [] = []
appendValueListDataItem [] _  = error "Value list size mismatch: appendValueListDataItem"
appendValueListDataItem _  [] = error "Value list size mismatch: appendValueListDataItem"
appendValueListDataItem (x1 : xs1) (y1 : ys1)
  | dataItemIteration x1 /= dataItemIteration y1 = error "Value list size mismatch: appendValueListDataItem"
  | otherwise = let z = x1 { dataItemValue = dataItemValue x1 <> dataItemValue y1 }
                in z : appendValueListDataItem xs1 ys1

-- | Append the accumulated data.
appendLastValueListDataItem :: LastValueListDataItem -> LastValueListDataItem -> LastValueListDataItem
appendLastValueListDataItem x y
  | dataItemIteration x /= dataItemIteration y = error "Value list iteration mismatch: appendLastValueListDataItem"
  | otherwise = x { dataItemValue = dataItemValue x <> dataItemValue y }
