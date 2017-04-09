
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Entity.Types
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines entities of simulation experiments.
--

module Simulation.Aivika.Experiment.Entity.Types
       (ExperimentUUID,
        ExperimentEntity(..),
        ExperimentIntegMethod(..),
        experimentIntegMethodToInt,
        experimentIntegMethodFromInt,
        VarUUID,
        VarEntity(..),
        SourceUUID,
        SourceKey,
        SourceEntity(..),
        SourceEntityType(..),
        sourceEntityTypeToInt,
        sourceEntityTypeFromInt,
        TimeSeriesEntity(..),
        LastValueEntity(..),
        SamplingStatsEntity(..),
        FinalSamplingStatsEntity(..),
        TimingStatsEntity(..),
        FinalTimingStatsEntity(..),
        ValueListEntity(..),
        LastValueListEntity(..),
        MultipleValueEntity(..),
        MultipleValueListEntity(..),
        MultipleLastValueListEntity(..),
        DeviationEntity(..),
        FinalDeviationEntity(..),
        DataUUID,
        DataEntity(..),
        MultipleDataUUID,
        MultipleDataEntity(..),
        DataItem(..)) where
 
import GHC.Generics (Generic)

import Control.DeepSeq

import Data.Typeable
import Data.Binary

import Simulation.Aivika
import Simulation.Aivika.Experiment.Entity.UUID

-- | The experiment identifier.
type ExperimentUUID = UUID

-- | The experiment entity.
data ExperimentEntity =
  ExperimentEntity { experimentEntityId :: ExperimentUUID,
                     -- ^ the experiment identifier
                     experimentEntityTitle :: String,
                     -- ^ the experiment title
                     experimentEntityDescription :: String,
                     -- ^ the experiment description
                     experimentEntityStartTime :: !Double,
                     -- ^ the start modeling time
                     experimentEntityStopTime :: !Double,
                     -- ^ the stop modeling time
                     experimentEntityDT :: !Double,
                     -- ^ the integration time step
                     experimentEntityIntegMethod :: ExperimentIntegMethod,
                     -- ^ the integration method
                     experimentEntityRunCount :: !Int,
                     -- ^ the run count
                     experimentEntityRealStartTime :: String
                     -- ^ the real start time of simulation
                   } deriving (Eq, Ord, Show, Typeable, Generic)

instance NFData ExperimentEntity
instance Binary ExperimentEntity
                              
-- | The integration method.
data ExperimentIntegMethod = EulerIntegMethod
                             -- ^ Euler's method
                           | RK2IntegMethod
                             -- ^ The 2nd order Runge-Kutta method
                           | RK4IntegMethod
                             -- ^ The 4th order Runge-Kutta method
                             deriving (Eq, Ord, Show, Typeable, Generic)

instance NFData ExperimentIntegMethod
instance Binary ExperimentIntegMethod

-- | Convert the integration method to an integer.
experimentIntegMethodToInt :: ExperimentIntegMethod -> Int
experimentIntegMethodToInt EulerIntegMethod = 1
experimentIntegMethodToInt RK2IntegMethod   = 2
experimentIntegMethodToInt RK4IntegMethod   = 3

-- | Convert the integration method from the integer.
experimentIntegMethodFromInt :: Int -> ExperimentIntegMethod
experimentIntegMethodFromInt 1 = EulerIntegMethod
experimentIntegMethodFromInt 2 = RK2IntegMethod
experimentIntegMethodFromInt 3 = RK4IntegMethod
experimentIntegMethodFromInt i =
  error $
  "Unknown integration method code (" ++ show i ++
  "): experimentIntegMethodFromInt"

-- | The variable identifier.
type VarUUID = UUID

-- | The variable entity, where PK is ('varEntityExperimentId', 'varEntityName') for consistency.
data VarEntity =
  VarEntity { varEntityId :: VarUUID,
              -- ^ an identifier.
              varEntityExperimentId :: ExperimentUUID,
              -- ^ the experiment identifier.
              varEntityName :: String,
              -- ^ the variable name
              varEntityDescription :: String
              -- ^ the variable description
            } deriving (Eq, Ord, Show, Typeable, Generic)

instance NFData VarEntity
instance Binary VarEntity

-- | The source identifier.
type SourceUUID = UUID

-- | The source key.
type SourceKey = String

-- | The source entity, where PK is ('sourceEntityExperimentId', 'sourceEntityKey') for consistency.
data SourceEntity =
  SourceEntity { sourceEntityId :: SourceUUID,
                 -- ^ an identifier.
                 sourceEntityExperimentId :: ExperimentUUID,
                 -- ^ the experiment identifier.
                 sourceEntityKey :: SourceKey,
                 -- ^ the source index.
                 sourceEntityTitle :: String,
                 -- ^ the source title.
                 sourceEntityDescription :: String,
                 -- ^ the source description.
                 sourceEntityVarEntities :: [VarEntity],
                 -- ^ the source variable entities.
                 sourceEntityType :: SourceEntityType
                 -- ^ the source entity type.
               } deriving (Eq, Ord, Show, Typeable, Generic)

instance NFData SourceEntity
instance Binary SourceEntity

-- | The source entity type.
data SourceEntityType = TimeSeriesEntityType
                        -- ^ the time series entity
                      | LastValueEntityType
                        -- ^ the last values in final time point
                      | SamplingStatsEnityType
                        -- ^ the sample-based statistics entity
                      | FinalSamplingStatsEntityType
                        -- ^ the entity of sample-based statistics in final time point
                      | TimingStatsEntityType
                        -- ^ the time-dependent statistics entity
                      | FinalTimingStatsEntityType
                        -- ^ the entity of time-dependent statistics in final time point
                      | ValueListEntityType
                        -- ^ the value list entity
                      | LastValueListEntityType
                        -- ^ the entity of values in the final time point
                      | DeviationEntityType
                        -- ^ the entity of aggregated sample-based statistics
                      | FinalDeviationEntityType
                        -- ^ the entity of aggregated sample-based statistics in final time point
                      deriving (Eq, Ord, Show, Typeable, Generic)

instance NFData SourceEntityType
instance Binary SourceEntityType

-- | Convert the source entity type to an integer.
sourceEntityTypeToInt :: SourceEntityType -> Int
sourceEntityTypeToInt TimeSeriesEntityType         = 1
sourceEntityTypeToInt LastValueEntityType          = 2
sourceEntityTypeToInt SamplingStatsEnityType       = 3
sourceEntityTypeToInt FinalSamplingStatsEntityType = 4
sourceEntityTypeToInt TimingStatsEntityType        = 5
sourceEntityTypeToInt FinalTimingStatsEntityType   = 6
sourceEntityTypeToInt ValueListEntityType          = 7
sourceEntityTypeToInt LastValueListEntityType      = 8
sourceEntityTypeToInt DeviationEntityType          = 9
sourceEntityTypeToInt FinalDeviationEntityType     = 10

-- | Convert the source entity type from the integer.
sourceEntityTypeFromInt :: Int -> SourceEntityType
sourceEntityTypeFromInt 1  = TimeSeriesEntityType
sourceEntityTypeFromInt 2  = LastValueEntityType
sourceEntityTypeFromInt 3  = SamplingStatsEnityType
sourceEntityTypeFromInt 4  = FinalSamplingStatsEntityType
sourceEntityTypeFromInt 5  = TimingStatsEntityType
sourceEntityTypeFromInt 6  = FinalTimingStatsEntityType
sourceEntityTypeFromInt 7  = ValueListEntityType
sourceEntityTypeFromInt 8  = LastValueListEntityType
sourceEntityTypeFromInt 9  = DeviationEntityType
sourceEntityTypeFromInt 10 = FinalDeviationEntityType
sourceEntityTypeFromInt i  =
  error $
  "Unknown source entity type code (" ++ show i ++
  "): sourceEntityTypeFromInt"

-- | The time series entity.
type TimeSeriesEntity = DataEntity [DataItem Double]

-- | The last value entity.
type LastValueEntity = DataEntity (DataItem Double)

-- | The sample-based statistics entity.
type SamplingStatsEntity = DataEntity [DataItem (SamplingStats Double)]

-- | Entity of the sample-based statistics in final time point.
type FinalSamplingStatsEntity = DataEntity (DataItem (SamplingStats Double))

-- | The time-dependent statistics entity.
type TimingStatsEntity = DataEntity [DataItem (TimingStats Double)]

-- | Entity of the time-dependent statistics in final time point.
type FinalTimingStatsEntity = DataEntity (DataItem (TimingStats Double))

-- | The value list entity.
type ValueListEntity = DataEntity [DataItem [Double]]

-- | Entity of values in the final time point.
type LastValueListEntity = DataEntity (DataItem [Double])

-- | The multiple value entity.
type MultipleValueEntity = MultipleDataEntity [DataItem Double]

-- | The value list entity.
type MultipleValueListEntity = MultipleDataEntity [DataItem [Double]]

-- | Entity of values in the final time point.
type MultipleLastValueListEntity = MultipleDataEntity (DataItem [Double])

-- | Entity of aggregated sample-based statistics.
type DeviationEntity = MultipleDataEntity [DataItem (SamplingStats Double)]

-- | Entity of aggregated sample-based statistics in final time point.
type FinalDeviationEntity = MultipleDataEntity (DataItem (SamplingStats Double))

-- | The data identifier.
type DataUUID = UUID

-- | The data entity.
data DataEntity a =
  DataEntity { dataEntityId :: DataUUID,
               -- ^ an identifier
               dataEntityExperimentId :: ExperimentUUID,
               -- ^ the experiment identifier
               dataEntityRunIndex :: !Int,
               -- ^ the run index
               dataEntityVarId :: VarUUID,
               -- ^ the variable identifier
               dataEntitySourceId :: SourceUUID,
               -- ^ the source identifier
               dataEntityItem :: a
               -- ^ the data item
             } deriving (Eq, Ord, Show, Typeable, Generic)

instance NFData a => NFData (DataEntity a)
instance Binary a => Binary (DataEntity a)

-- | The multiple data identifier.
type MultipleDataUUID = UUID

-- | The multiple data entity.
data MultipleDataEntity a =
  MultipleDataEntity { multipleDataEntityId :: MultipleDataUUID,
                       -- ^ an identifier
                       multipleDataEntityExperimentId :: ExperimentUUID,
                       -- ^ the experiment identifier
                       multipleDataEntityVarId :: VarUUID,
                       -- ^ the variable identifier
                       multipleDataEntitySourceId :: SourceUUID,
                       -- ^ the source identifier
                       multipleDataEntityItem :: a
                       -- ^ the data item
                     } deriving (Eq, Ord, Show, Typeable, Generic)

instance NFData a => NFData (MultipleDataEntity a)
instance Binary a => Binary (MultipleDataEntity a)

-- | The data item.
data DataItem a =
  DataItem { dataItemValue :: a,
             -- ^ the data item value
             dataItemIteration :: !Int,
             -- ^ the integration iteration
             dataItemTime :: !Double
             -- ^ the corresponding modeling time
           } deriving (Eq, Ord, Show, Typeable, Generic)

instance NFData a => NFData (DataItem a)
instance Binary a => Binary (DataItem a)
