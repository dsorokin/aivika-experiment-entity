
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
       (ExperimentEntity(..),
        ExperimentIntegMethod(..),
        experimentIntegMethodToInt,
        experimentIntegMethodFromInt,
        VarEntity(..),
        TimeSeriesEntity(..),
        LastValueEntity(..),
        SamplingStatsEntity(..),
        FinalSamplingStatsEntity(..),
        TimingStatsEntity(..),
        FinalTimingStatsEntity(..),
        ValueListEntity(..),
        LastValueListEntity(..),
        DeviationEntity(..),
        FinalDeviationEntity(..),
        DataEntity(..),
        MultipleDataEntity(..),
        AggregatedDataEntity(..),
        DataItem(..)) where
 
import GHC.Generics (Generic)

import Control.DeepSeq

import Data.Typeable
import Data.Binary

import Simulation.Aivika
import Simulation.Aivika.Experiment.Entity.UUID

-- | The experiment entity.
data ExperimentEntity =
  ExperimentEntity { experimentId :: UUID,
                     -- ^ the experiment identifier
                     experimentTitle :: String,
                     -- ^ the experiment title
                     experimentDescription :: String,
                     -- ^ the experiment description
                     experimentStartTime :: !Double,
                     -- ^ the start modeling time
                     experimentStopTime :: !Double,
                     -- ^ the stop modeling time
                     experimentDT :: !Double,
                     -- ^ the integration time step
                     experimentIntegMethod :: ExperimentIntegMethod,
                     -- ^ the integration method
                     experimentRunCount :: !Int,
                     -- ^ the run count
                     experimentRealStartTime :: String
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

-- | The variable entity
data VarEntity =
  VarEntity { varId :: UUID,
              -- ^ an identifier
              varExperimentId :: UUID,
              -- ^ the experiment identifier.
              varName :: String,
              -- ^ the variable name
              varDescription :: String
              -- ^ the variable description
            } deriving (Eq, Ord, Show, Typeable, Generic)

instance NFData VarEntity
instance Binary VarEntity

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
type ValueListEntity = MultipleDataEntity [DataItem [Double]]

-- | Entity of values in the final time point.
type LastValueListEntity = MultipleDataEntity (DataItem [Double])

-- | Entity of aggregated sample-based statistics.
type DeviationEntity = AggregatedDataEntity [DataItem (SamplingStats Double)]

-- | Entity of aggregated sample-based statistics in final time point.
type FinalDeviationEntity = AggregatedDataEntity (DataItem (SamplingStats Double))

-- | The data entity.
data DataEntity a =
  DataEntity { dataId :: UUID,
               -- ^ an identifier
               dataExperimentId :: UUID,
               -- ^ the experiment identifier
               dataRunIndex :: !Int,
               -- ^ the run index
               dataVarId :: UUID,
               -- ^ the variable identifier
               dataSourceId :: UUID,
               -- ^ the source identifier
               dataItem :: a
               -- ^ the data item
             } deriving (Eq, Ord, Show, Typeable, Generic)

instance NFData a => NFData (DataEntity a)
instance Binary a => Binary (DataEntity a)

-- | The multiple data entity.
data MultipleDataEntity a =
  MultipleDataEntity { multipleDataId :: UUID,
                       -- ^ an identifier
                       multipleDataExperimentId :: UUID,
                       -- ^ the experiment identifier
                       multipleDataVarId :: UUID,
                       -- ^ the variable identifier
                       multipleDataSourceId :: UUID,
                       -- ^ the source identifier
                       multipleDataItem :: a
                       -- ^ the data item
                     } deriving (Eq, Ord, Show, Typeable, Generic)

instance NFData a => NFData (MultipleDataEntity a)
instance Binary a => Binary (MultipleDataEntity a)

-- | The aggregated data entity.
data AggregatedDataEntity a =
  AggregatedDataEntity { aggregatedDataId :: UUID,
                         -- ^ an identifier
                         aggregatedDataExperimentId :: UUID,
                         -- ^ the experiment identifier
                         aggregatedDataVarId :: UUID,
                         -- ^ the variable identifier
                         aggregatedDataSourceId :: UUID,
                         -- ^ the source identifier
                         aggregatedDataItem :: a
                         -- ^ the data item
                       } deriving (Eq, Ord, Show, Typeable, Generic)

instance NFData a => NFData (AggregatedDataEntity a)
instance Binary a => Binary (AggregatedDataEntity a)

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
