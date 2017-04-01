
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
 
import Simulation.Aivika

-- | The experiment entity.
data ExperimentEntity =
  ExperimentEntity { experimentId :: String,
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
                     experimentRealStartTime :: Rational
                     -- ^ the real start time of simulation
                   } deriving (Eq, Ord, Show)
                      
-- | The integration method.
data ExperimentIntegMethod = EulerIntegMethod
                             -- ^ Euler's method
                           | RK2IntegMethod
                             -- ^ The 2nd order Runge-Kutta method
                           | RK4IntegMethod
                             -- ^ The 4th order Runge-Kutta method
                             deriving (Eq, Ord, Show)

-- | Convert the integration method to an integer.
experimentIntegMethodToInt :: ExperimentIntegMethod -> Int
experimentIntegMethodToInt EulerIntegMethod = 1
experimentIntegMethodToInt RK2IntegMethod   = 2
experimentIntegMethodToInt RK4IntegMethod   = 3

-- | The variable entity
data VarEntity =
  VarEntity { varId :: String,
              -- ^ an identifier
              varExperimentId :: String,
              -- ^ the experiment identifier.
              varName :: String,
              -- ^ the variable name
              varDescription :: String
              -- ^ the variable description
            } deriving (Eq, Ord, Show)

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
  DataEntity { dataId :: String,
               -- ^ an identifier
               dataExperimentId :: String,
               -- ^ the experiment identifier
               dataRunIndex :: !Int,
               -- ^ the run index
               dataVarId :: String,
               -- ^ the variable identifier
               dataSourceId :: String,
               -- ^ the source identifier
               dataItem :: a
               -- ^ the data item
             } deriving (Eq, Ord, Show)

-- | The multiple data entity.
data MultipleDataEntity a =
  MultipleDataEntity { multipleDataId :: String,
                       -- ^ an identifier
                       multipleDataExperimentId :: String,
                       -- ^ the experiment identifier
                       multipleDataVarId :: String,
                       -- ^ the variable identifier
                       multipleDataSourceId :: String,
                       -- ^ the source identifier
                       multipleDataItem :: a
                       -- ^ the data item
                     } deriving (Eq, Ord, Show)

-- | The aggregated data entity.
data AggregatedDataEntity a =
  AggregatedDataEntity { aggregatedDataId :: String,
                         -- ^ an identifier
                         aggregaredDataExperimentId :: String,
                         -- ^ the experiment identifier
                         aggregatedDataVarId :: String,
                         -- ^ the variable identifier
                         aggregatedDataSourceId :: String,
                         -- ^ the source identifier
                         aggregatedDataItem :: a
                         -- ^ the data item
                       } deriving (Eq, Ord, Show)

-- | The data item.
data DataItem a =
  DataItem { dataItemValue :: a,
             -- ^ the data item value
             dataItemIteration :: !Int,
             -- ^ the integration iteration
             dataItemTime :: !Double
             -- ^ the corresponding modeling time
           } deriving (Eq, Ord, Show)
