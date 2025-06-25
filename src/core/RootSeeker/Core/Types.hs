{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module RootSeeker.Core.Types
  ( -- * Core Types
    Event(..)
  , EventType(..)
  , Service(..)
  , ServiceId(..)
  , Timestamp(..)
  , Latency(..)
  , ErrorCode(..)
  , Severity(..)
  
  -- * Analysis Types
  , RootCause(..)
  , Confidence(..)
  , AnalysisResult(..)
  , DependencyGraph(..)
  , Timeline(..)
  
  -- * Rule Types
  , Rule(..)
  , RuleId(..)
  , RuleCondition(..)
  , RuleAction(..)
  , RuleMatch(..)
  
  -- * Utility Types
  , LogSource(..)
  , LogFormat(..)
  , MetricValue(..)
  
  -- * Lenses
  , eventTimestamp
  , eventService
  , eventType
  , eventSeverity
  , eventLatency
  , eventErrorCode
  , eventMetadata
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Map (Map)
import Data.Vector (Vector)
import Data.UUID (UUID)
import Data.Scientific (Scientific)
import GHC.Generics (Generic)
import Control.Lens (Lens', lens)

-- | Unique identifier for a service
newtype ServiceId = ServiceId { unServiceId :: Text }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

-- | Service information
data Service = Service
  { serviceId :: ServiceId
  , serviceName :: Text
  , serviceVersion :: Text
  , serviceDependencies :: [ServiceId]
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Timestamp wrapper
newtype Timestamp = Timestamp { unTimestamp :: UTCTime }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

-- | Latency in milliseconds
newtype Latency = Latency { unLatency :: Scientific }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

-- | Error code
newtype ErrorCode = ErrorCode { unErrorCode :: Text }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

-- | Event severity levels
data Severity
  = Debug
  | Info
  | Warning
  | Error
  | Critical
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

-- | Event types
data EventType
  = ServiceStart
  | ServiceStop
  | ServiceCrash
  | RequestStart
  | RequestComplete
  | RequestError
  | MetricUpdate
  | LogEntry
  | HealthCheck
  | CustomEvent Text
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Metric values
data MetricValue
  = CounterValue Scientific
  | GaugeValue Scientific
  | HistogramValue (Vector Scientific)
  | SummaryValue Scientific Scientific
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Log sources
data LogSource
  = KubernetesPod Text Text Text  -- namespace, pod, container
  | FilePath Text
  | Syslog
  | CustomSource Text
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Log formats
data LogFormat
  = JSONFormat
  | YAMLFormat
  | SyslogFormat
  | CustomFormat Text
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Core event structure
data Event = Event
  { eventId :: UUID
  , eventTimestamp :: Timestamp
  , eventService :: ServiceId
  , eventType :: EventType
  , eventSeverity :: Severity
  , eventLatency :: Maybe Latency
  , eventErrorCode :: Maybe ErrorCode
  , eventMessage :: Text
  , eventMetadata :: Map Text Text
  , eventMetrics :: Map Text MetricValue
  , eventSource :: LogSource
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Rule identifier
newtype RuleId = RuleId { unRuleId :: Text }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

-- | Rule conditions
data RuleCondition
  = EventCondition EventType
  | ServiceCondition ServiceId
  | SeverityCondition Severity
  | LatencyCondition Scientific  -- threshold in ms
  | ErrorCondition ErrorCode
  | TimeWindowCondition Timestamp Timestamp
  | AndCondition [RuleCondition]
  | OrCondition [RuleCondition]
  | NotCondition RuleCondition
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Rule actions
data RuleAction
  = FlagRootCause ServiceId Confidence
  | AddDependency ServiceId ServiceId
  | UpdateConfidence RootCause Confidence
  | CustomAction Text (Map Text Text)
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Rule definition
data Rule = Rule
  { ruleId :: RuleId
  , ruleName :: Text
  , ruleDescription :: Text
  , ruleCondition :: RuleCondition
  , ruleAction :: RuleAction
  , rulePriority :: Int
  , ruleEnabled :: Bool
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Confidence level (0.0 to 1.0)
newtype Confidence = Confidence { unConfidence :: Scientific }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

-- | Root cause candidate
data RootCause = RootCause
  { rootCauseService :: ServiceId
  , rootCauseDescription :: Text
  , rootCauseConfidence :: Confidence
  , rootCauseEvidence :: [Event]
  , rootCauseTimestamp :: Timestamp
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Rule match result
data RuleMatch = RuleMatch
  { ruleMatchRule :: Rule
  , ruleMatchEvents :: [Event]
  , ruleMatchConfidence :: Confidence
  , ruleMatchTimestamp :: Timestamp
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Analysis result
data AnalysisResult = AnalysisResult
  { analysisRootCauses :: [RootCause]
  , analysisRuleMatches :: [RuleMatch]
  , analysisTimeline :: Timeline
  , analysisDependencyGraph :: DependencyGraph
  , analysisConfidence :: Confidence
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Timeline of events
newtype Timeline = Timeline { unTimeline :: Vector Event }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Dependency graph (placeholder for now)
data DependencyGraph = DependencyGraph
  { graphNodes :: [Service]
  , graphEdges :: [(ServiceId, ServiceId)]
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Lenses for Event
eventTimestamp :: Lens' Event Timestamp
eventTimestamp = lens eventTimestamp (\e ts -> e { eventTimestamp = ts })

eventService :: Lens' Event ServiceId
eventService = lens eventService (\e s -> e { eventService = s })

eventType :: Lens' Event EventType
eventType = lens eventType (\e t -> e { eventType = t })

eventSeverity :: Lens' Event Severity
eventSeverity = lens eventSeverity (\e s -> e { eventSeverity = s })

eventLatency :: Lens' Event (Maybe Latency)
eventLatency = lens eventLatency (\e l -> e { eventLatency = l })

eventErrorCode :: Lens' Event (Maybe ErrorCode)
eventErrorCode = lens eventErrorCode (\e ec -> e { eventErrorCode = ec })

eventMetadata :: Lens' Event (Map Text Text)
eventMetadata = lens eventMetadata (\e m -> e { eventMetadata = m }) 