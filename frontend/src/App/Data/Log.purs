-- | This module defines a type to represent well-formed logs which can be written to an external
-- | logging service. 
-- | 
-- | Our logs ought to have predictable metadata we can parse and use to gain insight into how our 
-- | service is being used and what failures have occurred. To ensure logs are always well-formed, 
-- | we'll enforce that only functions from this module can create the `Log` type by using the 
-- | smart constructor pattern. To learn more about this pattern, please see:
-- | https://thomashoneyman.com/guides/real-world-halogen/design-data-pure-functions/#restricting-the-domain-using-smart-constructors
-- |
-- | This module is rarely used in the rest of the application. It's a bit too low-level. In our
-- | business logic the critical thing is to report a particular error or message. We shouldn't have
-- | to care about how to format or gather metadata or the mechanics of sending the error to a
-- | particular reporting service. 
-- |
-- | The `App.Capability.LogMessages` module describes the higher-level interface to log an
-- | error or message that is used throughout the rest of the application. I'd recommend reading
-- | through that module as well.
module App.Data.Log 
  ( LogLevel(..)
  , message
  , reason
  , Log -- no constructors exported
  , mkLog
  ) where

import Prelude

import App.Capability.Now (class Now, nowDateTime)
import Data.DateTime (DateTime)
import Data.Either (either)
import Data.Formatter.DateTime (formatDateTime)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data LogLevel = Debug | Info | Warn | Error

derive instance genericLogLevel :: Generic LogLevel _
derive instance eqLogLevel :: Eq LogLevel
derive instance ordLogLevel :: Ord LogLevel

instance showLogLevel :: Show LogLevel where show = genericShow

-- | We have not created a newtype instance nor exported the `Log` constructor, 
-- | so this type cannot be created except by using functions in this module.
newtype Log = Log 
  { reason :: LogLevel 
  , timestamp :: DateTime
  , message :: String
  }

derive instance genericLog :: Generic Log _
derive instance eqLog :: Eq Log

-- | This helper function retrieves the well-formed message from a `Log`.
message :: Log -> String
message (Log { message: m }) = m

-- | This helper function retrieves the reason a log was produced from a `Log`.
reason :: Log -> LogLevel
reason (Log { reason: r }) = r

-- | This helper function retrieves the time a `Log` was produced.
timestamp :: Log -> DateTime
timestamp (Log { timestamp: t }) = t

mkLog :: ∀ m. Now m => LogLevel -> String -> m Log
mkLog logReason inputMessage = do
  now <- nowDateTime

  let 
    -- Will produce a header like "{DEBUG: 2018-10-25 11:25:29 AM]\nMessage contents..."
    headerWith start = 
      "[" <> start <> ": " <> formatTimestamp now <> "]\n" <> inputMessage

    -- Writes the header with the correct log reason  
    formattedLog = case logReason of
      Debug -> headerWith "DEBUG"
      Info -> headerWith "INFO"
      Warn -> headerWith "WARNING"
      Error -> headerWith "ERROR"
    
  pure $ Log { reason: logReason, timestamp: now, message: formattedLog }

  where
  -- Will format "2018-10-25 11:25:29 AM"
  formatTimestamp = either (const "(Failed to assign time)") identity 
    <<< formatDateTime "YYYY-DD-MM hh:mm:ss a"
