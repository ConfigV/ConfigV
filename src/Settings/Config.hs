module Settings.Config (
  module Settings.Options,
  module Settings.Thresholds,
  module Settings.Config) 
  where

import Settings.Options
import Settings.Thresholds

data ConfigVConfiguration = ConfigVConfiguration {
    optionsSettings :: Options
  , thresholdSettings :: RawThresholds
  }

