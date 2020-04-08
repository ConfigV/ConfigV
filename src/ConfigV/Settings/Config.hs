module ConfigV.Settings.Config (
  module ConfigV.Settings.Options,
  module ConfigV.Settings.Thresholds,
  module ConfigV.Settings.Config) 
  where

import ConfigV.Settings.Options
import ConfigV.Settings.Thresholds


data ConfigVConfiguration = ConfigVConfiguration {
    optionsSettings :: Options
  , thresholdSettings :: RawThresholds
  }

