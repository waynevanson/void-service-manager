module VSM.Configuration where

import System.FilePath (FilePath)

data Configuration = Configuration
  { force :: Bool,
    serviceDir :: FilePath,
    runSvDir :: FilePath
  }

defaultConfiguration :: Configuration
defaultConfiguration =
  Configuration
    { force = False,
      serviceDir = "/etc/sv",
      runSvDir = "/var/service"
    }