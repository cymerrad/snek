{-# LANGUAGE NoImplicitPrelude #-}
module Types
  ( RA (..)
  , Options (..)
  ) where

import RIO
import RIO.Process

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  }

data RA = RA
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  -- Add other app-specific configuration information here
  }

instance HasLogFunc RA where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext RA where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })
