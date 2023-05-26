{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import Import
import UI (loop)

run :: RIO App ()
run = do
  -- logInfo "We're inside the application!"
  liftIO loop
