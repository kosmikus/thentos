module Thentos.Adhocracy3 (main) where

import Thentos (makeMain)

import qualified Thentos.Adhocracy3.Backend.Api.Simple as Simple (runBackend)


-- * main

main :: IO ()
main = makeMain $ \actionState mBeConfig _ -> do
    maybe (error "command `run` requires backend")
        (`Simple.runBackend` actionState)
        mBeConfig
