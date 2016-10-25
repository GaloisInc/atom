-- |
-- Module: Compile
-- Description: Compilation functions
-- Copyright: (c) 2013 Tom Hawkins & Lee Pike
--
-- Atom compilation functions

module Language.Atom.Compile
  ( compile
  , CompileResult(..)
  , reportSchedule
  , Schedule
  ) where

import System.Exit
import Control.Monad (when)
import Data.Maybe (isJust)

import Language.Atom.Channel.Types
import Language.Atom.Code
import Language.Atom.Scheduling
import Language.Atom.Elaboration
import Language.Atom.UeMap (emptyMap)
import Language.Atom.Language hiding (Atom)


-- | Package of all the compilation results
data CompileResult = CompileResult
  { compSchedule    :: Schedule        -- ^ schedule computed by the compiler
  , compCoverage    :: RuleCoverage    -- ^ rule coverage
  , compChans       :: [ChanInput]     -- ^ channels used in the system
  , compAssertNames :: [Name]          -- ^ assertion statement names
  , compCoverNames  :: [Name]          -- ^ coverage statement names
  , compProbes      :: [(Name, Type)]  -- ^ declared probe names and types
  }

-- | Compiles an atom description to C.
compile :: Name
        -> Config
        -> Atom ()
        -> IO CompileResult
compile name config atom' = do
  res <- elaborate emptyMap name atom'
  case res of
   Nothing -> putStrLn "ERROR: Design rule checks failed." >>
              exitWith (ExitFailure 1)
   Just (umap, (state, rules, chanIns, assertionNames, coverageNames, probeNames)) -> do
     -- main code generation step
     let sch = schedule rules umap
     ruleCoverage <- writeC name config state rules sch assertionNames
                     coverageNames probeNames

     when (isJust $ hardwareClock config) (putStrLn hwClockWarning)
     return $ CompileResult sch ruleCoverage chanIns assertionNames
                            coverageNames probeNames

hwClockWarning :: String
hwClockWarning = unlines
 [ ""
 , "*** Atom WARNING: you are configuring to use a hardware clock.  Please remember"
 , "    to set the \"__phase_start_time\" variable to the time at which the first"
 , "    phase should be run before you enter the main Atom-generated function the"
 , "    first time."
 ]
