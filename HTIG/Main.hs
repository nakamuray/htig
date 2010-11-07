module HTIG.Main
    ( htig
    ) where

import Control.Applicative ((<$>))
import Data.Time (getCurrentTime)
import System.FilePath ((</>))
import System.IO (stderr, hPutStrLn)

import qualified Config.Dyre as Dyre

import HTIG.Core
import HTIG.Config
import HTIG.IRCServer
import HTIG.Session
import HTIG.Utils

showError :: HConfig -> String -> HConfig
showError cfg e = cfg { errorMessage = Just e }

realMain :: HConfig -> IO ()
realMain cfg = do
    case errorMessage cfg of
        Just e  -> hPutStrLn stderr $ "Error: " ++ e
        Nothing -> return ()
    now <- getCurrentTime
    let h = listenAddr cfg
        p = listenPort cfg
        g = GS (b h) now cfg []
        s = Server h p (return g) (return newEmptyState) (return . const HSession)
    runServer s

htig :: HConfig -> IO ()
htig = Dyre.wrapMain $ Dyre.defaultParams
    { Dyre.projectName = s appName
    , Dyre.configDir   = Just getHTIGDir
    , Dyre.cacheDir    = Just ((</> "build") <$> getHTIGDir)
    , Dyre.realMain    = realMain
    , Dyre.showError   = showError
    , Dyre.hidePackages = ["monads-fd", "monads-tf"]
    -- "-i" is relative path from "~/.htig/build"
    , Dyre.ghcOpts     = ["-O2", "-i../lib"]
    }
