module HTIG.Main
    ( htig
    ) where

import Control.Applicative ((<$>))
import Data.Time (getCurrentTime)
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
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

type Params = Dyre.Params HConfig

defaultParams :: Params
defaultParams = Dyre.defaultParams
        { Dyre.projectName = s appName
        , Dyre.configDir   = Just getHTIGDir
        , Dyre.cacheDir    = Just ((</> "build") <$> getHTIGDir)
        , Dyre.realMain    = realMain
        , Dyre.showError   = showError
        -- "-i" is relative path from "~/.htig/build"
        , Dyre.ghcOpts     = ["-O2", "-i../lib"]
        }

parseArgs :: IO Params
parseArgs = parseArgs' defaultParams =<< getArgs

parseArgs' :: Params -> [String] -> IO Params
parseArgs' p []                      = return p
parseArgs' _ ("--help":_)            = printHelp >> exitSuccess
parseArgs' _ ("--version":_)         = putStrLn (s appName ++ "-" ++ s appVersion) >> exitSuccess
parseArgs' p ("--force-reconf":args) = parseArgs' p { Dyre.forceRecomp = True } args
parseArgs' p ("--":args)             = return p { Dyre.ghcOpts = Dyre.ghcOpts p ++ args }
parseArgs' _ (arg:_)                 = hPutStrLn stderr ("Error: unknown option: " ++ arg) >> exitFailure

printHelp :: IO ()
printHelp = putStrLn $ unlines $
    [ "Usage: " ++ s appName ++ " [OPTION] [-- GHC Options]"
    , "Options:"
    , "  --help            print this message"
    , "  --version         print the version number"
    , "  --force-reconf    force recompile your ~/.htig/htig.hs"
    ]

htig :: HConfig -> IO ()
htig cfg = do
    params <- parseArgs
    Dyre.wrapMain params cfg
