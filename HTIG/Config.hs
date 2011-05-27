{-# LANGUAGE OverloadedStrings #-}
module HTIG.Config
    ( defaultConfig
    ) where

import HTIG.Core
import HTIG.Action
import HTIG.Channel.Timeline
import HTIG.Channel.Mention
import HTIG.StatusHook
import HTIG.Hooks.ShowTimeDiff
import HTIG.Hooks.Tid
import HTIG.Utils
import qualified Data.Map as Map

myStatusHook = composeAll
    [ doUnHtmlEscape
    , doUnHtmlEntityRef
    , doShowReply
    , doShowTimeDiff
    , doShowTid
    , doShowRT
    ]

myChannels =
    [ ((== "#timeline"), timeline)
    , ((== "#mention"), mention)
    --, ((=~ "^#"), Search)
    --, ((const True), DM)
    ]

myActions =
    [ ("user",    userAction)

    , ("reply",   replyAction)
    , ("rp",      replyAction)

    , ("retweet", retweetAction)
    , ("rt",      retweetAction)
    ]

mySessionInitHook = do
    createTidTable
    joinTo "#timeline"
    joinTo "#mention"
    --forkDMFetcher

defaultConfig :: HConfig
defaultConfig = HConfig
    { listenAddr      = "127.0.0.1"
    , listenPort      = 16668
    , sessionInitHook = mySessionInitHook
    , channels        = myChannels
    , actions         = myActions
    , statusHook      = myStatusHook
    , errorMessage    = Nothing
    }
