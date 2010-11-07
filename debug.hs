{-
 - debug.hs
 -
 - provides some basic print debug mechanism
 -

to use this file, write program/module like below

> {-# LANGUAGE CPP #-}
> 
> module MyModule where
> 
> import SomeModule
> import OtherModule
> 
> #include "debug.hs"
> 
> main :: IO ()
> main = do
>     putStrLn "hello"
>     debug "debug message"
>     putStrLn "world"

and compile with "-DDEBUG" to enable debug print

 -}

#ifdef DEBUG
#define debug debug_(__FILE__)(__LINE__)

import Control.Monad.Trans (MonadIO, liftIO)
import Data.Time (getCurrentTime)

debug_ :: (MonadIO m, Show a) => String -> Int -> a -> m ()
debug_ file line x = do
    now <- liftIO getCurrentTime
    liftIO $ putStrLn $ show now ++ ":" ++ file ++ ":" ++ show line ++ ": " ++ show x

#else
debug :: (Monad m, Show a) => a -> m ()
debug = const $ return ()
#endif
