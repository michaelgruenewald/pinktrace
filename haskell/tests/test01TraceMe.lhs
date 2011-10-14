#!/usr/bin/env runhaskell

A simple test case, which tests whether 'traceMe' works fine.

> module Main (main) where
>
> import Data.Maybe
> import System.Exit
> import System.Posix.Process
> import System.PinkTrace.Trace
>
> main :: IO ()
> main = do
>   pid <- forkProcess child
>   status <- getProcessStatus True False pid
>   case fromJust status of
>       Stopped sig              -> putStrLn ("Child was stopped: " ++ (show sig)) >> exitFailure
>       Terminated sig           -> putStrLn ("Child was terminated: " ++ (show sig)) >> exitFailure
>       Exited (ExitFailure ret) -> putStrLn ("Invalid return: " ++ (show ret)) >> exitFailure
>       Exited ExitSuccess       -> return ()
>   where
>       child :: IO ()
>       child = do
>           traceMe
>           exitImmediately ExitSuccess
