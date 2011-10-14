#!/usr/bin/env runhaskell

A simple test case, which tests whether 'traceSystemCall' works correctly.

> module Main (main) where
>
> import Data.Maybe
> import Control.Monad
> import System.Exit
> import System.Posix.Process
> import System.Posix.Signals
> import System.PinkTrace.Trace
>
> main :: IO ()
> main = do
>   pid <- forkProcess child
>   status <- getProcessStatus True False pid
>   case fromJust status of
>       Exited (ExitFailure ret) -> putStrLn ("Child exited with code: " ++ (show ret)) >> exitFailure
>       Exited ExitSuccess       -> putStrLn ("Child exited with code: 0") >> exitFailure
>       Terminated sig           -> putStrLn ("Child was terminated: " ++ (show sig)) >> exitFailure
>       Stopped sig              -> unless (sig == softwareStop)
>                                       (putStrLn ("Wrong signal: " ++ show sig) >> exitFailure)
>
>   traceSystemCall nullSignal pid
>   status' <- getProcessStatus True False pid
>   case fromJust status' of
>       Exited (ExitFailure ret) -> putStrLn ("Child exited with code: " ++ (show ret)) >> exitFailure
>       Exited ExitSuccess       -> putStrLn ("Child exited with code: 0") >> exitFailure
>       Terminated sig           -> putStrLn ("Child was terminated: " ++ (show sig)) >> exitFailure
>       Stopped sig              -> unless (sig == breakpointTrap)
>                                       (putStrLn ("Wrong signal: " ++ show sig) >> exitFailure)
>
>   signalProcess killProcess pid
>   where
>       child :: IO ()
>       child = do
>           traceMe
>           raiseSignal softwareStop
>           exitImmediately ExitSuccess
