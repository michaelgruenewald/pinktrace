#!/usr/bin/env runhaskell

A simple test case, which tests whether 'traceContinue' works correctly.

> module Main (main) where
>
> import Data.Maybe
> import Control.Monad
> import System.Exit
> import System.Posix.Process (exitImmediately, forkProcess)
> import System.Posix.Signals
> import System.PinkTrace.Event
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
>       StoppedTrace ev          -> putStrLn ("Child has done an unexpected ptrace event " ++ (show ev)) >> exitFailure
>       Stopped sig              -> unless (sig == softwareStop)
>                                       (putStrLn ("Wrong signal: " ++ show sig) >> exitFailure)
>
>   traceContinue 1 nullSignal pid
>   status' <- getProcessStatus True False pid
>   case fromJust status' of
>       Stopped sig              -> putStrLn ("Child was stopped: " ++ (show sig)) >> exitFailure
>       StoppedTrace ev          -> putStrLn ("Child has done an unexpected ptrace event " ++ (show ev)) >> exitFailure
>       Terminated sig           -> putStrLn ("Child was terminated: " ++ (show sig)) >> exitFailure
>       Exited (ExitFailure ret) -> putStrLn ("Invalid return: " ++ (show ret)) >> exitFailure
>       Exited ExitSuccess       -> return ()
>   where
>       child :: IO ()
>       child = do
>           traceMe
>           raiseSignal softwareStop
>           exitImmediately ExitSuccess
