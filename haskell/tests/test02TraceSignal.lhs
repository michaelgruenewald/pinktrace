#!/usr/bin/env runhaskell

A simple test case, which tests whether signals work for the traced child.

> module Main (main) where
>
> import Control.Monad
> import Data.Maybe
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
>       Stopped sig              -> unless (sig == backgroundRead)
>                                       (putStrLn ("Wrong signal: " ++ show sig) >> exitFailure)
>   signalProcess continueProcess pid
>   where
>       child :: IO ()
>       child = do
>           traceMe
>           raiseSignal backgroundRead
>           exitImmediately ExitSuccess
