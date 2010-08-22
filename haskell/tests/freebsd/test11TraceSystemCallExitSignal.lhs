#!/usr/bin/env runhaskell
vim: set ft=lhaskell et ts=4 sts=4 sw=4 fdm=marker :

A simple test case, which tests whether 'traceSystemCallExit' works correctly
for sending signals.

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
>   traceSystemCallExit pid killProcess
>   status' <- getProcessStatus True False pid
>   case fromJust status' of
>       Exited (ExitFailure ret) -> putStrLn ("Child exited with code: " ++ (show ret)) >> exitFailure
>       Exited ExitSuccess       -> putStrLn ("Child exited with code: 0") >> exitFailure
>       Stopped sig              -> putStrLn ("Child was stopped: " ++ (show sig)) >> exitFailure
>       Terminated sig           -> unless (sig == killProcess)
>                                       (putStrLn ("Wrong signal: " ++ show sig) >> exitFailure)
>   where
>       child :: IO ()
>       child = do
>           traceMe
>           raiseSignal softwareStop
>           exitImmediately ExitSuccess
