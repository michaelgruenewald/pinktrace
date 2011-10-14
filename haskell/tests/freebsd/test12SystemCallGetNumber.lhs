#!/usr/bin/env runhaskell

Check whether 'getSystemCallNumber' works correctly.

> module Main (main) where
>
> import Data.Maybe
> import Control.Monad
> import System.Exit
> import System.Posix.Process
> import System.Posix.Signals
> import System.PinkTrace.Bitness
> import System.PinkTrace.SystemCall
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
>   sno   <- getSystemCallNumber bitnessDefault pid
>   sname <- nameSystemCall bitnessDefault sno
>   case sname of
>       Nothing   -> putStrLn ("Wrong system call: " ++ show sno) >> exitFailure
>       Just name -> (unless (name == "getpid"))
>                       (putStrLn ("Wrong system call: " ++ show sno) >> exitFailure)
>   traceKill pid
>   where
>       child :: IO ()
>       child = do
>           traceMe
>           raiseSignal softwareStop
>           getProcessID >> exitImmediately ExitSuccess
