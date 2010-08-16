#!/usr/bin/env runhaskell
vim: set ft=lhaskell et ts=4 sts=4 sw=4 fdm=marker :

> module Main (main) where
>
> import Data.Maybe
> import Control.Monad
> import System.Exit
> import System.Posix.Process
> import System.Posix.Signals
> import System.PinkTrace.Bitness
> import System.PinkTrace.Trace
>
> main :: IO ()
> main = do
>   pid <- forkProcess child
>   status <- getProcessStatus True False pid
>   sig <- case fromJust status of
>           Exited _ -> putStrLn "Child has exited abnormally!" >> exitFailure
>           Terminated _ -> putStrLn "Child was terminated!" >> exitFailure
>           Stopped s -> return s
>   when (sig /= sigSTOP) (putStrLn ("Invalid signal " ++ show sig) >> exitFailure)
>   bit <- getBitness pid
>   when (bit /= bitnessDefault) (putStrLn ("Invalid bitness " ++ show bit) >> exitFailure)
>   where
>       child :: IO ()
>       child = do
>           traceMe
>           raiseSignal sigSTOP
>           exitImmediately $ ExitFailure 13
