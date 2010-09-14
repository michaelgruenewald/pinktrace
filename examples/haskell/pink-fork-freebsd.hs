-- A simple example showing how to do the tracing fork on FreeBSD
-- vim: set ft=haskell et ts=4 sts=4 sw=4 fdm=marker :

import Data.Maybe
import Control.Monad
import System.Exit
import System.Posix.Process
import System.Posix.Signals
import System.PinkTrace.Trace

main :: IO ()
main = do
    pid <- forkProcess child
    status <- getProcessStatus True False pid
    case fromJust status of
        Exited (ExitFailure ret) -> putStrLn ("Child exited with code: " ++ (show ret)) >> exitFailure
        Exited ExitSuccess       -> putStrLn ("Child exited with code: 0") >> exitFailure
        Terminated sig           -> putStrLn ("Child was terminated: " ++ (show sig)) >> exitFailure
        Stopped sig              -> unless (sig == softwareStop)
                                        (putStrLn ("Wrong signal: " ++ show sig) >> exitFailure)

    -- Nothing interesting in this example.
    -- Let the child continue its execution.
    traceContinue 1 nullSignal pid
    where
        child :: IO ()
        child = do
            traceMe
            raiseSignal softwareStop
            putStrLn "hello world"
