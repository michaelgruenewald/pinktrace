-- A simple example showing how to do tracing fork on Linux

import Data.Maybe
import Control.Monad
import System.Exit
import System.Posix.Process (forkProcess)
import System.Posix.Signals
import System.PinkTrace.Event
import System.PinkTrace.Trace

main :: IO ()
main = do
    pid <- forkProcess child
    -- Note this 'getProcessStatus' function is from the System.PinkTrace.Event
    -- module. It's a simple re-implementation of 'getProcessStatus' function
    -- of the System.Posix.Process module which gives information about ptrace
    -- events.
    status <- getProcessStatus True False pid
    case fromJust status of
        Exited (ExitFailure ret) -> putStrLn ("Child exited with code: " ++ (show ret)) >> exitFailure
        Exited ExitSuccess       -> putStrLn ("Child exited with code: 0") >> exitFailure
        Terminated sig           -> putStrLn ("Child was terminated: " ++ (show sig)) >> exitFailure
        StoppedTrace ev          -> putStrLn ("Child has done an unexpected ptrace event " ++ (show ev)) >> exitFailure
        Stopped sig              -> unless (sig == softwareStop)
                                        (putStrLn ("Wrong signal: " ++ show sig) >> exitFailure)

    -- Setup the child
    traceSetup opt pid

    -- Nothing interesting in this example
    -- Let the child continue its execution.
    traceContinue 1 nullSignal pid
    where
        opt :: TraceOption
        opt = TraceOption { traceOptionSysGood   = True
                          , traceOptionFork      = False
                          , traceOptionVFork     = False
                          , traceOptionClone     = False
                          , traceOptionExec      = False
                          , traceOptionVForkDone = False
                          , traceOptionExit      = False
                          }
        child :: IO ()
        child = do
            traceMe
            raiseSignal softwareStop
            putStrLn "hello world"
