-- A simple example showing how to use PinkTrace version macros
-- vim: set ft=haskell et ts=4 sts=4 sw=4 fdm=marker :

import qualified System.PinkTrace as PinkTrace

main :: IO ()
main = do
    putStr ( "Built using "
            ++ PinkTrace.package ++ " "
            ++ (show PinkTrace.versionMajor) ++ "."
            ++ (show PinkTrace.versionMinor) ++ "."
            ++ (show PinkTrace.versionMicro)
            ++ PinkTrace.versionSuffix
           )
    if PinkTrace.gitHead /= ""
        then putStrLn (" git-" ++ PinkTrace.gitHead)
        else putStrLn ""
