-- A simple example showing how to use PinkTrace version macros
-- vim: set ft=haskell et ts=4 sts=4 sw=4 fdm=marker :

import System.PinkTrace

main :: IO ()
main = do
    putStr ( "Built using "
            ++ pinkPackage ++ " "
            ++ (show pinkVersionMajor) ++ "."
            ++ (show pinkVersionMinor) ++ "."
            ++ (show pinkVersionMicro)
            ++ pinkVersionSuffix
           )
    if pinkGitHead /= ""
        then putStrLn (" git-" ++ pinkGitHead)
        else putStrLn ""
