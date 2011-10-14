-- A simple example showing how to use PinkTrace version macros

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
