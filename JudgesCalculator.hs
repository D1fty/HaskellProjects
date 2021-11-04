import System.Process (system)
import Numeric
import Data.Char

-- Main
-- This program reads in a number of judges and a score for each judge
-- It then outputs the following: Largest Score, Lowest Score and the Score Average
main :: IO ()
main = do system "cls"
          putStrLn "--------------------------------------------------------------"
          putStrLn "                        Judges Calculator"
          putStrLn "--------------------------------------------------------------"
          putStrLn "Enter the number of judges: "
          input    <- getLine
          putStrLn ""
          putStrLn "Enter a score for each judge: "
          scores   <- getScores (read input :: Int)
          putStrLn ""
          showScores  scores    (read input :: Int)

-- Get the scores
getScores :: Int -> IO [Float]
getScores 0 = return []
getScores x = do input <- getLine
                 y     <- getScores (x-1)
                 return ((read input :: Float) : y)

-- Show the scores
showScores :: [Float] -> Int -> IO ()
showScores [] _   = return ()
showScores list x = do putStr "Highest score: "
                       print $ maximum list
                       putStr "Lowest score:  "
                       print $ minimum list
                       putStr "Final average score: "
                       print $ average (removeValues list)

-- Pure functions
-- Remove min and max from the list
removeValues :: [Float] -> [Float]
removeValues list = removeValue (removeValue  list (minimum list)) (maximum list)

-- Remove one occurance from a value in the list
removeValue :: [Float] -> Float -> [Float]
removeValue [] _ = []
removeValue (h:t) x
 | h == x = t
 | otherwise = h : removeValue t x

-- Get the average of the list
average :: [Float] -> Float
average []   = 0
average list = foldr (+) 0 list / fromIntegral (length list) :: Float