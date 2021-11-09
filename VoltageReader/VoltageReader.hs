import System.Process

-- Threshholds data type
data Threshholds = Threshholds { 
                                 average :: Float,
                                 typeOne :: Float,
                                 typeTwo :: Float 
                               }

-- Errors data type
data Error = 
  TypeOne Int Float Float | TypeTwo Int Int Float | TypeThree

showError :: Error -> String
showError (TypeOne v1 v2 v3) = "Voltage at hour " ++ show v1 ++ " was " ++ show v2 ++ " (difference of " ++ show v3 ++ ")"
showError (TypeTwo v1 v2 v3) = "Voltage change from hour " ++ show v1 ++ " to hour " ++ show v2 ++ " was " ++ show v3
showError TypeThree          = "No input voltages"

-- Main
-- This program reads in a list of voltages and error checks the readings
main :: IO ()
main = do system "cls"
          putStrLn "--------------------------------------------------------------"
          putStrLn "                         Voltage Reader"
          putStrLn "--------------------------------------------------------------"
          putStrLn "Reading Voltages"
          voltages <- readVoltages 
          putStrLn "Calculating Voltages"
          let voltOut = calculate 1 voltages (threshholds voltages)
           in  output voltOut

-- Reads voltages too a list of floats from an output file
readVoltages :: IO [Float]
readVoltages = do voltages <- readFile "Voltages.txt"
                  return $ map (read :: String -> Float) . lines $ voltages

-- Reads error to screen
output :: [Error] -> IO ()
output []  = putStrLn "No errors detected"
output [x] = putStrLn $ showError x
output (h:t) = do putStrLn $ showError h
                  output t

-- Pure functions
-- Calculate threshholds
threshholds :: [Float] -> Threshholds
threshholds list = let avg = listAverage list
                    in Threshholds avg (avg * 0.1) (avg * 0.15)

-- Calculate errors
calculate :: Int -> [Float] -> Threshholds -> [Error]
calculate _ [] _              = [TypeThree]                  
calculate hour [voltage] (Threshholds avg t1 t2)
 | diff avg voltage > t1      = [TypeOne hour voltage (diff avg voltage)]
 | otherwise                  = []
calculate hour (voltage : t) (Threshholds avg t1 t2)
 | diff avg voltage > t1      = TypeOne hour voltage (diff avg voltage) : calculate (hour+1) t (Threshholds avg t1 t2)
 | diff voltage (head t) > t2 = TypeTwo hour (hour + 1) (diff voltage (head t)) : calculate (hour+1) t (Threshholds avg t1 t2)
 | otherwise                  = calculate (hour+1) t (Threshholds avg t1 t2)

-- Get the average of the list
listAverage :: [Float] -> Float
listAverage []   = 0
listAverage list = sum list / fromIntegral (length list) :: Float

-- Get difference between two floats
diff :: Float -> Float -> Float
diff x y = abs x - y