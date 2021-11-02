-- This calculates the next day of a given date
import System.Process ( system )
import Data.Char ( digitToInt, isDigit )

main :: IO ()
main = do system "cls"
          putStrLn "--------------------------------------------------------------"
          putStrLn "                         Date Calculator"
          putStrLn "--------------------------------------------------------------"
          putStrLn "Enter a date in dd/mm/yyyy format"
          input <- getLine
          let nextdate = getNextDate input
          putStrLn nextdate

getNextDate :: String -> String
getNextDate  [d1, d2, '/', m1, m2, '/', y1, y2, y3, y4] 
 | allDigits [d1, d2, m1, m2, y1, y2, y3, y4] = validate ((digitToInt d1 * 10) + digitToInt d2) ((digitToInt m1 * 10) + digitToInt m2) ((digitToInt y1 * 1000) + (digitToInt y2 * 100) + (digitToInt y3 * 10) + digitToInt y4)
 | otherwise                                  = "Non digit in date"
getNextDate x                                 = "Invalid date format"

validate :: Int -> Int -> Int -> String
validate day month year 
 | year < 1804 || year > 9999                                  = "Year must be higher than 1804 or lower than 10000"
 | month > 12  || month < 0                                    = "Year has 0 to 12 months"
 | month == 2 && day > 29 && leapYear year                     = "February only has 28 days in a non leap year"
 | month == 2 && day > 30 && not (leapYear year)               = "February only has 29 days in a leap year"
 | (30 + (month + month `div` 8) `mod` 2 == 30) && (day >= 31) = "Month only has 30 days"
 | (30 + (month + month `div` 8) `mod` 2 == 31) && (day >= 32) = "Month only has 31 days"
 | otherwise = getNextDay (day + 1) month year (leapYear year)

getNextDay :: Int -> Int -> Int -> Bool -> String
getNextDay day month year leapYear
 | month == 2 && ((leapYear && day == 30) || (not leapYear && day == 29))  = dateToString 1 (month + 1) year
 | (30 + (month + month `div` 8) `mod` 2 == 30) && day == 31               = dateToString 1 (month + 1) year
 | day == 32 && month == 12                                                = dateToString 1 1 (year + 1)
 | day == 32                                                               = dateToString 1 (month + 1) year
 | otherwise                                                               = dateToString day month year

dateToString :: Int -> Int -> Int -> String
dateToString day month year = show day ++ "/" ++ show month ++ "/" ++ show year

allDigits :: [Char] -> Bool
allDigits [] = True
allDigits (h:t)
 | isDigit h = allDigits t
 | otherwise = False

leapYear :: Int -> Bool
leapYear year
 | year `mod` 4 == 0 && year `mod` 100 > 0 || year `mod` 400 == 0 = True
 | otherwise                                                      = False