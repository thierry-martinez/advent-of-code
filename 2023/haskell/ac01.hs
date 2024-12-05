import Data.Char
import Data.List
import Data.Maybe

firstDigit str =
  digitToInt (fromJust (find isDigit str))

lastDigit str =
  digitToInt (fromJust (find isDigit (reverse str)))

part1 input_lines =
  sum (map (\line -> firstDigit line * 10 + lastDigit line) input_lines)

englishDigits =
  map (\(digit :: Int) -> (show digit, digit)) [0 .. 9] ++
  [("zero", 0), ("one", 1), ("two", 2), ("three", 3), ("four", 4), ("five", 5),
   ("six", 6), ("seven", 7), ("eight", 8), ("nine", 9)]

englishDigitPrefixToInt str =
  listToMaybe (catMaybes (map (\(text, value) ->
    if isPrefixOf text str then Just value else Nothing
  ) englishDigits))

findEnglishDigit list =
  let digit:_ = catMaybes (map englishDigitPrefixToInt list)
  in digit

firstEnglishDigit str =
  findEnglishDigit (tails str)

lastEnglishDigit str =
  findEnglishDigit (reverse (tails str))

part2 input_lines =
  sum (map (\line ->
    firstEnglishDigit line * 10 + lastEnglishDigit line
  ) input_lines)

main = do
  input <- getContents
  let input_lines = lines input
  putStrLn ("Part 1: " ++ show (part1 input_lines))
  putStrLn ("Part 2: " ++ show (part2 input_lines))
  
