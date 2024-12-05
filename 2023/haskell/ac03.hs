import Data.Char
import Data.List
import qualified Data.Map as Map

cut p list =
  let (hd, _:tl) = break p list
  in (hd, tl)

split p list =
  let (hd, tl) = break p list
  in case tl of
    [] -> [hd]
    _:tl -> hd:split p tl

checkSymbol c =
  not (isDigit c) && c /= '.'

findSymbols line =
  filter (checkSymbol . snd) line

parseNumber list =
  let (x, _c):_tl = list
  in (x, (length list, read (map snd list)))

findNumbers =
  map parseNumber . filter (/= []) . split (not . isDigit . snd)

getNumber (_x, (_len, n)) = n

isMarked (current, (previous, next)) (x, (len, _n)) =
  any (flip Map.member previous) [x - 1 .. x + len] ||
  Map.member (x - 1) current || Map.member (x + len) current ||
  any (flip Map.member next) [x - 1 .. x + len]

numberIsNear gx (nx, (len, _n)) =
  not (nx > gx + 1 || nx + len - 1 < gx - 1)

getGearRatio (current, (previous, next)) (x, c) =
  if c == '*' then
    case filter (numberIsNear x) (previous ++ current ++ next) of
      [(_, (_, m)), (_, (_, n))] -> m * n
      _ -> 0
  else
    0

neighborhood empty list =
  let _:tl = list in
  zip list (zip (empty:list) (tl ++ [empty]))

getGearRatios (neighborhood, columns) =
  map (getGearRatio neighborhood) columns

getMarkedNumbers (neighborhood, columns) =
  map getNumber (filter (isMarked neighborhood) columns)

main = do
  input <- getContents
  let input_lines = lines input
  let lines = map (zip [0..]) input_lines
  let numbers = map findNumbers lines
  let symbols = map findSymbols lines
  let symbols_maps = map Map.fromList symbols
  let symbols_n = neighborhood Map.empty symbols_maps
  let marked_numbers = map getMarkedNumbers (zip symbols_n numbers)
  let result_part1 = sum (concat marked_numbers)
  putStrLn ("Part 1: " ++ show (result_part1))
  let numbers_maps = neighborhood [] numbers
  let gear_ratios = map getGearRatios (zip numbers_maps symbols)
  let result_part2 = sum (concat gear_ratios)
  putStrLn ("Part 2: " ++ show (result_part2))
