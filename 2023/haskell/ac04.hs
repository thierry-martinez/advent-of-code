import Data.Char
import Data.List
import Data.Bits
import qualified Data.Set as Set

cut p list =
  let (hd, _:tl) = break p list
  in (hd, tl)

split p list =
  let (hd, tl) = break p list
  in case tl of
    [] -> [hd]
    _:tl -> hd:split p tl

split_numbers = map read . filter (/= []) . split (== ' ')

parseLine line =
  let (_, contents) = cut (== ':') line
      (winning, cards) = cut (== '|') (dropWhile isSpace contents)
      winning_numbers :: [Int] = split_numbers (dropWhileEnd isSpace winning)
      card_numbers :: [Int] = split_numbers (dropWhile isSpace cards)
      winning_set = Set.fromList winning_numbers
  in length (filter (flip Set.member winning_set) card_numbers)

scoreGame :: Int -> Int
scoreGame winningCount =
  if winningCount == 0 then
    0
  else
    shiftL 1 (winningCount - 1)

countCards total copies winningCounts =
  case (winningCounts, copies) of
    ([], _) -> total
    (hd:tl, copies:copies_tail) ->
      let list = map (+ copies) (take hd copies_tail) ++ drop hd copies_tail
      in countCards (total + copies) list tl

main = do
  input <- getContents
  let input_lines = lines input
  let winningCounts = map parseLine input_lines
  let result_part1 = sum (map scoreGame winningCounts)
  let result_part2 = countCards 0 (repeat 1) winningCounts
  putStrLn ("Part 1: " ++ show result_part1)
  putStrLn ("Part 2: " ++ show result_part2)
