import Data.List
import Data.Maybe

cut p list =
  let (hd, (_:tl)) = break p list
  in (hd, tl)

split p list =
  let (hd, tl) = break p list
  in case tl of
    [] -> [hd]
    _:tl -> hd:split p tl

derive [] = []
derive [_] = []
derive (a : tl@(b : _)) = b - a : derive tl

extrapolate side op numbers =
  if all (( == ) 0) numbers then
    0
  else
    op (side numbers) (extrapolate side op (derive numbers))

solve side op numbers =
  sum (map (extrapolate side op) numbers)

parse line =
  map read (split (== ' ') line)

first (hd:_) = hd

main = do
  input <- getContents
  let numbers = map parse (lines input)
  let result_part1 = solve last ( + ) numbers
  putStrLn ("Part 1: " ++ show result_part1)
  let result_part2 = solve first ( - ) numbers
  putStrLn ("Part 2: " ++ show result_part2)
