import Data.List
import Data.Char
import Data.Maybe
import qualified Data.Map as Map

parseLine line =
  let subst c = if c == '=' || c == '(' || c == ',' || c == ')' then ' ' else c
      [key, left, right] = words (map subst line)
  in (key, (left, right))

parse input =
  let (instructions : _empty : rest) = input
  in (instructions, Map.fromList (map parseLine rest))

follow direction m place =
  let (left, right) = fromJust (Map.lookup place m)
  in case direction of
       'L' -> left
       'R' -> right
       _   -> error "Invalid direction"

followPath m end stepCount place ((_, direction):rest) =
  if end place then stepCount
  else followPath m end (stepCount + 1) (follow direction m place) rest

pathLength instructions m end initialPlace =
  followPath m end 0 initialPlace (zip [0..] (cycle instructions))

part1 instructions m =
  pathLength instructions m (== "ZZZ") "AAA"

part2 instructions m =
  let places = filter ((== 'A') . last) (map fst (Map.toList m))
      lengths = map (pathLength instructions m ((== 'Z') . last)) places
  in foldl1 lcm lengths

main = do
  input <- getContents
  let (instructions, m) = parse (lines input)
  let result_part1 = part1 instructions m
  putStrLn ("Part 1: " ++ show result_part1)
  let result_part2 = part2 instructions m
  putStrLn ("Part 2: " ++ show result_part2)
