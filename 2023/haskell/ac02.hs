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

parseColor (count, color) =
  (color, read count)

parseGame line =
  let (game_id, rest) = cut (== ':') line
      id = read (drop (length "Game ") game_id)
      sets = map (dropWhile isSpace) (split (== ';') rest)
      colors = map (
        map (parseColor . cut (== ' ') . dropWhile isSpace) . split (== ',')
        ) sets
  in (id, colors)

checkColor (color, count) =
  case color of
    "red" -> count < 12
    "green" -> count < 13
    "blue" -> count < 14
    _ -> True

checkLine (_id, sets) =
  all (all checkColor) sets

part1 games =
  sum (map fst (filter checkLine games))

power (_id, sets) =
  product (map snd (Map.toList (Map.unionsWith max (map Map.fromList sets))))

part2 games = 
  sum (map power games)

main = do
  input <- getContents
  let input_lines = lines input
  let games = map parseGame input_lines
  putStrLn ("Part 1: " ++ show (part1 games))
  putStrLn ("Part 2: " ++ show (part2 games))
