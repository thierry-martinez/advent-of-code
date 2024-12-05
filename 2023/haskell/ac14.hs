import Data.List
import qualified Data.Map as Map

tiltColumn accu [] = accu
tiltColumn accu ('.':tl) = '.':tiltColumn accu tl
tiltColumn accu ('#':tl) = accu ++ '#':tiltColumn [] tl
tiltColumn accu ('O':tl) = tiltColumn ('O':accu) tl

tiltEast grid =
  map (tiltColumn []) grid

tiltWest grid =
  map (reverse . tiltColumn [] .reverse) grid

tiltNorth grid =
  transpose (tiltWest (transpose grid))

tiltSouth grid =
  transpose (tiltEast (transpose grid))

tiltCycle =
  tiltEast . tiltSouth . tiltWest . tiltNorth

countLine (i, line) =
  length (filter (== 'O') line) * i

amountOfLoad grid =
  sum (map countLine (zip [1..] (reverse grid)))

measureCycleLoop map seq count grid =
  case Map.lookup grid map of
    Just index -> (reverse seq, index, count)
    Nothing ->
      let map' = Map.insert grid count map
      in measureCycleLoop map' (grid:seq) (count + 1) (tiltCycle grid)

measureCycle grid =
  measureCycleLoop Map.empty [] 0 grid

iterateTiltCycle grid count =
  let (seq, initial, count') = measureCycle grid
  in if count < count' then
    seq!!count
  else
    let left = (count - count') `mod` (count' - initial)
    in seq!!(initial + left)
  
main = do
  contents <- getContents
  let grid = lines contents
  let result_part1 = amountOfLoad (tiltNorth grid)
  putStrLn("Part 1: " ++ show result_part1)
  let result_part2 = amountOfLoad (iterateTiltCycle grid 1000000000)
  putStrLn("Part 2: " ++ show result_part2)
