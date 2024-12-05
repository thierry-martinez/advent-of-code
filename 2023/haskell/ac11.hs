import Data.Array
import Data.List

distances galaxies emptyRows emptyColumns factor =
    sum [ sum [ abs (x1 - x2) + abs (y1 - y2) +
                factor * (abs (emptyRows ! y1 - emptyRows ! y2) +
                         abs (emptyColumns ! x1 - emptyColumns ! x2))
              | (x2, y2) <- ts ]
          | ((x1, y1):ts) <- tails galaxies ]

main = do
    contents <- getContents
    let grid = map (map (== '#')) (lines contents)
    let first:_ = grid
    let width = length first
    let emptyRows =
          listArray (0, length grid)
          (scanl1 (+) (map (fromEnum . all not) grid))
    let emptyColumns =
          listArray (0, width)
          (scanl1 (+) (map (fromEnum . all not) (transpose grid)))
    let galaxies = [
          (x, y) | (y, row) <- zip [0..] grid, (x, True) <- zip [0..] row]
    let resultPart1 = distances galaxies emptyRows emptyColumns 1
    putStrLn("Part 1: " ++ show resultPart1)
    let resultPart2 = distances galaxies emptyRows emptyColumns (1000000 - 1)
    putStrLn("Part 2: " ++ show resultPart2)
