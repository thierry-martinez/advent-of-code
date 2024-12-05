import Data.Array
import Data.List
import qualified Data.Set as Set
import Debug.Trace

nextVisit (x, y) (dx, dy) =
  ((x + dx, y + dy), (dx, dy))

countEnergizedLoop grid beam ((visited, energized), count) =
  let (p@(x, y), (dx, dy)) = beam
  in if x < 0 || y < 0 || x > snd (snd (bounds grid)) || y > fst (snd (bounds grid)) then
       ((visited, energized), count)
     else
       if Set.member beam visited then
         ((visited, energized), count)
       else
         let visited' = Set.insert beam visited
             (energized', count') =
               if energized!(y, x) then
                 (energized, count)
               else
                 (energized//[((y, x), True)], count + 1) in
           case grid!(y, x) of
             '.' -> countEnergizedLoop grid (nextVisit p (dx, dy)) ((visited', energized'), count')
             '\\' -> countEnergizedLoop grid (nextVisit p (dy, dx)) ((visited', energized'), count')
             '/' -> countEnergizedLoop grid (nextVisit p (- dy, - dx)) ((visited', energized'), count')
             '|' ->
               if dx == 0 then
                 countEnergizedLoop grid (nextVisit p (dx, dy)) ((visited', energized'), count')
               else
                 countEnergizedLoop grid (nextVisit p (0, -1))
                   (countEnergizedLoop grid (nextVisit p (0, 1)) ((visited', energized'), count'))
             '-' ->
               if dy == 0 then
                 countEnergizedLoop grid (nextVisit p (dx, dy)) ((visited', energized'), count')
               else
                 countEnergizedLoop grid (nextVisit p (-1, 0))
                   (countEnergizedLoop grid (nextVisit p (1, 0)) ((visited', energized'), count'))

countEnergized grid beam =
  let energized =
        array (bounds grid) (map (\(i, _) -> (i, False)) (assocs grid))
  in snd (countEnergizedLoop grid beam ((Set.empty, energized), 0))

main = do
  input <- getContents
  let grid_list = lines input
  let height = length grid_list
  let width = length (grid_list!!0)
  let grid = listArray ((0, 0), (height - 1, width - 1)) (concat grid_list)
  let result_part1 = countEnergized grid ((0, 0), (1, 0))
  putStrLn("Part1: " ++ show result_part1)
  let result_part2 :: Int = foldl1 max (map (countEnergized grid) (
                             [((0, y), (1, 0)) | y <- [0 .. height - 1]] ++
                             [((width - 1, y), (-1, 0)) | y <- [0 .. height - 1]] ++
                             [((x, 0), (0, 1)) | x <- [0 .. width - 1]] ++
                             [((x, height - 1), (0, -1)) | x <- [0 .. width - 1]]))
  putStrLn("Part1: " ++ show result_part2)
