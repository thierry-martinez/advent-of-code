import Data.Array
import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set

cut p list =
  let (hd, (_:tl)) = break p list
  in (hd, tl)

split p list =
  let (hd, tl) = break p list
  in case tl of
    [] -> [hd]
    _:tl -> hd:split p tl

addCharToHash current_value char =
  (current_value + ord char) * 17 `mod` 256

hash = foldl addCharToHash 0

stripRemove [] _set accu = accu

stripRemove (hd:tl) set accu =
  if last hd == '-' then
    stripRemove tl (Set.insert (init hd) set) accu
  else
    let (label, focus_str) = cut (== '=') hd
        accu' =
          if Set.member label set then
            accu
          else
            (label, read focus_str):accu
    in stripRemove tl set accu'

insertLen :: (Array Int [String], Map.Map String Int) -> (String, Int) -> (Array Int [String], Map.Map String Int)
insertLen (boxes, focuses) (label, focus) =
  let boxes' =
        if Map.member label focuses then
          boxes
        else
          accum (flip (:)) boxes [(hash label, label)]
  in (boxes', Map.insert label focus focuses)

measureLen focuses (i, label) =
  i * focuses Map.! label

sumBox focuses (i, lens) =
  (i + 1) * sum (map (measureLen focuses) (zip [1..] (reverse lens)))

main = do
  contents <- getContents
  let instructions_str = split (== ',') (filter (/= '\n') contents)
  let result_part1 = sum (map hash instructions_str)
  putStrLn("Part 1: " ++ show result_part1)
  let set_instructions = stripRemove (reverse instructions_str) Set.empty []
  let init_boxes = array (0, 255) [(i, []) | i <- [0 .. 255]]
  let (boxes, focuses) =
        foldl insertLen (init_boxes, Map.empty) set_instructions
  let result_part2 = sum (map (sumBox focuses) (assocs boxes))
  putStrLn("Part 2: " ++ show result_part2)
