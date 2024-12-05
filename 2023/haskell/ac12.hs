import Data.List
import qualified Data.Map as Map

cut p list =
  let (hd, (_:tl)) = break p list
  in (hd, tl)

split p list =
  let (hd, tl) = break p list
  in case tl of
    [] -> [hd]
    _:tl -> hd:split p tl

parseLine line =
  let (damaged, seqs) = cut (== ' ') line
  in (damaged, map read (split (== ',') seqs))

align memo previous_bad damaged sequences =
  case Map.lookup (previous_bad, damaged, sequences) memo of
    Just result -> (memo, result)
    Nothing ->
      let (memo''', result) =
            case damaged of
              [] ->
                case (previous_bad, sequences) of
                  (0, []) -> (memo, 1)
                  (_, [bad]) | bad == previous_bad -> (memo, 1)
                  _ -> (memo, 0)
              symbol:damaged' ->
                let ok =
                      if previous_bad == 0 then
                        align memo 0 damaged' sequences
                      else
                        case sequences of
                          hd:tl | hd == previous_bad ->
                                 align memo 0 damaged' tl
                          _ -> (memo, 0)
                    bad memo' =
                      case sequences of
                        hd:_tl | hd > previous_bad ->
                                 align memo' (previous_bad + 1) damaged' sequences
                        _ -> (memo', 0)
                in case symbol of
                  '.' -> ok
                  '#' -> bad memo
                  '?' -> let (memo', ok_result) = ok
                             (memo'', bad_result) = bad memo'
                         in (memo'', ok_result + bad_result)
      in (Map.insert (previous_bad, damaged, sequences) result memo''', result)

countAlignments (damaged, sequences) = snd (align Map.empty 0 damaged sequences)

unfold 1 inst = inst
unfold n (damaged, seqs) =
  let (damaged', seqs') = unfold (n - 1) (damaged, seqs)
  in (damaged ++ "?" ++ damaged', seqs ++ seqs')

main = do
    contents <- getContents
    let instances = map parseLine (lines contents)
    let resultPart1 = sum (map countAlignments instances)
    putStrLn("Part 1: " ++ show resultPart1)
    let resultPart2 = sum (map (countAlignments . unfold 5) instances)
    putStrLn("Part 2: " ++ show resultPart2)
