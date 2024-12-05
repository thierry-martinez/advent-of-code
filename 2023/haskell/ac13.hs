import Data.List

cut p list =
  let (hd, (_:tl)) = break p list
  in (hd, tl)

split p list =
  let (hd, tl) = break p list
  in case tl of
    [] -> [hd]
    _:tl -> hd:split p tl

checkSmudge smudge_count [] [] = smudge_count == 0

checkSmudge smudge_count (ha:ta) (hb:tb) =
  if ha == hb then
    checkSmudge smudge_count ta tb
  else
    smudge_count > 0 && checkSmudge (smudge_count - 1) ta tb

isMirror smudge_count candidate =
  let (l1, l2) = unzip candidate
  in checkSmudge smudge_count (concat l1) (concat l2)

mirrorCandidates pat =
  let (_:tl) = pat
      downwards = tails tl
      (_:rev_tl) = reverse pat
      (_:upwards) = reverse (tails rev_tl)
  in map (uncurry zip) (zip upwards downwards)

mirrors smudge_count pat =
  let candidates = zip [1..] (mirrorCandidates pat)
      mirrors = filter (isMirror smudge_count . snd) candidates
  in map fst mirrors

evaluateReflection smudge_count pat =
  let horizontal = sum (mirrors smudge_count pat)
      vertical = sum (mirrors smudge_count (transpose pat))
  in vertical + 100 * horizontal

main = do
    contents <- getContents
    let patterns = split (== []) (lines contents)
    let resultPart1 = sum (map (evaluateReflection 0) patterns)
    putStrLn("Part 1: " ++ show resultPart1)
    let resultPart2 = sum (map (evaluateReflection 1) patterns)
    putStrLn("Part 2: " ++ show resultPart2)
