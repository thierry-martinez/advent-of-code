import Data.Char
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

data Line = Line {
  dst :: Int,
  src :: Int,
  len :: Int
}

parseLine line =
  let (dst_str, tail) = cut (== ' ') line
      (src_str, len_str) = cut (== ' ') tail
  in Line { dst = read dst_str, src = read src_str, len = read len_str }

parseMaps lines =
  case lines of
    [] -> []
    _emptyline:_title:value_lines ->
      let (values_str, tail) = break (== []) value_lines
          values = map parseLine values_str
      in values:parseMaps tail

parseInput (seeds_str:maps_lines) =
  let seeds = map read (split (== ' ') (drop (length "seeds: ") seeds_str))
      maps = parseMaps maps_lines
  in (seeds, maps)

singleImageOfLine value line =
  if value >= src line && value < src line + len line then
    Just (dst line + value - src line)
  else
    Nothing

singleImageOfMap value m =
  case catMaybes (map (singleImageOfLine value) m) of
    image:_ -> image
    [] -> value

data Interval = Interval {
  isrc :: Int,
  ilen :: Int
}

peekInterval list =
  case list of
    src:len:tail -> Just (Interval { isrc = src, ilen = len }, tail)
    _ -> Nothing

intervalBetween isrc end =
  if isrc < end then
    [Interval { isrc = isrc, ilen = end - isrc }]
  else
    []

intervalImageOfLine line (remaining_intervals, new_intervals) interval =
  let line_end = src line + len line
      interval_end = isrc interval + ilen interval
  in if line_end <= isrc interval || interval_end <= src line then
       (interval:remaining_intervals, new_intervals)
     else
       let begin = max (src line) (isrc interval)
           end = min line_end interval_end
       in (intervalBetween (isrc interval) begin ++
           intervalBetween end interval_end ++
           remaining_intervals,
           Interval { isrc = dst line + begin - src line, ilen = end - begin }:new_intervals)

intervalImagesOfLine (intervals, new_intervals) line =
  foldl (intervalImageOfLine line) ([], new_intervals) intervals

intervalImagesOfMap intervals map =
  let (remaining_intervals, new_intervals) = foldl intervalImagesOfLine (intervals, []) map
  in remaining_intervals ++ new_intervals

main = do
  input <- getContents
  let (seeds, maps) = parseInput (lines input)
  let values = map (\value -> foldl singleImageOfMap value maps) seeds
  let result_part1 = foldr1 min values
  putStrLn ("Part 1: " ++ show result_part1)
  let seed_intervals = unfoldr peekInterval seeds
  let intervals = foldl intervalImagesOfMap seed_intervals maps
  let result_part2 = foldr1 min (map isrc intervals)
  putStrLn ("Part 2: " ++ show result_part2)
