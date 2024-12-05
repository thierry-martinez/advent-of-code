import Data.List
import Data.Char
import qualified Data.Map as Map

data Kind = Five | Four | Full | Three | Two | One | High
  deriving (Eq, Ord)

parikhOfStr = foldl (\m c -> Map.insertWith (+) c 1 m) Map.empty

countsOfParikhVector = reverse . sort . map snd . Map.toList

kindOfCounts counts =
  case counts of
       [5] -> Five
       [4, 1] -> Four
       [3, 2] -> Full
       [3, 1, 1] -> Three
       [2, 2, 1] -> Two
       [2, 1, 1, 1] -> One
       [1, 1, 1, 1, 1] -> High
       _ -> error "Invalid hand"
       
kindOfStrPart1 = kindOfCounts . countsOfParikhVector . parikhOfStr

kindOfStrPart2 hand =
  let parikhVector = parikhOfStr hand
      (maybe_jokers, parikhVector') =
        Map.updateLookupWithKey (\_ _ -> Nothing) 'J' parikhVector
      jokers = maybe 0 id maybe_jokers
      counts = case countsOfParikhVector parikhVector' of
                 best : others -> (best + jokers) : others
                 [] -> [jokers]
  in kindOfCounts counts

handBidOfStr str =
  let (hand, (_space:rest)) = break isSpace str
  in (hand, read rest)

strengthPart1 = "AKQJT98765432"
strengthPart2 = "AKQT98765432J"

getStrength strength card =
  let hd:_ = filter ((== card) . snd) (zip [0..] strength)
  in hd

handBidCompare getKind strength (hand1, _bid1) (hand2, _bid2) =
  case compare (getKind hand2) (getKind hand1) of
    EQ ->
      compare (map (getStrength strength) hand2)
        (map (getStrength strength) hand1)
    result -> result

eval getKind strength handBids =
  let sortedHands = sortBy (handBidCompare getKind strength) handBids
  in sum (map (\(rank, (_, bid)) -> rank * bid) (zip [1..] sortedHands))

main = do
  input <- getContents
  let handBids = map handBidOfStr (lines input)
  let result_part1 = eval kindOfStrPart1 strengthPart1 handBids
  putStrLn ("Part 1: " ++ show result_part1)
  let result_part2 = eval kindOfStrPart2 strengthPart2 handBids
  putStrLn ("Part 2: " ++ show result_part2)
