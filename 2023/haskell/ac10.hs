import Data.Array
import Data.List
import Data.Maybe

followPipe '|' (0, dy) = Just (0, dy)
followPipe '-' (dx, 0) = Just (dx, 0)
followPipe 'L' (0, 1) = Just (1, 0)
followPipe 'L' (-1, 0) = Just (0, -1)
followPipe 'J' (0, 1) = Just (-1, 0)
followPipe 'J' (1, 0) = Just (0, -1)
followPipe '7' (1, 0) = Just (0, 1)
followPipe '7' (0, -1) = Just (-1, 0)
followPipe 'F' (0, -1) = Just (1, 0)
followPipe 'F' (-1, 0) = Just (0, 1)
followPipe _ _ = Nothing

inferPipe (0, _) (0, _) = '|'
inferPipe (_, 0) (_, 0) = '-'
inferPipe (0, 1) (1, 0) = 'L'
inferPipe (-1, 0) (0, -1) = 'L'
inferPipe (0, 1) (-1, 0) = 'J'
inferPipe (1, 0) (0, -1) = 'J'
inferPipe (1, 0) (0, 1) = '7'
inferPipe (0, -1) (-1, 0) = '7'
inferPipe (0, -1) (1, 0) = 'F'
inferPipe (-1, 0) (0, 1) = 'F'

addOffset (x, y) (dx, dy) =
  (x + dx, y + dy)

getCell grid (x, y) =
  grid ! y ! x

pushPipe pipes (_x, _y) '-' = pipes
pushPipe pipes (x, y) symbol =
  accum (flip ( : )) pipes [(y, (x, symbol))]

followPipes grid pipes len p d =
  let p' = addOffset p d
  in case getCell grid p' of
       'S' -> (pipes, d, len + 1)
       symbol ->
         let pipes' = pushPipe pipes p' symbol
             d' = fromJust (followPipe symbol d)
         in followPipes grid pipes' (len + 1) p' d'

findCycleEntry grid p dy dx =
  let d = (dx, dy)
      p' = addOffset p d
      symbol = getCell grid p'
      d'' = followPipe symbol d
  in fmap (\d'' -> (d, p', symbol, d'')) d''

measureCycle grid pipes p =
  let (d, p', symbol, d'):_ =
        catMaybes (concatMap (\dy ->
                                map (findCycleEntry grid p dy) [-1..1]) [-1..1])
      (pipes', d'', len) = followPipes grid pipes 1 p' d'
      pipes'' = pushPipe (pushPipe pipes' p' symbol) p (inferPipe d'' d)
  in (pipes'', len)

data State = Inside Int | Outside | Up | Down

nextState (x, '|') (Inside _) = Outside
nextState (x, '|') Outside = Inside x
nextState (x, 'L') Outside = Down
nextState (x, 'L') (Inside _) = Up
nextState (x, 'F') Outside = Up
nextState (x, 'F') (Inside _) = Down
nextState (x, '7') Down = Inside x
nextState (x, '7') Up = Outside
nextState (x, 'J') Down = Outside
nextState (x, 'J') Up = Inside x

measureCell (total, state) (x, symbol) =
  let total' =
        case state of
          Inside x0 -> total + x - x0 - 1
          _ -> total
  in (total', nextState (x, symbol) state)

measureLine pipes =
  fst (foldl measureCell (0, Outside) (sortOn fst pipes))

measureInside pipes = sum (map measureLine (elems pipes))

arrayOfList list =
  listArray (0, length list - 1) list

main = do
  contents <- getContents
  let grid = arrayOfList (map arrayOfList (lines contents))
  let animal:_ = [(x, y) |
                   y <- range (bounds grid),
                   x <- range (bounds (grid ! y)),
                   getCell grid (x, y) == 'S']
  let pipes = array (bounds grid) [(y, []) | y <- range (bounds grid)]
  let (pipes', len) = measureCycle grid pipes animal
  let area = measureInside pipes'
  putStrLn("Part 1: " ++ show (round (len / 2)))
  putStrLn("Part 2: " ++ show area)
