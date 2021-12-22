import Data.List
import Data.Maybe

type BoardRow = [Int]
type Board = [BoardRow]
type Line = String
type BingoNumbers = [Int]

splitComma :: String -> [String]
splitComma s =  case dropWhile (==',') s of
             "" -> []
             s' -> w : splitComma s''
               where (w, s'') = break (==',') s'

constructBoards :: [Line] -> [Board]
constructBoards [] = []
constructBoards xs =
  let
    xs' = dropWhile (=="") xs
    (start, rest) = splitAt 5 xs'
    board =  fmap (\line -> fmap (\word -> read word :: Int) $ words line) start
  in
    [board] ++ constructBoards rest

parseInput :: [String] -> (BingoNumbers, [Board])
parseInput xs =
  let
    bingoNumbers = fmap (\word -> read word :: Int) $ splitComma $ head xs
    boards = constructBoards $ tail xs
  in
    (bingoNumbers, boards)

getBingoScore :: [String] -> Maybe Int
getBingoScore xs =
  let
    (bingoNumbers, boards) = parseInput xs
    (_, scoreM) = foldr (\board (prevDrawn, prevM) ->
           let
             (currDrawn, currM) = getScore 0 board bingoNumbers
           in
             if (currDrawn > prevDrawn && isJust currM) then (currDrawn, currM) else (prevDrawn, prevM)
          ) (0, Nothing) boards
  in
    scoreM

getBoardScore :: Board -> BingoNumbers -> Maybe Int
getBoardScore board bingoNumbers =
  let
    checkBingo :: [Int] -> Bool
    checkBingo xs = foldr (\num acc -> acc && elem num bingoNumbers) True xs

    hasRow = foldr (\line acc -> acc || checkBingo line) False board
    hasColumn = foldr (\line acc -> acc || checkBingo line) False $ transpose board

    score = foldr (\number acc -> case elem number bingoNumbers of
                         True -> acc
                         False -> acc + number
                     ) 0 $ concat board
  in
    case hasRow || hasColumn of
      True -> Just score
      False -> Nothing

getScore :: Int -> Board -> BingoNumbers -> (Int, Maybe Int)
getScore i board bingoNumbers =
  let
    partial = take (i + 1) bingoNumbers
    scoreM = getBoardScore board partial

    continue = (i + 1) < length bingoNumbers && isNothing scoreM
    lastNumber = bingoNumbers !! i
  in
    case continue of
      False -> (i, fmap (\score -> score * lastNumber) scoreM)
      True -> getScore (i + 1) board bingoNumbers

main :: IO ()
main = do
  contents <- readFile "input.txt"

  print $ getBingoScore $ lines contents
