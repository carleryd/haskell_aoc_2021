import System.IO
import Data.Maybe (catMaybes)

data Movement = Forward Int | Down Int | Up Int deriving (Show)

parseMovement :: String -> Maybe Movement
parseMovement str =
  case words str of
    [ "forward", x ] -> Just $ Forward (read x :: Int)
    [ "down", x ] -> Just $ Down (read x :: Int)
    [ "up", x ] -> Just $ Up (read x :: Int)
    _ -> Nothing

getPosition :: [Movement] -> (Int, Int, Int)
getPosition = foldl
  (\(horizontal, depth, aim) movement ->
     case movement of
       Forward x -> (horizontal + x, depth + aim * x, aim)
       Down x -> (horizontal, depth, aim + x)
       Up x -> (horizontal, depth, aim - x)
  ) (0, 0, 0)

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle

  let (horizontal, depth, _) = getPosition $ catMaybes $ fmap parseMovement $ lines contents
  print $ horizontal * depth

  hClose handle
