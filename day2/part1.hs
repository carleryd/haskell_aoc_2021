import Data.Maybe (catMaybes)

data Movement = Forward Int | Down Int | Up Int deriving (Show)

parseMovement :: String -> Maybe Movement
parseMovement str =
  case words str of
    [ "forward", amount ] -> Just $ Forward (read amount :: Int)
    [ "down", amount ] -> Just $ Down (read amount :: Int)
    [ "up", amount ] -> Just $ Up (read amount :: Int)
    _ -> Nothing

getPosition :: [Movement] -> (Int, Int)
getPosition = foldr
  (\movement (forward, downward) ->
     case movement of
       Forward amount -> (forward + amount, downward)
       Down amount -> (forward, downward + amount)
       Up amount -> (forward, downward - amount)
  ) (0, 0)

main :: IO ()
main = do
  contents <- readFile "input.txt"

  let (forward, downward) = getPosition $ catMaybes $ fmap parseMovement $ lines contents

  print $ forward * downward
