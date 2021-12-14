count :: [Int] -> Int -> Int
count [] acc = acc
count [_] acc = acc
count (x:xs) acc =
  let
    newAcc = if (x < head xs) then (acc + 1) else acc
  in
    count xs newAcc

main :: IO ()
main = do
  contents <- readFile "input.txt"

  let numbers = fmap read (words contents) :: [Int]
  let increases = count numbers 0
  print increases
