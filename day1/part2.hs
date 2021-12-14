import System.IO

count :: [Int] -> Maybe Int -> Int -> Int
count [] _ acc = acc
count [_] _ acc = acc
count [_,_] _ acc = acc
count (x1:x2:x3:xs) prevM acc =
  let
    curr = sum [x1,x2,x3]
    newAcc = case prevM of
      Just prev -> if (prev < curr) then (acc + 1) else acc
      Nothing -> acc
  in
    count (x2:x3:xs) (Just curr) newAcc

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle

  let numbers = fmap read (words contents) :: [Int]
  let increases = count numbers Nothing 0
  print increases

  hClose handle
