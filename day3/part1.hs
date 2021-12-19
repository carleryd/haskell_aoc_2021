import Data.List
import Data.Binary ()
import Data.Maybe

type Gamma = Int
type Epsilon = Int

type Zeros = Int
type Ones = Int

binToDec :: String -> Int
binToDec binary =
  let
    xs :: [Bool]
    xs = catMaybes $ foldr
      (\x acc ->
         case x of
           '0' -> acc ++ [Just False]
           '1' -> acc ++ [Just True]
           _ -> acc ++ [Nothing]
      ) [] binary
  in
    foldr (\x y -> fromEnum x + 2*y) 0 xs

parseBinary :: [[Char]] -> (Gamma, Epsilon)
parseBinary xs =
  let
    transposed = transpose xs
    counted :: [(Zeros, Ones)]
    counted = map (\str ->
                     foldr
                       (\x (zeros, ones) ->
                         case x of
                           '0' -> (zeros + 1, ones)
                           '1' -> (zeros, ones + 1)
                           _ -> (zeros, ones)
                       ) (0, 0) str
                  ) transposed
    gamma = fmap (\(zeros, ones) -> if zeros > ones then '0' else '1') counted
    epsilon = fmap (\(zeros, ones) -> if zeros > ones then '1' else '0') counted
  in
    (binToDec gamma, binToDec epsilon)

main :: IO ()
main = do
  contents <- readFile "input.txt"

  print $ parseBinary $ lines contents
