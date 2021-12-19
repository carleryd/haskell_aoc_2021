import Data.Binary ()
import Data.Maybe

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

data RatingType = Oxygen | Co2 deriving (Eq)

getRating :: RatingType -> [String] -> Int -> Maybe String
getRating _ [] _ = Nothing
getRating _ [x] _ = Just x
getRating rType xs i =
  let
    zeros = foldr (\x count -> if x !! i == '0' then count + 1 else count) 0 xs
    ones = foldr (\x count -> if x !! i == '1' then count + 1 else count) 0 xs

    digit = case rType of
      Oxygen -> if zeros > ones then '0' else '1'
      Co2 -> if zeros > ones then '1' else '0'

    xs' = filter (\x -> x !! i == digit) xs
  in
    getRating rType xs' (i + 1)

getOxygenRating :: [String] -> Int -> Maybe String
getOxygenRating = getRating Oxygen

getCo2Rating :: [String] -> Int -> Maybe String
getCo2Rating = getRating Co2

getLifeSupportRating :: [String] -> Int
getLifeSupportRating xs =
  let
    oxygenRating = binToDec $ fromMaybe "" (getOxygenRating xs 0)
    co2Rating = binToDec $ fromMaybe "" (getCo2Rating xs 0)
  in
    oxygenRating * co2Rating

main :: IO ()
main = do
  contents <- readFile "input.txt"

  print $ getLifeSupportRating $ lines contents
