{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

-- Custom implementation of Map
data Map k v = Empty | Node k v (Map k v) (Map k v) deriving (Show)

-- Custom implementation of ByteString
newtype ByteString = ByteString String deriving (Show)

-- Helper functions for Map
empty :: Map k v
empty = Empty

insert :: Ord k => k -> v -> Map k v -> Map k v
insert k v Empty = Node k v Empty Empty
insert k v (Node k' v' left right)
    | k < k'    = Node k' v' (insert k v left) right
    | k > k'    = Node k' v' left (insert k v right)
    | otherwise = Node k v left right

lookup :: Ord k => k -> Map k v -> Maybe v
lookup _ Empty = Nothing
lookup k (Node k' v left right)
    | k < k'    = Main.lookup k left
    | k > k'    = Main.lookup k right
    | otherwise = Just v

-- Helper functions for string manipulation
splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn delimiter xs = foldr f [""] xs
  where
    f c acc@(current:rest)
      | c == delimiter = "":acc
      | otherwise = (c:current):rest

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

-- Trade data type and related functions
data Trade = Trade
  { symbol :: String
  , price :: Int
  , quantity :: Int
  } deriving (Show)

parseTrade :: String -> Either String Trade
parseTrade line = case map trim (splitOn ',' line) of
  [_, _, sym, _, pStr, qStr] ->
    case (readMaybe pStr :: Maybe Int, readMaybe qStr :: Maybe Int) of
      (Just p, Just q) -> Right $ Trade sym p q
      _ -> Left $ "Invalid number format in line: " ++ line
  _ -> Left $ "Invalid trade format in line: " ++ line

updateProduct :: (Int, Int) -> Trade -> (Int, Int)
updateProduct (totalValue, totalQuantity) (Trade _ p q) =
  (totalValue + p * q, totalQuantity + q)

-- Process trade data
processTradeData :: [Trade] -> Map String (Int, Int)
processTradeData = foldl' (\acc trade ->
  insert (symbol trade)
         (updateProduct (fromMaybe (0, 0) (Main.lookup (symbol trade) acc)) trade)
         acc) empty

-- Calculate VWAP
calculateVWAP :: (Int, Int) -> Double
calculateVWAP (_, 0) = 0  -- Avoid division by zero
calculateVWAP (totalValue, totalQuantity) = fromIntegral totalValue / fromIntegral totalQuantity

-- JSON-like structure and encoding
data JSON = JObject [(String, JSON)] | JNumber Double | JString String deriving (Show)

encode :: JSON -> ByteString
encode = ByteString . go
  where
    go (JObject pairs) = "{" ++ intercalate "," (map encodePair pairs) ++ "}"
    go (JNumber n) = show n
    go (JString s) = show s
    encodePair (k, v) = show k ++ ":" ++ go v

intercalate :: [a] -> [[a]] -> [a]
intercalate _ [] = []
intercalate _ [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

-- Create JSON object
createJSONObject :: Map String (Int, Int) -> JSON
createJSONObject m = JObject $ map (\(sym, (tv, tq)) ->
  (sym, JObject [("vwap", JNumber (calculateVWAP (tv, tq))), ("volume", JNumber (fromIntegral tq))])) (toList m)

-- Convert Map to list
toList :: Map k v -> [(k, v)]
toList Empty = []
toList (Node k v left right) = toList left ++ [(k, v)] ++ toList right

-- Main function using standard input
main :: IO ()
main = do
  input <- getContents
  let linesOfInput = filter (not . all isSpace) $ lines input
      parsedTrades = map parseTrade linesOfInput
  case sequence parsedTrades of
    Left err -> putStrLn err
    Right trades -> do
      let processedData = processTradeData trades
          jsonObject = createJSONObject processedData
      putStrLn $ case encode jsonObject of ByteString s -> s

-- Helper function for folding left
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ z [] = z
foldl' f z (x:xs) = let z' = f z x in seq z' (foldl' f z' xs)
