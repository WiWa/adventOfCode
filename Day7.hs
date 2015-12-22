import System.Environment
import Data.Bits
import Data.List.Split
import qualified Data.Map as Map
import Data.Maybe (isJust, fromJust)
import Data.Char (isDigit)
-- import Debug.Trace (trace)
-- Basically, load definitions into a list,
-- keep re-scanning list until definitions are finished.

type Defs = Map.Map String Int

getOp :: String -> Int -> Int -> Int
getOp "NOT" = \_ a -> complement a
getOp "AND" = (.&.)
getOp "OR" = (.|.)
getOp "LSHIFT" = shift
getOp "RSHIFT" = \a b -> shift a (-b)

isInt :: String -> Bool
isInt = foldl (\acc c -> acc && isDigit c) True

getVal :: String -> Defs -> Maybe Int
getVal a dm =
  if isInt a
    then Just (read a :: Int)
    else Map.lookup a dm

parseConst :: [String] -> Defs -> Maybe Int
parseConst (a:_) = getVal a

parseUnary :: [String] -> Defs -> Maybe Int
parseUnary (op:a:_) dm =
  let aVal = getVal a dm
  in
    if isJust aVal
      then Just (getOp op 0 $ fromJust aVal)
    else Nothing

parseBinary :: [String] -> Defs -> Maybe Int
parseBinary (a:op:b:_) dm =
  let aVal = getVal a dm
      bVal = getVal b dm
  in
    if isJust aVal && isJust bVal
      then Just (getOp op (fromJust aVal) (fromJust bVal))
    else Nothing

parseDef :: [String] -> Defs -> Maybe Int
parseDef xs dm
  | length xs == 3 = parseConst xs dm
  | length xs == 4 = parseUnary xs dm
  | length xs == 5 = parseBinary xs dm
  -- | otherwise = Nothing

getKey :: [a] -> a
getKey = last

parseMultiDefs :: [[String]] -> Defs -> Defs
parseMultiDefs defs dm = parseMultiDefsWithOverrides defs dm Map.empty

parseMultiDefsWithOverrides :: [[String]] -> Defs -> Defs -> Map.Map String Int
parseMultiDefsWithOverrides [def] dm overrides =
  let key = getKey def
      override = Map.lookup key overrides
  in
    if isJust override
      then Map.insert key (fromJust override) dm
    else Map.insert key (fromJust $ parseDef def dm) dm
parseMultiDefsWithOverrides (def:defs) dm overrides =
  let key = getKey def
      override = Map.lookup key overrides
      val = parseDef def dm
  in
    if isJust override
      then parseMultiDefsWithOverrides defs
            (Map.insert key (fromJust override) dm) overrides
    else
      if isJust val
        then parseMultiDefsWithOverrides defs
            (Map.insert key (fromJust val) dm) overrides
      else parseMultiDefsWithOverrides (defs ++ [def]) dm overrides


main :: IO ()
main = do
    print "Day 7!"
    args <- getArgs
    input <- readFile $ head args
    let q = map (splitOn " ") $ lines input
        defs = parseMultiDefs q Map.empty
    print $ Map.lookup "a" defs
      -- 956
    -- Part 2
    let overrides = Map.insert "b" (fromJust $ Map.lookup "a" defs) Map.empty
        defs2 = parseMultiDefsWithOverrides q Map.empty overrides
    print $ Map.lookup "a" defs2
