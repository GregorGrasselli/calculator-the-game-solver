import System.Environment (getArgs)
import Data.List (sortBy, tails)

{- Operation syntax: '>n' means shift right in decimal by n places
                     '-'  means change sign
                     '+n' means add n
                     '-n' means subtract n
                     '*n' means multiply by n
                     '/n' means divide by n
                     'reverse' reverse
                     'a=>b' replace a with b
                     ':n' means add n to the end
-}

contains :: String -> String -> Bool
contains string sub =
  or $ zipWith (==) (repeat sub) (takeWhile (\x -> length x >= length sub) $ tails string)


replace :: (String, String) -> Int -> Int
replace (a, b) x = read (replaceHelper a b $ show x)

replaceHelper :: String -> String -> String -> String
replaceHelper a b s
  | length s < la  = s
  | take la s == a = b ++ (replaceHelper a b $ drop la s)
  | otherwise      = [head s] ++ (replaceHelper a b $ tail s)
  where la = length a


data Operation = Operation { applicable :: Int -> Bool
                           , apply :: Int -> Int
                           , representation :: String}

compileOperation :: String -> Operation
compileOperation op
  | startsWith '*'   = Operation (const True) (* numArg) op
  | startsWith '+'   = Operation (const True) (+ numArg) op
  | startsWith '^'   = Operation (const True) (^ numArg) op
  | op == "-"        = Operation (const True) negate op
  | startsWith '-'   = Operation (const True) (subtract numArg) op
  | startsWith '>'   = Operation (const True) (flip div $ 10 ^ numArg) op
  | startsWith '/'   = Operation (\x -> mod x numArg == 0) (flip div $ numArg) op
  | contains op "=>" = Operation (\x -> contains (show x) (fst split)) (replace split) op
  | op == "reverse"  = Operation (const True) (read . reverse . show) op
  | startsWith ':'   = Operation (const True) (read . (++ (show numArg)) . show) op
  where startsWith s = head op == s
        numArg       = read $ tail op
        split        = (takeWhile (/= '=') op, tail $ dropWhile (/= '>') op)


applyOperation :: Operation -> Int -> Maybe (String, Int)
applyOperation op x = calcOp x >>= (\x -> return (representation op, x))
  where calcOp x
          | applicable op $ x = Just $ apply op $ x
          | otherwise = Nothing


solve :: Int -> Int -> Int -> [Operation] -> [[(String, Int)]]
solve start goal moves operations =
  map reverse $ filter isSolution $ (!! moves) $ iterate (concatMap nextGeneration) [[("", start)]]
  where
    nextGeneration sol@((_, s):_)
      | s == goal = [sol]
      | otherwise = map (maybeApply s sol) operations

    maybeApply s sol op = case applyOperation op s of
                            Just v  -> v:sol
                            Nothing -> sol

    isSolution ((_, s):_) = s == goal

prettySolution :: [(String, Int)] -> String
prettySolution ((_, start):solution)
  = show start ++ concatMap (\(op, res) -> " -- [" ++ op ++ "] --> {" ++ show res ++ "}") solution

main = do
  start:goal:moves:operations <- getArgs
  let solutions = solve (read start) (read goal) (read moves) $ map compileOperation operations
  mapM_ (putStrLn . prettySolution) $ sortBy (\x y -> compare (length x) (length y)) $ solutions
