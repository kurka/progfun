-- import Control.Applicative
-- import Control.Monad
-- import System.IO

main :: IO ()
main = do
  _n <- getLine
  numStr <- getLine
  let numLst = str2lst numStr
  let (_vmax, rep) = foldr (\num (cvmax, count) ->
                              case compare num cvmax of
                                LT -> (cvmax, count)
                                EQ -> (cvmax, count+1)
                                GT -> (num, 1)
                           ) (-1, 0) numLst
  print rep

str2lst :: String -> [Int]
str2lst = map read . words
