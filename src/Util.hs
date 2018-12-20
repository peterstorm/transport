module Util where

import Data.List

-- | Simple pretty printing a list to the terminal, I found on stackoverflow!

type Filler = Int -> String -> String

data ColDesc t = ColDesc
               { colTitleFill :: Filler
               , colTitle     :: String
               , colValueFill :: Filler
               , colValue     :: t -> String }

fillLeft :: a -> Int -> [a] -> [a]
fillLeft c n s = s <> replicate (n - length s) c

fillRight :: a -> Int -> [a] -> [a]
fillRight c n s = replicate (n - length s) c <> s

fillCenter :: a -> Int -> [a] -> [a]
fillCenter c n s = replicate l c <> s <> replicate r c
  where x = n - length s
        l = x `div` 2
        r = x - l

left :: Int -> String -> String
left = fillLeft ' '

right :: Int -> String -> String
right = fillRight ' '

center :: Int -> String -> String
center = fillCenter ' '

showTable :: [ColDesc t] -> [t] -> String
showTable cs ts =
  let header = colTitle <$> cs
      rows   = [[colValue c t | c <- cs] | t <- ts]
      widths = [maximum $ length <$> col | col <- transpose $ header : rows]
      seperator = intercalate "-+-" [replicate width '-' | width <- widths]
      fillColls fill cols = intercalate " | " [fill c width col | (c, width, col) <- zip3 cs widths cols]
  in
      unlines $ fillColls colTitleFill header : seperator : map (fillColls colValueFill) rows

