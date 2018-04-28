module Lib
    ( substitute, match, transformationApply, reflect
    ) where

--import Data.Maybe

substitute :: Eq a => a -> [a] -> [a] -> [a]
sustitute _ [] _ = []
substitute wildcard (t:ts) s =
  if (t == wildcard)
    then s ++ substitute wildcard ts s
    else t : substitute wildcard ts s
substitute _ _ _ = []

match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ [] _ = Nothing
match _ _ [] = Nothing
match wildcard (x:xs) (t:ts) =
  if (x == wildcard)
    then orElse (singleWildcardMatch (x:xs) (t:ts)) (longerWildcardMatch (x:xs) (t:ts))
    else
      if (x == t)
        then match wildcard xs ts
        else Nothing

singleWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (x:xs) (t:ts) =
  mmap (\m -> [t]) (match x xs ts)

singleWildcardMatch _ _ = Nothing

longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
longerWildcardMatch _ [] = Nothing
longerWildcardMatch (x:xs) (t:ts) =
  mmap (\m -> (t:m)) (match x (x:xs) ts)


transformationApply :: Eq a => a -> ([a] -> [a]) ->
                       [a] -> ([a], [a]) -> Maybe [a]
transformationApply wildcard f s (p1, p2) =
    mmap (substitute wildcard p2) (mmap f (match wildcard p1 s))


transformationsApply :: Eq a => a -> ([a] -> [a]) ->
                        [([a], [a])] -> [a] -> Maybe [a]
transformationsApply wildcard f (p:ps) s = orElse (transformationApply wildcard f s p) (transformationsApply wildcard f ps s)

type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]

reflections =
  [ ("am",     "are"),
    ("was",    "were"),
    ("i",      "you"),
    ("i'm",    "you are"),
    ("i'd",    "you would"),
    ("i've",   "you have"),
    ("i'll",   "you will"),
    ("my",     "your"),
    ("me",     "you"),
    ("are",    "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your",   "my"),
    ("yours",  "mine"),
    ("you",    "me")
  ]
reflect :: Phrase -> Phrase
reflect phrase = map (\word -> foldl (\acc (r1, r2) -> if(r1 == word) then r2 else acc) word reflections) phrase








--map2 :: (a -> b, c -> d) -> (a, c) -> (b, d)
--map2 (f1, f2) (x1, x2) = (f1 x1, f2 x2)

mmap :: (a -> b) -> Maybe a -> Maybe b
mmap f  Nothing = Nothing
mmap f (Just x) = Just (f x)

orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing  x  = x
orElse (Just a) _  = Just a

try :: (a -> Maybe a) -> a -> a
try f x = maybe x id (f x)

fix :: Eq a => (a -> a) -> a -> a
fix f x
   |  f x == x  = x
   |  otherwise = fix f (f x)

pick :: RealFrac r => r -> [a] -> a
pick u xs = xs !! (floor.(u*).fromIntegral.length) xs
