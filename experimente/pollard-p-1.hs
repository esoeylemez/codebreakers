-- |
-- Copyright:  (c) 2016 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>

module Main (main) where

import Control.Monad
import System.Environment


modExp :: Integer -> Integer -> Integer -> Integer
-- modExp n x e = x^e `mod` n
modExp n = go 1
    where
    go y _ 0 = y
    go y x e
        | even e    = go y             x2 (e `div` 2)
        | otherwise = go (y*x `mod` n) x2 (e `div` 2)
        where
        x2 = x*x `mod` n


pollard :: Integer -> (Integer, Integer)
pollard n =
    foldr const (1, n) $ do
        b0 <- [2,3,5]
        (y, _) <- takeWhile (\(b, _) -> b /= 1) $
                  iterate (\(b, e) -> (modExp n b e, e + 1)) (b0, 2)
        let f = gcd (y - 1) n
            g = n `div` f
        guard (f /= 1)
        guard (f*g == n)
        pure (f, n `div` f)


main :: IO ()
main = getArgs >>= mapM_ (print . pollard . read)
