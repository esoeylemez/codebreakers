-- Copyright 2016 Ertugrul Söylemez
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module:     Codebreakers.String
-- Copyright:  Copyright 2016 Ertugrul Söylemez
-- License:    Apache License 2.0
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>

module Codebreakers.String
    ( -- * Utility functions
      countChars,
      permute,

      -- * Text lenses
      blocks,
      charAt,
      letter,
      listFiltered
    )
    where

import Codebreakers.Subst
import Control.Applicative
import Control.Lens
import Data.Char
import Data.Int
import qualified Data.Map.Strict as M
import Data.Monoid
import qualified Data.Text.Lazy as Tl


-- | Traversal for blocks of the given length.

blocks :: Int -> Traversal' [a] [a]
blocks n l = go
    where
    go [] = pure mempty
    go xs = liftA2 (<>) (l pfx) (go sfx)
        where
        (pfx, sfx) = splitAt n xs


-- | Convenience traversal for specific characters in blocks of a
-- specific size.

charAt :: Int -> Int -> Traversal' [Char] Int
charAt b n =
    listFiltered (\c -> isAsciiLower c || isAsciiUpper c) .
    blocks b . ix n . letter


-- | Character counts for the given string.

countChars :: String -> M.Map Char Int
countChars = M.fromListWith (+) . map (, 1)


-- | Traversal for the reduced numeric representation of letters.

letter :: Traversal' Char Int
letter l c
    | isAsciiUpper c = (\x -> chr (mod x 26 + ord 'A')) <$> l (ord c - ord 'A')
    | isAsciiLower c = (\x -> chr (mod x 26 + ord 'a')) <$> l (ord c - ord 'a')
    | otherwise      = pure c


-- | Traversal over all characters that satisfy the given predicate.

listFiltered :: (a -> Bool) -> Lens' [a] [a]
listFiltered p l xs0 = back xs0 <$> l (filter p xs0)
    where
    back (x:xs) (y:ys)
        | p x       = y : back xs ys
        | otherwise = x : back xs (y:ys)
    back xs _ = xs


-- | Permute the positions in the given text using the given permutation
-- table.

permute :: Subst Int64 -> Tl.Text -> Tl.Text
permute ps' = go
    where
    ps = substInverse ps'

    go xs =
        Tl.pack (map (\i -> Tl.index xs (subst ps i))
                     [0 .. Tl.length xs - 1])
