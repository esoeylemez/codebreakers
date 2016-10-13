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
-- Module:     Codebreakers.Text
-- Copyright:  Copyright 2016 Ertugrul Söylemez
-- License:    Apache License 2.0
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>

module Codebreakers.Text
    ( -- * Utility functions
      countChars,
      permute,

      -- * Text lenses
      blocks,
      charAt,
      letter,
      text,
      textFiltered
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
import Data.Text.Lazy.Lens


-- | Traversal for blocks of the given length.

blocks :: Int64 -> Traversal' Tl.Text Tl.Text
blocks n l = go
    where
    go xs | Tl.null xs = pure mempty
    go xs = liftA2 (<>) (l pfx) (go sfx)
        where
        (pfx, sfx) = Tl.splitAt n xs


-- | Convenience traversal for specific characters in blocks of a
-- specific size.

charAt :: Int64 -> Int64 -> Traversal' Tl.Text Int
charAt b n =
    textFiltered (\c -> isAsciiLower c || isAsciiUpper c) .
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

textFiltered :: (Char -> Bool) -> Lens' Tl.Text Tl.Text
textFiltered p l xs = back xs <$> l (Tl.filter p xs)
    where
    back xs' ys'
        | Just (x, xs) <- Tl.uncons xs', Just (y, ys) <- Tl.uncons ys', p x = Tl.cons y (back xs ys)
        | Just (x, xs) <- Tl.uncons xs'                                     = Tl.cons x (back xs ys')
        | otherwise                                                         = mempty


-- | Permute the positions in the given text using the given permutation
-- table.

permute :: Subst Int64 -> Tl.Text -> Tl.Text
permute ps' = go
    where
    ps = substInverse ps'

    go xs =
        Tl.pack (map (\i -> Tl.index xs (subst ps i))
                     [0 .. Tl.length xs - 1])
