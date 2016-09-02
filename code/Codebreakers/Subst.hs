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

{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module:     Codebreakers.Subst
-- Copyright:  Copyright 2016 Ertugrul Söylemez
-- License:    Apache License 2.0
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>

module Codebreakers.Subst
    ( -- * Substitution tables
      Subst(..),
      (-->),
      subst,
      substInverse,
      substSingleton
    )
    where

import           Data.Foldable
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Mh
import           Data.Hashable (Hashable)
import           Data.List (sort)
import           Data.Monoid


-- | Substitution tables.

data Subst a =
    Subst {
      fromSubst :: HashMap a a
    }
    deriving (Eq)

instance (Eq a, Hashable a) => Monoid (Subst a) where
    mempty = Subst mempty

    mappend (Subst ys2) (Subst ys1) =
        Subst $
        Mh.map (\y -> Mh.lookupDefault y y ys2) ys1 <>
        Mh.difference ys2 ys1

instance {-# OVERLAPPABLE #-} (Ord a, Show a) => Show (Subst a) where
    showsPrec d (Subst xs') =
        showParen (d > 0) $
            (if d > 0 then showChar '\n' else id) .
            showString (concatMap (\(x, _, l) -> pad l $ showsPrec 11 x "") xs) .
            showChar '\n' .
            showString (concatMap (\(_, y, l) -> pad l $ showsPrec 11 y "") xs) .
            (if d > 0 then showChar '\n' else id)

        where
        len x = length (showsPrec 11 x "")
        pad l ys = ' ' : replicate (l - length ys) ' ' ++ ys

        xs = map (\(x, y) -> (x, y, max (len x) (len y))) . sort . Mh.toList $ xs'

instance {-# OVERLAPPING #-} Show (Subst Char) where
    showsPrec d (Subst xs') =
        showParen (d > 0) $
            (if d > 0 then showChar '\n' else id) .
            foldl' (.) id (map (showChar . fst) xs) .
            showChar '\n' .
            foldl' (.) id (map (showChar . snd) xs) .
            (if d > 0 then showChar '\n' else id)

        where
        xs = sort . Mh.toList $ xs'


-- | Alias for 'substSingleton'.

(-->) :: (Eq a, Hashable a) => a -> a -> Subst a
(-->) = substSingleton


-- | Substitute using the given table.

subst :: (Eq a, Hashable a) => Subst a -> a -> a
subst (Subst ys) x = Mh.lookupDefault x x ys


-- | Inverse of the given substitution.

substInverse :: (Eq a, Hashable a) => Subst a -> Subst a
substInverse = Subst . Mh.fromList . map (\(x, y) -> (y, x)) . Mh.toList . fromSubst


-- | Substitution table with only the given substitution.

substSingleton :: (Eq a, Hashable a) => a -> a -> Subst a
substSingleton x y = Subst (Mh.fromList [(x, y), (y, x)])
