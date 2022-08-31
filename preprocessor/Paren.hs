{-# LANGUAGE ScopedTypeVariables, DeriveFunctor #-}

-- Most of this module follows the Haskell report, https://www.haskell.org/onlinereport/lexemes.html
module Paren(Paren(..), parens, unparens) where

import Data.Tuple.Extra
import Lexer(Lexeme(..))

-- | A list of items which are paranthesised.
data Paren a
    = Item a -- Indiviaaul item
    | Paren a [Paren a] a -- parenthesise, open, inner, close
    deriving (Show,Eq,Functor)

-- `a` is actually `Lexeme`, and `b` is actually `String`
-- Arguments in order: lexemeToString, parenthesisPairDefinitions, remainingLexemes; returns found parenthesised segments
parenOn :: forall a b . Eq b => (a -> b) -> [(b, b)] -> [a] -> [Paren a]
parenOn proj pairs = fst . go Nothing
    where
        -- invariant: if first argument is Nothing, second component of result will be Nothing
        go :: Maybe b -> [a] -> ([Paren a], Maybe (a, [a]))
        -- reached the end of a paranthesised block, return the ending delimiter and the remainder with empty "inner" lexemes
        go (Just close) (x:xs) | close == proj x = ([], Just (x, xs))
        go close (start:xs)
            -- if the next lexeme is the beginning of a inner parenthesised block, get the ending block delimiter
            | Just end <- lookup (proj start) pairs
            -- then get the inner lexemes, and Maybe the pair of (end, remaining)
            , (inner, res) <- go (Just end) xs
            = case res of
                -- if there is no end and remaining lexemes for the current block, treat everything from the start to the end as a single unparenthesised Item
                Nothing -> (Item start : inner, Nothing)
                -- construct a Paren from the opening, closing and inner lexemes, and continue with the outer block
                Just (end, xs) -> first (Paren start inner end :) $ go close xs
        -- the next element is neither the beginning nor the end of a block, so treat it as an item
        go close (x:xs) = first (Item x :) $ go close xs
        go close [] = ([], Nothing)

-- groups Lexmes by paranthesised segments as a tree structure of nested segments
parens :: [Lexeme] -> [Paren Lexeme]
-- `lexeme` is the field accessor of Lexeme, so Lexeme -> String
parens = parenOn lexeme [("(",")"),("[","]"),("{","}"),("`","`")]

unparens :: [Paren a] -> [a]
unparens = concatMap unparen

-- transform back to Lexemes
unparen :: Paren a -> [a]
unparen (Item x) = [x]
unparen (Paren a b c) = [a] ++ unparens b ++ [c]
