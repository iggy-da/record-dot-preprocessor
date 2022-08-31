{-# LANGUAGE ScopedTypeVariables, DeriveFunctor #-}

-- Most of this module follows the Haskell report, https://www.haskell.org/onlinereport/lexemes.html
module Paren(Paren(..), parens, unparens) where

import Data.Tuple.Extra
import Data.List
import Lexer(Lexeme(..))
import Debug.Trace

-- | A list of items which are paranthesised.
data Paren a
    = Item a -- Indiviaaul item
    | Paren a [Paren a] a -- parenthesise, open, inner, close
    | Indent a [Paren a] -- an indented block, open and body
    deriving (Show,Eq,Functor)

-- Arguments in order: lexemeToString, parenthesisPairDefinitions, remainingLexemes; returns found parenthesised segments
parenOn :: (Lexeme -> String) -> [(String, String)] -> [Lexeme] -> [Paren Lexeme]
parenOn proj pairs = fst . go Nothing 0
    where
        -- invariant: if first argument is Nothing, second component of result will be Nothing
        go :: Maybe (Either Int String) -> Int -> [Lexeme] -> ([Paren Lexeme], Maybe (Lexeme, [Lexeme]))
        -- reached the end of a paranthesised block, return the ending delimiter and the remainder with empty "inner" lexemes
        go (Just (Right close)) _ (x:xs) | close == proj x = ([], Just (x, xs))
        go (Just (Left minCol)) _ (x:xs) | col x <= minCol = ([], Just (x{lexeme = "", whitespace = ""}, x:xs))
        -- if we are in an indentation aware "with" block, and there are no more lexemes left, the block is terminated
        go (Just (Left minCol)) _ [] = ([], Just ((Lexeme 0 0 "" ""), []))
        go close currentIdent (start:xs)
            -- if the next lexeme is the beginning of a inner parenthesised block, get the ending block delimiter
            | Just end <- lookup (proj start) pairs
            -- then get the inner lexemes, and Maybe the pair of (end, remaining)
            , (inner, res) <- go (Just $ Right end) currentIdent xs
            = case res of
                -- if there is no end and remaining lexemes for the current block, treat everything from the start to the end as a single unparenthesised Item
                Nothing -> (Item start : inner, Nothing)
                -- construct a Paren from the opening, closing and inner lexemes, and continue with the outer block
                Just (end, xs) -> first (Paren start inner end :) $ go close currentIdent xs
        go close currentIdent (start:next:xs)
            -- if the next lexeme is the beginning of a "with" block
            | proj start == "with"
            , isInfixOf "\n" $ whitespace start -- if the "with" is not followed by a newline, the fields are comma separated
            , proj next /= "{" -- if the "with" is followed by "{", the fields are comma separated
            , (inner, res) <- go (Just $ Left currentIdent) currentIdent (next:xs)
            = case res of
                -- if there is no end and remaining lexemes for the current block, treat everything from the start to the end as a single unparenthesised Item
                Nothing -> (Item start : inner, Nothing)
                -- construct a Paren from the opening, closing and inner lexemes, and continue with the outer block
                Just (_, xs) -> first (Indent start inner :) $ go close currentIdent xs
        go close currentIdent (start:next:xs)
            -- if the next lexeme is the beginning of a "with" block
            | proj start == "with"
            , not $ isInfixOf "\n" $ whitespace start -- if the "with" is not followed by a newline, the field are comma separated
            , proj next /= "{" -- if the "with" is followed by a "{" block, ignore the "with" (treat it as an item)
            = first (Paren start (fmap Item $ inner ++ [last]) (Lexeme 0 0 "" "") :) $ go close currentIdent rest
            where
              (inner, last:rest) = span (not . (isInfixOf "\n") . whitespace) (next:xs)

        -- the next element is neither the beginning nor the end of a block, so treat it as an item
        go close currentIdent (x:xs) = first (Item x :) $ go close nextIdent xs
          where
            -- if the current lexeme ends with a newline, the next indentation will be the column of the next lexeme
            nextIdent = if (isInfixOf "\n" $ whitespace x) then col next else currentIdent
            next = head xs
        go close _ [] = ([], Nothing)

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
unparen (Indent a b) = [a] ++ unparens b
