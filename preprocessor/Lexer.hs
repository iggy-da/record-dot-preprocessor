{-# LANGUAGE RecordWildCards, BangPatterns #-}

-- Most of this module follows the Haskell report, https://www.haskell.org/onlinereport/lexemes.html
module Lexer(Lexeme(..), lexer, unlexerFile) where

import Data.Char
import Data.List.Extra
import Data.Tuple.Extra

-- | A lexeme of text, approx some letters followed by some space.
data Lexeme = Lexeme
    {line :: {-# UNPACK #-} !Int -- ^ 1-based line number (0 = generated)
    ,col :: {-# UNPACK #-} !Int -- ^ 1-based col number (0 = generated)
    ,lexeme :: String -- ^ Actual text of the item
    ,whitespace :: String -- ^ Suffix spaces and comments
    } deriving Show


charNewline x = x == '\r' || x == '\n' || x == '\f'
charSpecial x = x `elem` "(),;[]`{}"
charAscSymbol x = x `elem` "!#$%&*+./<=>?@\\^|-~" || x == ':' -- special case for me
charSymbol x = charAscSymbol x || (isSymbol x && not (charSpecial x) && x `notElem` "_\"\'")

charIdentStart x = isAlpha x || x == '_'
charIdentCont x = isAlphaNum x || x == '_' || x == '\''


lexer :: String -> [Lexeme]
lexer = go1 1 1
    where
        -- we might start with whitespace, before any lexemes
        go1 line col xs -- matches if the string is prefixed with whitespaces
            | (whitespace, xs) <- lexerWhitespace xs
            -- only match if there are whitespaces
            , whitespace /= ""
            -- update position based on whitespaces
            , (line2, col2) <- reposition line col whitespace
            = Lexeme{lexeme="", ..} : go line2 col2 xs
        -- else go straight to "go"
        go1 line col xs = go line col xs
        
        -- exit condition
        go line col "" = []
        -- there is more...
        go line col xs
            -- take next lexeme (can be empty)
            | (lexeme, xs) <- lexerLexeme xs
            -- take next whitespaces (can be empty)
            , (whitespace, xs) <- lexerWhitespace xs
            -- update position based on whitespaces and lexeme
            , (line2, col2) <- reposition line col $ lexeme ++ whitespace -- update position
            = Lexeme{..} : go line2 col2 xs


-- update the position (line/column) by a movement represented by the provided String
reposition :: Int -> Int -> String -> (Int, Int)
reposition = go
    where
        go !line !col [] = (line, col)
        go line col (x:xs)
            -- if it's a newline, increment the line number
            | x == '\n' = go (line+1) 1 xs
            -- if it's a tab, increment the column by N
            | x == '\t' = go line (col+8) xs -- technically not totally correct, but please, don't use tabs
            -- if its any other character, increment the column by one
            | otherwise = go line (col+1) xs


-- We take a lot of liberties with lexemes around module qualification, because we want to make fields magic
-- we ignore numbers entirely because they don't have any impact on what we want to do
lexerLexeme :: String -> (String, String)
-- the next lexeme is a single '<char>'. (this case is handled indirectly by the following code and could be removed)
lexerLexeme ('\'':x:'\'':xs) = (['\'',x,'\''], xs)
-- next lexeme is a single apostrophe - may signify end of other lexeme through recursive call
lexerLexeme ('\'':x:xs) | x /= '\'' = ("\'", x:xs) -- might be a data kind, see #25
-- next lexeme is a string or char with length greater than 1 (eg. "'\t'")
lexerLexeme (open:xs) | open == '\'' || open == '\"' = seen [open] $ go xs
    where
        -- take all characters until the `open`ing charater is encountered again
        go (x:xs) | x == open = ([x], xs)
                  | x == '\\', x2:xs <- xs = seen [x,x2] $ go xs -- the next character is escaped, so take it even if it matches `open`
                  | otherwise = seen [x] $ go xs -- everything inbetween the opening and closing character
        go [] = ([], [])
-- next lexeme is an operator/symbol
lexerLexeme (x:xs)
    | charSymbol x
    , (a, xs) <- span charSymbol xs
    = (x:a, xs)
-- next lexeme is an identifier
lexerLexeme (x:xs)
    | charIdentStart x
    , (a, xs) <- span charIdentCont xs
    = (x:a, xs)
-- anything else
lexerLexeme (x:xs) = ([x], xs)
-- exit condition
lexerLexeme [] = ([], [])


-- take the whitespace prefix from the input string, and return it paired with the rest of the string
lexerWhitespace :: String -> (String, String)
lexerWhitespace (x:xs) | isSpace x = seen [x] $ lexerWhitespace xs
-- if the string begins with "--", i.e. a single-line comment ...
lexerWhitespace ('-':'-':xs)
    -- take any further "-" characters, assign them to "a"
    | (a, xs) <- span (== '-') xs
    -- ensure the next charater is not a charSymbol (why?)
    , not $ any charSymbol $ take 1 xs
    -- take everything up to the newline
    , (b, xs) <- break charNewline xs
    -- take the newline
    , (c, xs) <- splitAt 1 xs
    -- concatenate all the found white-space elements in the first element of the result tuple, continue with the rest
    = seen "--" $ seen a $ seen b $ seen c $ lexerWhitespace xs
-- if the string begins with "{-", i.e. a block comment
lexerWhitespace ('{':'-':xs) = seen "{-" $ f 1 xs
    where
        -- if i == 1, and the next element is "-}", we have reached the end of the multi-line comment
        f 1 ('-':'}':xs) = seen "-}" $ lexerWhitespace xs
        -- decrement `i` if there is an inner "-}" (i /= 1)
        f i ('-':'}':xs) = seen "-}" $ f (i-1) xs
        -- increment `i` if there is an inner "{-"
        f i ('{':'-':xs) = seen "{-" $ f (i+1) xs
        -- take any charaters as long as the outermost block comment is not terminated
        f i (x:xs) = seen [x] $ f i xs
        f i [] = ([], [])
-- anything else is not a whitespace
lexerWhitespace xs = ([], xs)

-- prepends whitespaces found in lexerWhitespace to the first element of the result tuple
seen :: [a] -> ([a], b) -> ([a], b)
seen xs = first (xs++)

-- concatenate the lexemes back into a single string/file
unlexerFile :: Maybe FilePath -> [Lexeme] -> String
unlexerFile src xs =
    dropping 1 ++
    -- we split the whitespace up to increase the chances of startLine being true below
    -- pretty ugly code...
    go 1 True (concat
        [ [(line, lexeme ++ w1 ++ take 1 w2)
          ,(if line == 0 then 0 else line + length (filter (== '\n') (lexeme ++ w1 ++ take 1 w2)), drop1 w2)]
        | Lexeme{..} <- xs, let (w1,w2) = break (== '\n') whitespace])
    where
        go
            :: Int -- ^ What line does GHC think we are on
            -> Bool -- ^ Are we at the start of a line
            -> [(Int, String)] -- ^ (original line, lexemes followed by their whitespace)
            -> String
        go ghcLine startLine ((i, x):xs) =
            (if emitDropping then dropping i else "") ++
            x ++
            go
                ((if emitDropping then i else ghcLine) + length (filter (== '\n') x))
                (if null x then startLine else "\n" `isSuffixOf` x)
                xs
            where emitDropping = ghcLine /= i && i /= 0 && startLine
        go _ _ [] = ""

        -- write out a line marker with a trailing newline
        dropping n = case src of
          Just src' -> "{-# LINE " ++ show n ++ " " ++ show src' ++ " #-}\n"
          Nothing -> ""
