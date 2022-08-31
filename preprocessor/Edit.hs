{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

module Edit(recordDotPreprocessor, recordDotPreprocessorOnFragment) where

import Lexer
import Paren
import Data.Maybe
import Data.Char
import Data.List.Extra
import Control.Monad.Extra

recordDotPreprocessor :: FilePath -> String -> String
recordDotPreprocessor original = unlexerFile (Just original) . unparens . edit . parens . lexer
    where
        edit :: [PL] -> [PL]
        edit = editAddPreamble . editAddInstances . editLoop

recordDotPreprocessorOnFragment :: String -> String
recordDotPreprocessorOnFragment = unlexerFile Nothing . unparens . editLoop . parens . lexer


---------------------------------------------------------------------
-- HELPERS

-- Projecting in on the 'lexeme' inside
type L = Lexeme
unL = lexeme
mkL x = Lexeme 0 0 x ""
pattern L x <- (unL -> x)

-- Projecting in on the lexeme inside an Item
type PL = Paren L
unPL (Item (L x)) = Just x
unPL _ = Nothing
isPL x y = unPL y == Just x
pattern PL x <- (unPL -> Just x)
mkPL = Item . mkL

-- Whitespace
pattern NoW x <- (\v -> if null $ getWhite v


-- | Parenthesise the given list of items
paren :: [PL] -> PL
paren [x] = x
paren xs = case unsnoc xs of
    Just (xs,x) -> Paren (mkL "(") (xs `snoc` setWhite "" x) (mkL ")"){whitespace = getWhite x}
    _ -> Paren (mkL "(") xs (mkL ")")

-- | Add a space to the whitespace of the given item
spc :: PL -> PL
spc = addWhite " "

-- | Add a newline to the whitespace of the given item
nl :: PL -> PL
nl = addWhite "\n"

-- | Add the given whitespace to the given item
addWhite :: String -> PL -> PL
addWhite w x = setWhite (getWhite x ++ w) x

-- | Get the whitespace of the given item
getWhite :: PL -> String
getWhite (Item x) = whitespace x
getWhite (Paren _ _ x) = whitespace x

-- | Set the whitespace of the given item
setWhite :: String -> PL -> PL
setWhite w (Item x) = Item x{whitespace=w}
setWhite w (Paren x y z) = Paren x y z{whitespace=w}

-- | Is the given item a constructor?
isCtor :: PL -> Bool
isCtor (Item x) = any isUpper $ take 1 $ lexeme x
isCtor _ = False

-- | Is the given string a field?
isField :: String -> Bool
isField (x:_) = x == '_' || isLower x
isField _ = False

-- | Make a field name from the given list of strings
makeField :: [String] -> String
makeField [x] = "@" ++ show x
makeField xs = "@'(" ++ intercalate "," (map show xs) ++ ")"


---------------------------------------------------------------------
-- PREAMBLE

-- | Add the necessary extensions, imports and local definitions
editAddPreamble :: [PL] -> [PL]
editAddPreamble o@xs
    -- if the first element is "module", and the second element is a module name, and the third element is "where"
    | (premodu, modu:modname@xs) <- break (isPL "module") xs
    -- if the first element is "where", and the second element is not "where"
    , (prewhr, whr:xs) <- break (isPL "where") xs
    -- then add the prefix, module, where, imports, and trailing
    = nl (mkPL prefix) : premodu ++ modu : prewhr ++ whr : nl (mkPL "") : nl (mkPL imports) : xs ++ [nl $ mkPL "", nl $ mkPL $ trailing modname]
    -- else add the prefix, imports, and trailing
    | otherwise = blanks ++ nl (mkPL prefix) : nl (mkPL imports) : rest ++ [nl $ mkPL "", nl $ mkPL $ trailing []]
    where
        (blanks, rest) = span (isPL "") o

        -- the prefix is the language extensions, and a HLINT ignore
        prefix = "{-# LANGUAGE DuplicateRecordFields, DataKinds, FlexibleInstances, TypeApplications, FlexibleContexts, MultiParamTypeClasses, TypeFamilies, TypeOperators, GADTs, UndecidableInstances #-}\n" ++
                 -- it's too hard to avoid generating excessive brackets, so just ignore the code
                 -- only really applies to people using it through Haskell Language Server (see #37)
                 "{- HLINT ignore \"Redundant bracket\" -}"
        -- the imports
        imports = "import qualified GHC.Records.Extra as Z"
        -- if you import two things that have preprocessor_unused, and export them as modules, you don't want them to clash
        trailing modName = "_recordDotPreprocessorUnused" ++ uniq ++ " :: Z.HasField \"\" r a => r -> a;" ++
                           "_recordDotPreprocessorUnused" ++ uniq ++ " = Z.getField @\"\""
            where uniq = filter isAlphaNum $ concat $ take 19 $ takeWhile modPart $ map lexeme $ unparens modName
        modPart x = x == "." || all isUpper (take 1 x)


---------------------------------------------------------------------
-- SELECTORS

-- given .lbl1.lbl2 return ([lbl1,lbl2], whitespace, rest)
spanFields :: [PL] -> ([String], String, [PL])
-- if the first element is ".", and the second element is a field, and the third element is whitespace
spanFields (NoW (PL "."):x@(PL fld):xs) | isField fld = (\(a,b,c) -> (fld:a,b,c)) $
    -- if the third element is whitespace, then recurse
    case x of NoW{} -> spanFields xs; _ -> ([], getWhite x, xs)
spanFields xs = ([], "", xs)


editLoop :: [PL] -> [PL]

--  Leave quasiquotations alone
editLoop (p : ps) | isQuasiQuotation p = p : editLoop ps

-- | a.b.c ==> getField @'(b,c) a
editLoop (NoW e : (spanFields -> (fields@(_:_), whitespace, rest)))
    | not $ isCtor e
    -- add the whitespace, then the getField, then the field name, then the expression
    = editLoop $ addWhite whitespace (paren [spc $ mkPL "Z.getField", spc $ mkPL $ makeField fields, e]) : rest

-- (.a.b) ==> (getField @'(a,b))
editLoop (Paren start@(L "(") (spanFields -> (fields@(_:_), whitespace, [])) end:xs)
  -- add the getField, then the field name, then the close
  = editLoop $ Paren start [spc $ mkPL "Z.getField", addWhite whitespace $ mkPL $ makeField fields] end : xs

-- e{b.c=d, ...} ==> setField @'(b,c) d
editLoop (e:Paren (L "{") inner end:xs)
    -- if the first element is not a constructor, and the first element is not "::", and the first element has no whitespace
    | not $ isCtor e
    , not $ isPL "::" e
    , getWhite e == ""
    -- then split the inner on ",", and for each element, if it is a field, then get the field name, and the operator and body
    , Just updates <- mapM f $ split (isPL ",") inner
    -- then add the update
    , let end2 = [Item end{lexeme=""} | whitespace end /= ""]
    = editLoop $ renderUpdate (Update e updates) : end2 ++ xs
    where
        -- if the first element is a field, then get the field name, and the operator and body
        f (NoW (PL field1) : (spanFields -> (fields, whitespace, xs)))
            | isField field1
            = g (field1:fields) xs
        -- if the first element is a field, then get the field name, and the operator and body
        f (x@(PL field1):xs)
            | isField field1
            = g [field1] xs
        f _ = Nothing

        -- if the operator is "=", then the operator is Nothing, else the operator is Just the operator
        -- if the body is empty, then the body is Nothing, else the body is Just the body
        g fields (op:xs) = Just (fields, if isPL "=" op then Nothing else Just op, Just $ paren xs)
        g fields [] = Just (fields, Nothing, Nothing)


-- | If the first element is a parenthesis, then recurse on the inner
editLoop (Paren a b c:xs) = Paren a (editLoop b) c : editLoop xs
-- | Otherwise, recurse on the tail
editLoop (x:xs) = x : editLoop xs
-- | If the list is empty, then return an empty list
editLoop [] = []


---------------------------------------------------------------------
-- UPDATES

-- | A record update
data Update = Update
    PL -- The expression being updated
    [([String], Maybe PL, Maybe PL)] -- (fields, operator, body)

-- | Render a record update
renderUpdate :: Update -> PL
renderUpdate (Update e upd) = case unsnoc upd of
    -- if there is no update, then return the expression
    Nothing -> e
    -- if there is an update, then return the update
    Just (rest, (field, operator, body)) -> paren
        [spc $ mkPL $ if isNothing operator then "Z.setField" else "Z.modifyField"
        ,spc $ mkPL $ makeField $ if isNothing body then [last field] else field
        ,spc (renderUpdate (Update e rest))
        ,case (operator, body) of
            -- if there is an operator, and a body, then return the operator and the body
            (Just o, Just b) -> paren [spc $ if isPL "-" o then mkPL "subtract" else o, b]
            -- if there is no operator, and a body, then return the body
            (Nothing, Just b) -> b
            -- if there is no operator, and no body, then return the field
            (Nothing, Nothing)
                | [field] <- field -> mkPL field
                | f1:fs <- field -> paren [spc $ mkPL "Z.getField", spc $ mkPL $ makeField fs, mkPL f1]
            -- otherwise, error
            _ -> error "renderUpdate, internal error"
        ]


---------------------------------------------------------------------
-- INSTANCES

-- | Add the necessary instances
editAddInstances :: [PL] -> [PL]
editAddInstances xs = xs ++ concatMap (\x -> [nl $ mkPL "", mkPL x])
    -- for each record, for each field, add an instance
    [ "instance (aplg ~ (" ++ ftyp ++ ")) => Z.HasField \"" ++ fname ++ "\" " ++ rtyp ++ " aplg " ++
      "where hasField _r = (\\_x -> case _r of {" ++ intercalate " ; "
        -- for each constructor, if the field is in the constructor, then add a setter
        [ if fname `elem` map fst fields then
            "(" ++ cname ++ " " ++
                unwords [if fst field == fname then "_" else "_x" ++ show i | (i, field) <- zipFrom 1 fields] ++
            ") -> " ++ cname ++ " " ++
                unwords [if fst field == fname then "_x" else "_x" ++ show i | (i, field) <- zipFrom 1 fields]
          else
            cname ++ "{} -> Prelude.error " ++ show ("Cannot update " ++ msg cname)
        | Ctor cname fields <- ctors] ++
        -- for each constructor, if the field is in the constructor, then add a getter
        "}, case _r of {" ++ intercalate " ; "
        [ if fname `elem` map fst fields then
            "(" ++ cname ++ " " ++
                unwords [if fst field == fname then "_x1" else "_" | field <- fields] ++
            ") -> _x1"
          else
            cname ++ "{} -> Prelude.error " ++ show ("Cannot get " ++ msg cname)
        | Ctor cname fields <- ctors] ++
      "})"
    | Record rname rargs ctors <- parseRecords xs
    , let rtyp = "(" ++ unwords (rname : rargs) ++ ")"
    , (fname, ftyp) <- nubOrd $ concatMap ctorFields ctors
    , let msg cname = "field " ++ show fname ++ " of type " ++ show rname ++ " with constructor " ++ show cname
    ]

-- | Represent a record, ignoring constructors. For example:
--
-- > data Type a b = Ctor1 {field1 :: Int, field2 :: String} | Ctor2 {field1 :: Int, field3 :: [Bool]}
--
--   Gets parsed as:
--
-- > Record "Type" ["a","b"]
-- >   [Ctor "Ctor1" [("field1","Int"), ("field2","String")]
-- >   [Ctor "Ctor2" [("field1","Int"), ("field3","[Bool]")]
data Record = Record
    {recordName :: String -- Name of the type (not constructor)
    ,recordTyArgs :: [String] -- Type arguments
    ,recordCtors :: [Ctor]
    }
    deriving Show

data Ctor = Ctor
    {ctorName :: String -- Name of constructor
    ,ctorFields :: [(String, String)] -- (field, type)
    }
    deriving Show



-- | Find all the records and parse them
parseRecords :: [PL] -> [Record]
parseRecords = mapMaybe whole . drop1 . split (isPL "data" ||^ isPL "newtype")
    where
        -- if the first element is "data" or "newtype", then parse the record
        whole :: [PL] -> Maybe Record
        whole xs
            | PL typeName : xs <- xs
            -- if the first element is a type name, and the second element is "=", or "where"
            , (typeArgs, _:xs) <- break (isPL "=" ||^ isPL "where") xs
            = Just $ Record typeName (mapMaybe typeArg typeArgs) $ ctor xs
        whole _ = Nothing

        -- some types are raw, some are in brackets (with a kind signature)
        typeArg (PL x) = Just x
        typeArg (Paren _ (x:_) _) = typeArg x
        typeArg _ = Nothing

        -- parse the constructors
        ctor xs
            -- drop the context
            | xs <- dropContext xs
            -- if the first element is a constructor name, then parse the constructor
            , PL ctorName : xs <- xs
            -- drop the type signature
            , xs <- dropWhile (isPL "::") xs
            -- drop the context
            , xs <- dropContext xs
            -- if the first element is a parenthesis, then parse the fields
            , Paren (L "{") inner _ : xs <- xs
            = Ctor ctorName (fields $ map (break (isPL "::")) $ split (isPL ",") inner) :
              -- if the first element is "|", then parse the next constructor
              case xs of
                PL "|":xs -> ctor xs
                _ -> []
        ctor _ = []

        -- we don't use a full parser so dealing with context like
        --   Num a => V3 { xx, yy, zz :: a }
        -- is hard. Fake it as best we can
        dropContext (Paren (L "(") _ _ : PL "=>" : xs) = xs
        dropContext (_ : _  : PL "=>": xs) = xs
        dropContext xs = xs

        -- parse the fields
        fields ((x,[]):(y,z):rest) = fields $ (x++y,z):rest
        fields ((names, _:typ):rest) = [(name, dropWhile (== '!') $ trim $ unlexer $ unparens typ) | PL name <- names] ++ fields rest
        fields _ = []

        -- if the user has a trailing comment want to rip it out so our brackets still work
        unlexer = concatMap $ \x -> lexeme x ++ [' ' | whitespace x /= ""]

