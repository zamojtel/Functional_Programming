module Main where

import System.Environment (getArgs)
import Language.Haskell.Syntax
import Language.Haskell.Parser
import Data.List (intercalate)

-- Types 

newtype Prog = Prog {progDefs :: [Def]} deriving (Show)

type Name = String

data Def = Def { defMatches :: [Match] } deriving (Show)
data Match = Match
    { matchName :: Name
    , matchPats :: [Pat]
    , matchRhs  ::Expr
    } deriving (Show)

infixl 9 :$

data Expr
    = Var Name
    | Con Name
    | Expr :$ Expr deriving (Show)
data Pat = PVar Name | PApp Name [Pat] deriving (Show)

-- Parsing
fromHsString :: String -> Prog
fromHsString str = Prog $ fromParseResult $ parseModule str

fromParseResult :: ParseResult HsModule -> [Def]
fromParseResult (ParseFailed srcLoc message) = error $ "Parse failed with message "++message++" at the line "++show (srcLine srcLoc)
fromParseResult (ParseOk hsModule) = fromHsModule hsModule

fromHsModule :: HsModule -> [Def]
fromHsModule (HsModule _ _ _ _ hsDecls) = map fromHsDecl hsDecls


fromHsDecl::HsDecl -> Def
fromHsDecl (HsFunBind []) = error "Empty Declaration"
fromHsDecl (HsFunBind hsMatches) = Def (map fromHsMatch hsMatches)
fromHsDecl (HsPatBind _srcLoc (HsPVar (HsIdent identName)) (HsUnGuardedRhs hsExp) []) = Def [ Match identName [] (fromHsExpr hsExp) ]

fromHsMatch::HsMatch -> Match
fromHsMatch (HsMatch _srcLoc (HsIdent identName) hsPats (HsUnGuardedRhs hsExp) []) = Match identName (map fromHsPat hsPats) (fromHsExpr hsExp)

-- data Pat = PVar Name | PApp Name [Pat] deriving (Show)
fromHsPat:: HsPat -> Pat
fromHsPat (HsPParen hsPat) = fromHsPat hsPat
fromHsPat (HsPVar (HsIdent name)) = PVar name
fromHsPat (HsPApp (UnQual (HsIdent identName)) hsPats) = PApp identName (map fromHsPat hsPats)
fromHsPat p = error $ "Unexpected Pattern: " ++ show p

-- data Expr
--     = Var Name
--     | Con Name
--     | Expr :$ Expr deriving (Show)

fromHsExpr :: HsExp -> Expr
fromHsExpr (HsParen hsExpr) = fromHsExpr hsExpr
fromHsExpr (HsApp leftHsExpr rightHsExpr) = fromHsExpr leftHsExpr :$ fromHsExpr rightHsExpr
fromHsExpr (HsVar (UnQual (HsIdent name))) = Var name
fromHsExpr (HsCon (UnQual (HsIdent name))) = Con name
fromHsExpr p = error $ "Unexpected expression"++show p

parseStr :: String -> IO ()
parseStr str = do
    let p = fromHsString str
    -- p is of type prog
    mapM_ (putStrLn . prettyDef) (progDefs p)  
    -- temporary
    -- print $ fromHsString str

prettyExpr :: Expr -> String
prettyExpr (Var name) = name 
prettyExpr (Con conName) = conName 
prettyExpr e = concatSpace (map f (toList e))
    where
        f :: Expr -> String 
        f expr = if simple expr 
            then prettyExpr expr
            else "("++ prettyExpr expr ++ ")"

toList :: Expr -> [Expr]
toList e = aux e [] 
    where
        aux :: Expr -> [Expr]-> [Expr]
        aux (a :$ b) acc = aux a (b:acc) 
        aux c acc = c:acc

fromList :: [Expr] -> Expr
fromList [] = error "Empty list"
fromList (x:xs) = go x xs 
    where
        go :: Expr -> [Expr] -> Expr
        go x [] = x
        go x (y:ys) = go (x :$ y) ys
        -- Example
        -- (((e1 :$ e2) :$ e3) :$ e4) 

concatSpace :: [String] -> String
concatSpace [] = ""
concatSpace [a] = a
concatSpace (x:xs) = x++" "++concatSpace xs

simple :: Expr -> Bool
simple (_ :$ _) = False
simple _ = True

-- data Def = Def { defMatches :: [Match] } deriving (Show)
-- data Match = Match
--     { matchName :: Name
--     , matchPats :: [Pat]
--     , matchRhs  ::Expr
--     } deriving (Show)


prettyDef::Def -> String
prettyDef (Def xs) = intercalate "\n" (map prettyMatch xs)

prettyMatch::Match -> String
prettyMatch (Match name [] rhs) = name ++" = " ++ prettyExpr rhs
prettyMatch (Match name pats rhs) = name ++" "++concatSpace (map prettyPat pats)++" = "++prettyExpr rhs

-- data Pat = PVar Name | PApp Name [Pat] deriving (Show)

prettyPat:: Pat -> String
prettyPat (PVar name) = name
-- we just print the name of a constrtor without a list of arguments
prettyPat (PApp name []) = name 
prettyPat (PApp name pats) = "(" ++ concatSpace (name : map prettyPat pats) ++ ")"

testExpr:: IO ()
testExpr = putStrLn $ prettyExpr (Con "S" :$ ((Var "add" :$ Var "m") :$ Var "n"))

-- testPrettyPat:: IO ()
-- testExpr = putStrLn $ prettyExpr (Con "S" :$ ((Var "add" :$ Var "m") :$ Var "n"))


main :: IO ()
main = do 
    args <- getArgs
    let helpMsg = "Usage: zadanie3 [--help] [file]\n--help  - display this message\nfile    - file with program to reduce"
    case args of 
        ("--help":_) -> putStrLn helpMsg
        [fileName] -> do
            s <- readFile fileName
            parseStr s
        _ -> putStrLn helpMsg
