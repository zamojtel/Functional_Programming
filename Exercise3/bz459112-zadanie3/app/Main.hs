{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}
module Main where

import System.Environment (getArgs)
import Language.Haskell.Syntax
import Language.Haskell.Parser
import Data.List (intercalate,find)
import Data.Maybe (fromMaybe, Maybe (Nothing))
import Data.Map (Map)
import qualified Data.Map as M


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

data Dir 
    = L 
    | R deriving (Show)

type Path = [Dir]

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

fromHsPat:: HsPat -> Pat
fromHsPat (HsPParen hsPat) = fromHsPat hsPat
fromHsPat (HsPVar (HsIdent name)) = PVar name
fromHsPat (HsPApp (UnQual (HsIdent identName)) hsPats) = PApp identName (map fromHsPat hsPats)
fromHsPat p = error $ "Unexpected Pattern: " ++ show p

fromHsExpr :: HsExp -> Expr
fromHsExpr (HsParen hsExpr) = fromHsExpr hsExpr
fromHsExpr (HsApp leftHsExpr rightHsExpr) = fromHsExpr leftHsExpr :$ fromHsExpr rightHsExpr
fromHsExpr (HsVar (UnQual (HsIdent name))) = Var name
fromHsExpr (HsCon (UnQual (HsIdent name))) = Con name
fromHsExpr p = error $ "Unexpected expression: "++show p

parseStr :: String -> IO ()
parseStr str = do
    let prog = fromHsString str
    let defMap = buildDefMap prog
    mapM_ (putStrLn . prettyDef) (progDefs prog)  
    putStrLn "---------------------------------"
    printPath defMap (Var "main")

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

toListWithPath :: Expr -> [(Expr,Path)]
toListWithPath e = aux e [] [] 
    where
        aux :: Expr -> [(Expr,Path)] -> Path -> [(Expr,Path)]
        aux (a :$ b) acc currentPath = aux a ((b,R:currentPath):acc) (L:currentPath)
        aux c acc currentPath = map (\(e,path) -> (e,reverse path)) $ (c,currentPath):acc 

test_case_1 = toListWithPath (Con "A" :$ Var "arg1" :$ Var "arg2")

fromList :: [Expr] -> Expr
fromList [] = error "Empty list"
fromList (x:xs) = go x xs 
    where
        go :: Expr -> [Expr] -> Expr
        go x [] = x
        go x (y:ys) = go (x :$ y) ys

concatSpace :: [String] -> String
concatSpace [] = ""
concatSpace [a] = a
concatSpace (x:xs) = x++" "++concatSpace xs

simple :: Expr -> Bool
simple (_ :$ _) = False
simple _ = True

prettyDef::Def -> String
prettyDef (Def xs) = intercalate "\n" (map prettyMatch xs)

prettyMatch::Match -> String
prettyMatch (Match name [] rhs) = name ++" = " ++ prettyExpr rhs
prettyMatch (Match name pats rhs) = name ++" "++concatSpace (map prettyPat pats)++" = "++prettyExpr rhs

prettyPat:: Pat -> String
prettyPat (PVar name) = name
-- we just print the name of a constrtor without a list of arguments
prettyPat (PApp name []) = name 
prettyPat (PApp name pats) = "(" ++ concatSpace (name : map prettyPat pats) ++ ")"

testExpr:: IO ()
testExpr = putStrLn $ prettyExpr (Con "S" :$ ((Var "add" :$ Var "m") :$ Var "n"))

-- Przykładowa funkcja do podstawienia zmiennych (do uzupełnienia)
type Subst = [(Name, Expr)]

substList :: Subst -> Expr -> Expr
substList sub e@(Var x) = fromMaybe e (lookup x sub)
substList sub e@(Con c) = e
substList sub (a :$ b) = substList sub a :$ substList sub b

fitPat::Pat -> Expr -> Either (Maybe Path) Subst
fitPat (PVar name) e = Right [(name,e)]
fitPat p@(PApp name pats) e  = case toListWithPath e of
    ((Con s,_) : es ) -> 
        if name == s && length es == length pats 
            then fitPats pats es 
            else Left Nothing
    ((Var _,_) : _ ) -> Left (Just [])
    (_) -> error "Unexpected Expression"


-- List of patterns is the same length as the list of expressions
fitPats::[Pat] -> [(Expr,Path)] -> Either (Maybe Path) Subst
fitPats [] [] = Right []
fitPats (p:ps) ((expr,path) : es) = 
    case fitPat p expr of 
        (Left Nothing) -> Left Nothing
        (Left (Just path2)) -> Left (Just (path++path2))
        (Right subst) -> 
            case fitPats ps es of
            (Left Nothing) -> Left Nothing
            (Left (Just path3)) -> Left (Just path3)
            (Right subst2) -> Right $ subst ++subst2
fitPats _ _ = error "Number of patterns differ from the number of expressions"

fitMatch::Match -> [(Expr,Path)] -> Either (Maybe Path) Subst
fitMatch m es = fitPats (matchPats m) es

findMatch:: [Match] -> [(Expr,Path)] -> Either (Maybe Path) (Match,Subst)
findMatch [] es = Left Nothing -- brak matchy
findMatch (m:ms) es =  
    if length (matchPats m) == length es
        then 
            case fitMatch m es of
                (Left Nothing) -> findMatch ms es
                (Left (Just path)) -> Left (Just path)
                (Right subst) -> Right (m,subst)
        else
            Left Nothing


outerStep:: DefMap -> Expr -> Either (Maybe Path) Expr
outerStep prog e = case toListWithPath e of
    ((Con _,_) : _ ) -> Left Nothing
    ((Var name,_) : es ) -> 
        let def = M.lookup name prog
        in case def of 
            Nothing -> Left Nothing
            (Just d) -> case findMatch (defMatches d) es of
                (Left Nothing) -> Left Nothing
                (Left (Just path)) -> Left (Just path)
                (Right (m,subst)) -> Right $ substList subst (matchRhs m)

rstep::DefMap -> Expr -> Either (Maybe Path) Expr
rstep prog e = case outerStep prog e of
    (Right e2) -> Right e2
    (Left (Just path)) -> Left (Just path)
    (Left Nothing) -> case e of 
        (a :$ b) -> case rstep prog a of
            (Right a2) -> Right (a2 :$ b)
            (Left (Just path)) -> Left (Just (L:path))
            (Left Nothing) -> case rstep prog b of 
                (Right b2) -> Right (a :$ b2)
                (Left (Just path2)) -> Left (Just (R:path2))
                (Left Nothing) -> Left Nothing
        _ -> Left Nothing

rstepAt:: DefMap -> Expr -> Path -> Either (Maybe Path) Expr
rstepAt prog e [] = rstep prog e
rstepAt prog (a :$ b) (L:ps) = case rstepAt prog a ps of 
    (Left Nothing) -> Left Nothing
    (Left (Just path)) -> Left (Just (L:path))
    (Right expr) -> Right (expr :$ b)
rstepAt prog (a :$ b) (R:ps) = case rstepAt prog b ps of 
    (Left Nothing) -> Left Nothing
    (Left (Just path)) -> Left (Just (R:path))
    (Right expr) -> Right (a :$ expr)
rstepAt _ expr path = error $ "rstepAt "++prettyExpr expr ++ " with path "++ show path

rpath:: DefMap -> Expr -> Path -> [(Expr,Path)]
rpath prog e path = case rstepAt prog e path of
    (Right expr) -> (expr,[]):rpath prog expr []
    (Left Nothing) -> []
    (Left (Just p)) -> rpath prog e p 

printPath:: DefMap -> Expr ->  IO()
printPath prog expr = mapM_ (putStrLn.prettyExpr.fst) $ take 30 $ rpath prog expr []

-- DEFMAP
type DefMap = Map Name Def
buildDefMap :: Prog -> DefMap
buildDefMap (Prog defList) = M.fromList (map ( \d -> (matchName $ head $ defMatches d,d)) defList)

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
