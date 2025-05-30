module Main where

import System.Environment (getArgs)
import Language.Haskell.Syntax
import Language.Haskell.Parser
import Data.List (intercalate,find)
import Data.Maybe (fromMaybe)
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

-- subst :: (Name, Expr) -> Expr -> Expr
-- subst (name,expr) c@(Con _) = c 
-- subst (name,expr) v@(Var var_name)  =
--     if name == var_name 
--         then expr
--         else v
-- subst p (expr1 :$ expr2) = subst p expr1 :$ subst p expr2

-- substList :: [(Name,Expr)] -> Expr -> Expr 
-- substList [] expr = expr
-- substList (x:xs) expr = substList xs (subst x expr)


-- Przykładowa funkcja do podstawienia zmiennych (do uzupełnienia)
type Subst = [(Name, Expr)]

substList :: Subst -> Expr -> Expr
substList sub e@(Var x) = fromMaybe e (lookup x sub)
substList sub e@(Con c) = e
substList sub (a :$ b) = substList sub a :$ substList sub b


-- renameMatch::Match -> Match
-- renameMatch (Match name pats rhs) = Match name (map renamePat pats) (renameExpr rhs) 
--     where 
--         renameExpr:: Expr -> Expr
--         renameExpr v@(Var name) =  
--             if name `elem` vars
--                 then Var (addPrefix name)
--                 else v
--         renameExpr c@(Con name) = c
--         renameExpr (e1 :$ e2) = renameExpr e1 :$ renameExpr e2
--         vars::[ String ]
--         vars = concatMap patVars pats

fitPat::DefMap -> Pat -> Expr -> (Expr,Maybe [(Name,Expr)])
fitPat prog (PVar name) e = (e,Just [(name,e)])
-- fitPat prog _ _ = error "Patterns are not supported" 
fitPat prog p@(PApp name pats) e  = case toList e of
    (c@(Con constr) : es ) -> 
        if name == constr
            then 
                if length pats == length es
                    then 
                        case fitPats prog pats es of
                            (es2,Nothing) -> (fromList (c:es2),Nothing)
                            (es2,Just ctx) -> (fromList (c:es2),Just ctx) 
                    else 
                        case rstep prog e of 
                            (Nothing) -> (e,Nothing)
                            (Just e2) -> fitPat prog p e2
            else
                (e,Nothing)
    (v@(Var varname) : es) -> case rstep prog e of
        (Nothing) -> (e,Nothing)
        (Just e3) -> fitPat prog p e3 
    (_) -> error "Unexpected Expression"


-- List of patterns is the same length as the list of expressions
fitPats::DefMap -> [Pat] -> [Expr] -> ([Expr],Maybe [(Name,Expr)])
fitPats prog [] [] = ([],Just [])
fitPats prog (p:ps) (e:es) = 
    case fitPat prog p e of 
        (e1,Nothing) -> (e1:es,Nothing)
        (e1,Just ctx1) -> case fitPats prog ps es of
            (es2,Nothing) -> (e1:es2,Nothing)
            (es2,Just ctx2) -> (e1:es2,Just (ctx1++ctx2))
fitPats _ _ _ = error "Number of patterns differ from the number of expressions"

fitMatch::DefMap -> Match -> [Expr] -> ([Expr],Maybe [(Name,Expr)])
fitMatch prog m es = fitPats prog (matchPats m) es

findMatch::DefMap -> [Match] -> [Expr] -> Maybe (Match,[(Name,Expr)])
findMatch prog [] es = Nothing
findMatch prog (m:ms) es = 
    if length (matchPats m) == length es
        then 
            case fitMatch prog m es of
                (es2,Nothing) ->  findMatch prog ms es2
                (es2,Just ctx) -> Just (m,ctx)
        else
            Nothing

-- renameCtx::[(Name,Expr)] -> [(Name,Expr)]
-- renameCtx ctx = map (\(s,e) -> ((addPrefix s),e)) ctx 

-- outerStep:: Prog -> Expr -> Maybe Expr
outerStep:: DefMap -> Expr -> Maybe Expr
outerStep prog e = case toList e of
    ((Con _) : _ ) -> Nothing
    ((Var name) : es ) -> 
            -- let allDefs = progDefs prog
            -- def = find (any((name == ) .matchName).defMatches) allDefs 
        let def = M.lookup name prog
        in case def of 
            Nothing -> Nothing
            (Just d) -> case findMatch prog (defMatches d) es of
                Nothing -> Nothing
                (Just (m,ctx)) -> 
                    -- let m2 = renameMatch m 
                    --     ctx2 = renameCtx ctx
                    -- in 
                    Just $ substList ctx (matchRhs m)


rstep::DefMap -> Expr -> Maybe Expr
rstep prog e = case outerStep prog e of
    (Just e2) -> Just e2
    (Nothing) -> case e of 
        (a :$ b) -> case rstep prog a of
            (Just a2) -> Just (a2 :$ b)
            (Nothing) -> case rstep prog b of 
                (Just b2) -> Just (a :$ b2)
                (Nothing) -> Nothing
        _ -> Nothing

rpath:: DefMap -> Expr -> [Expr]
rpath prog e = e : case rstep prog e of
    (Just e2) -> rpath prog e2
    (Nothing) -> []


printPath:: DefMap -> Expr ->  IO()
printPath prog expr = mapM_ (putStrLn.prettyExpr) $ take 30 $ rpath prog expr

-- addPrefix::String -> String
-- addPrefix s = "#"++s

-- patVars::Pat -> [String]
-- patVars (PVar name) = [name]
-- patVars (PApp name pats) =  concatMap patVars pats


-- renamePat::Pat -> Pat
-- renamePat (PVar name) =  PVar (addPrefix name)
-- renamePat (PApp name pats) = PApp name (map renamePat pats)

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
