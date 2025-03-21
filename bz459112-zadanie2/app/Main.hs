module Main where
import System.Environment (getArgs)
import Language.Haskell.Syntax
import Language.Haskell.Parser

data Def = Def Name [Pat] Expr
data Expr = Var Name | Expr :$ Expr
type Pat = Name 
type Name = String

newtype Prog = Prog {progDefs :: [Def]} deriving (Show)

infixl 9 :$
-- functions 

fromHsString :: String -> Prog
fromHsString str = Prog $ fromParseResult $ parseModule str

-- a -> b-> c
fromParseResult :: ParseResult HsModule -> [Def]
fromParseResult (ParseFailed srcLoc message) = error $ "Parse failed with message "++message++" at the line "++show (srcLine srcLoc)
fromParseResult (ParseOk hsModule) = fromHsModule hsModule

fromHsModule :: HsModule -> [Def]
fromHsModule (HsModule _ _ _ _ hsDecls) = map fromHsDecl hsDecls

fromHsDecl :: HsDecl -> Def
fromHsDecl (HsFunBind [HsMatch _ (HsIdent name) hsPats (HsUnGuardedRhs hsExpr) [] ]) =
    Def name (map fromHsPat hsPats) (fromHsExpr hsExpr)
-- wariant konstruktora bez argumentow
fromHsDecl (HsPatBind _ hsPat (HsUnGuardedRhs hsExpr) []) = 
    Def (fromHsPat hsPat) [] (fromHsExpr hsExpr)
fromHsDecl (p) = error $ "Unexpected declaration "++show p 

fromHsPat:: HsPat -> Name
fromHsPat (HsPVar (HsIdent name)) = name
fromHsPat p = error $ "Unexpected pattern"++show p

fromHsExpr :: HsExp -> Expr
fromHsExpr (HsParen hsExpr) = fromHsExpr hsExpr
fromHsExpr (HsApp leftHsExpr rightHsExpr) = fromHsExpr leftHsExpr :$ fromHsExpr rightHsExpr
fromHsExpr (HsVar (UnQual (HsIdent name))) = Var name
fromHsExpr (HsCon (UnQual (HsIdent name))) = Var name
fromHsExpr p = error $ "Unexpected expression"++show p
-- -- progDefs prog_cos_tam

parseStr :: String -> IO()
-- parseStr str = print (fromHsString str)
parseStr str = do 
    mapM_ print $ progDefs (fromHsString str)
    putStrLn "--------------------------------------------------" 
    -- case parseModule str of
    -- (ParseFailed srcLoc message) -> error $ "Parse failed with message "++message++" at the line "++show (srcLine srcLoc)
    -- (ParseOk (HsModule _ _ _ _ hsDecl)) -> print hsDecl
    -- (ParseOk (HsModule _ _ _ _ hsDecl)) -> print fromHsString

-- For printing

prettyExpr :: Expr -> String
prettyExpr (Var name) = name 
prettyExpr e = concatSpace (map f (toList2 e))
    where
        f :: Expr -> String 
        f expr = if simple expr 
            then prettyExpr expr
            else "("++ prettyExpr expr ++ ")"

-- definiujemy jak wyglada show na expr 
instance Show Expr where
    show expr = prettyExpr expr

-- data Def = Def Name [Pat] Expr deriving (Show)

instance Show Def where
    show (Def name [] expr) = name ++" = "++ show expr
    show (Def name pats expr) = name ++" "++concatSpace pats ++" = "++show expr 

concatSpace :: [String] -> String
concatSpace [] = ""
concatSpace [a] = a
concatSpace (x:xs) = x++" "++concatSpace xs

toList :: Expr -> [Expr]
toList (a :$ b) = toList a ++ [b]
toList a = [a]

-- second implementation
toList2 :: Expr -> [Expr]
toList2 e = aux e [] 
    where
        aux :: Expr -> [Expr]-> [Expr]
        aux (a :$ b) acc = aux a (b:acc) 
        aux c acc = c:acc 

simple :: Expr -> Bool
simple (_ :$ _) = False
simple _ = True

main :: IO ()
main = do 
    args <- getArgs
    -- print args
    let helpMsg = "Usage: zadanie2 [--help] [file]\n--help  - display this message\nfile    - file with program to reduce"
    case args of 
        ("--help":_) -> putStrLn helpMsg
        -- (fileName:[]) -> putStrLn fileName
        -- [fileName] -> putStrLn fileName
        [fileName] -> do
            s <- readFile fileName
            -- putStrLn s 
            parseStr s
        _ -> putStrLn helpMsg
    