module Main where
import System.Environment (getArgs)
import Language.Haskell.Syntax
import Language.Haskell.Parser
import Data.Map (Map)
import qualified Data.Map as M

data Def = Def Name [Pat] Expr
data Expr = Var Name | Expr :$ Expr
type Pat = Name 
type Name = String

newtype Prog = Prog {progDefs :: [Def]} deriving (Show)

infixl 9 :$

fromHsString :: String -> Prog
fromHsString str = Prog $ fromParseResult $ parseModule str

fromParseResult :: ParseResult HsModule -> [Def]
fromParseResult (ParseFailed srcLoc message) = error $ "Parse failed with message "++message++" at the line "++show (srcLine srcLoc)
fromParseResult (ParseOk hsModule) = fromHsModule hsModule

fromHsModule :: HsModule -> [Def]
fromHsModule (HsModule _ _ _ _ hsDecls) = map fromHsDecl hsDecls

fromHsDecl :: HsDecl -> Def
fromHsDecl (HsFunBind (m :_:_)) =
    case m of
        (HsMatch _ (HsIdent defName) _ _ _) -> error $ "Multiple definitions for a combinator: "++defName
        _-> error "Multiple definitions for some combinator"
fromHsDecl (HsFunBind [HsMatch _ (HsIdent name) hsPats (HsUnGuardedRhs hsExpr) [] ]) =
    Def name (map fromHsPat hsPats) (fromHsExpr hsExpr)
fromHsDecl (HsPatBind _ hsPat (HsUnGuardedRhs hsExpr) []) = 
    Def (fromHsPat hsPat) [] (fromHsExpr hsExpr)
fromHsDecl p = error $ "Unexpected declaration "++show p 

fromHsPat:: HsPat -> Name
fromHsPat (HsPVar (HsIdent name)) = name
fromHsPat p = error $ "Unexpected pattern"++show p

fromHsExpr :: HsExp -> Expr
fromHsExpr (HsParen hsExpr) = fromHsExpr hsExpr
fromHsExpr (HsApp leftHsExpr rightHsExpr) = fromHsExpr leftHsExpr :$ fromHsExpr rightHsExpr
fromHsExpr (HsVar (UnQual (HsIdent name))) = Var name
fromHsExpr (HsCon (UnQual (HsIdent name))) = Var name
fromHsExpr p = error $ "Unexpected expression"++show p

parseStr :: String -> IO()
parseStr str = do 
    let prog = fromHsString str
    let definitions = progDefs prog
    mapM_ print definitions
    putStrLn "--------------------------------------------------"
    let name = checkDuplicates $ map (\(Def defName _ _) -> defName) definitions
    case name of
        Nothing -> case findDuplicatedParams definitions of
            (Just (defName,paramName)) -> putStrLn $ "Multiple arguments with the same name "++paramName++" for the combinator: "++defName
            Nothing -> do
                let m = buildDefMap prog
                case M.lookup "main" m of
                    Nothing -> putStrLn "Missing main definition!"
                    (Just def) -> case def of 
                        (Def _ [] expr) -> printPath m expr
                        _ -> putStrLn "Main should have no aruments!"
        (Just defName) -> do
            putStrLn $ "Definition name: "++defName++" occured more than once"

findDuplicatedParams :: [Def] -> Maybe (Name , Name)
findDuplicatedParams [] = Nothing
findDuplicatedParams (Def defName params _: xs) =
    case checkDuplicates params of
        (Just name) -> Just (defName,name)
        Nothing-> findDuplicatedParams xs

-- For printing
prettyExpr :: Expr -> String
prettyExpr (Var name) = name 
prettyExpr e = concatSpace (map f (toList2 e))
    where
        f :: Expr -> String 
        f expr = if simple expr 
            then prettyExpr expr
            else "("++ prettyExpr expr ++ ")"

instance Show Expr where
    show expr = prettyExpr expr

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

toList2 :: Expr -> [Expr]
toList2 e = aux e [] 
    where
        aux :: Expr -> [Expr]-> [Expr]
        aux (a :$ b) acc = aux a (b:acc) 
        aux c acc = c:acc 

simple :: Expr -> Bool
simple (_ :$ _) = False
simple _ = True

type DefMap = Map Name Def
buildDefMap :: Prog -> DefMap
buildDefMap (Prog defList) = M.fromList (map ( \d@(Def name _ _) -> (name,d)) defList)

subst :: (Name, Expr) -> Expr -> Expr
subst (name,expr) v@(Var var_name)  =
    if name == var_name 
        then expr
        else v
subst p (expr1 :$ expr2) = subst p expr1 :$ subst p expr2 

substList :: [(Name,Expr)] -> Expr -> Expr 
substList [] expr = expr
substList (x:xs) expr = substList xs (subst x expr)

renameDef :: Def -> Def
renameDef (Def defName pats expr) = Def defName (map addPrefix pats) (renameExpr expr)
    where 
        addPrefix :: String -> String
        addPrefix s = "_"++s

        renameExpr :: Expr -> Expr
        renameExpr v@(Var name) = if name `elem` pats then Var (addPrefix name) else v
        renameExpr (expr1 :$ expr2) = renameExpr expr1 :$ renameExpr expr2

outerStep :: DefMap -> Expr -> Maybe Expr
outerStep m expr = case M.lookup defName m of 
    Nothing -> Nothing
    (Just def) -> if length (getParameters def) /= length xs 
        then Nothing 
        else
            let r_Def = renameDef def 
                toSubs = zip (getParameters r_Def) xs 
            in 
                Just $ substList toSubs (getExpr r_Def)
    where 
        ((Var defName):xs) = toList2 expr 

getParameters :: Def -> [Pat]
getParameters (Def _ pats _) = pats

getExpr :: Def -> Expr
getExpr (Def _ _ expr) = expr

rstep :: DefMap -> Expr -> Maybe Expr
rstep m expr = case outerStep m expr of
    (Just e2) -> Just e2
    Nothing -> case expr of 
        (a :$ b) -> case rstep m a of
            (Just a2) -> Just (a2 :$ b)
            Nothing -> case rstep m b of
                (Just b2) -> Just (a :$ b2)
                Nothing -> Nothing
        _ -> Nothing   

rpath :: DefMap -> Expr -> [Expr]
rpath m e = e: case rstep m e of
        (Just e2) ->  rpath m e2
        Nothing -> []

printPath :: DefMap -> Expr -> IO ()
printPath m expr = mapM_ (putStrLn.prettyExpr) $ take 30 $ rpath m expr

checkDuplicates :: [Name] -> Maybe Name 
checkDuplicates [] = Nothing
checkDuplicates (x:xs) = if x `elem` xs 
    then
        Just x
    else
        checkDuplicates xs

main :: IO ()
main = do 
    args <- getArgs
    let helpMsg = "Usage: zadanie2 [--help] [file]\n--help  - display this message\nfile    - file with program to reduce"
    case args of 
        ("--help":_) -> putStrLn helpMsg
        [fileName] -> do
            s <- readFile fileName
            parseStr s
        _ -> putStrLn helpMsg
    