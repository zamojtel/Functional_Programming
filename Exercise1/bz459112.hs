data Expr = S | K | I | B 
    | Expr :$ Expr 
    | X | Z | V Int  
    deriving (Show, Read)

infixl 9 :$

test1 = S :$ K :$ K :$ X
twoB = S :$B :$ I
threeB = S :$ B :$ (S :$B :$ I)
test3 = threeB :$ X :$ Z
omega = ((S :$ I) :$ I) :$ ((S :$ I) :$ I)
kio = K :$ I :$ omega
add = (B :$ S) :$ (B :$ B)

prettyExpr :: Expr -> String
prettyExpr S = "S"
prettyExpr K = "K"
prettyExpr B = "B"
prettyExpr I = "I"
prettyExpr X = "x"
prettyExpr Z = "z"
prettyExpr (V i) = "v"++show i 
prettyExpr e = concatSpace (map f (toList2 e))
    where
        f :: Expr -> String 
        f e = if simple e 
            then prettyExpr e
            else "("++ prettyExpr e ++ ")"

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

rstep :: Expr -> Maybe Expr
rstep e = 
    case innerStep e of 
        (Just e1) -> Just e1
        Nothing -> 
            case outerStep e of 
                (Just e2) -> Just e2 
                Nothing -> case e of 
                    (a :$ b) -> case rstep a of
                        (Just a2) -> Just ( a2 :$ b )
                        Nothing -> case rstep b of
                            (Just b2) -> Just (a :$ b2)
                            Nothing -> Nothing
                    _ -> Nothing

innerStep :: Expr -> Maybe Expr
innerStep (I :$ x) = Just x
innerStep (a :$ b) = case innerStep a of
                        (Just a2) -> Just (a2 :$ b)
                        Nothing -> case innerStep b of
                            (Just b2) -> Just (a :$ b2)
                            Nothing -> Nothing
innerStep _ = Nothing


rpath :: Expr -> [Expr]
rpath e = e: case rstep e of
            (Just e2) ->  rpath e2
            Nothing -> []

printPath :: Expr -> IO ()
printPath e =  mapM_ (putStrLn . prettyExpr) (take 30 (rpath e))

outerStep :: Expr -> Maybe Expr
outerStep (I :$ x) = Just x
outerStep (B :$ x :$ y :$ z) = Just (x :$ (y :$ z))
outerStep (K :$ x :$ y) = Just x
outerStep (S :$ x :$ y :$ z) = Just ((x :$ z) :$ ( y :$ z))
outerStep _ = Nothing

