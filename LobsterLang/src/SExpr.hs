module SExpr (SExpr(..), getSymbol, getInteger, getList, printTree) where

data SExpr = Integer Int
            | Symbol String
            | List [SExpr]

instance Show SExpr where
    show (Integer i) = show i
    show (Symbol s) = show s
    show (List l) = filter (/= '\"') ("( " ++ foldr (++) "" ((++ " ") <$> (show <$> l)) ++ ")")

getSymbol :: SExpr -> Maybe String
getSymbol (Integer _) = Nothing
getSymbol (List _) = Nothing
getSymbol (Symbol s) = Just s

getInteger :: SExpr -> Maybe Int
getInteger (Integer int) = Just int
getInteger (List _) = Nothing
getInteger (Symbol _) = Nothing

getList :: SExpr -> Maybe [SExpr]
getList (Integer _) = Nothing
getList (List l) = Just l
getList (Symbol _) = Nothing

printTree :: SExpr -> Maybe String
printTree s = Just (show s)
