module Countdown where
import Data.List
import Data.Char
import Data.Maybe
import Data.Tuple

data BinOp = Plus | Minus | Times | Div | No
    deriving (Show, Eq, Enum)
data Expr = Const Int | Op BinOp Expr Expr
    deriving (Show, Eq)

matchOp :: BinOp -> String
matchOp op = case op of 
                Plus -> " + "
                Minus -> " - "
                Div -> " / "
                Times -> " * "

expr2String :: Expr -> String
expr2String (Const n) = show n
expr2String (Op op exp1 exp2) =  expr1 ++ matchOp op ++ expr2
        where expr1 | isGreaterPriority op exp1 = "(" ++ expr2String exp1 ++ ")"
                    | otherwise = expr2String exp1
              expr2 | isGreaterPriority op exp2 = "(" ++ expr2String exp2 ++ ")"
                    |  otherwise = if getOp exp2 == op then "(" ++ expr2String exp2 ++ ")"
                        else expr2String exp2

-- increasing priority
operatorsPriority = [Plus,Minus,Times,Div]

isGreaterPriority :: BinOp -> Expr -> Bool
isGreaterPriority _ (Const n) = False
isGreaterPriority op1 (Op op2 _ _) = elemIndex op2 operatorsPriority < elemIndex op1 operatorsPriority

getOp :: Expr -> BinOp
getOp (Const _) = No
getOp (Op op _ _) = op

leftExpr :: Expr -> Expr
leftExpr (Op op exp1 _) = exp1

rightExpr :: Expr -> Expr
rightExpr (Op op _ exp2) = exp2


countdownAllSolutions :: [Int] -> Int -> [Expr]
countdownAllSolutions xs n = [expr | xs <- subsets xs,
                                    expr <- allExpressions xs,
                                    calc expr == n]

allSplits :: [Int] -> [([Int],[Int])]
allSplits [] = [([],[])]
allSplits xs = zs -- ++ map swap zs -- pairs ++ swap pairs
    where ys = makePairs xs [] --make pairs
          zs = take (length ys - 2)  ys -- drop last 2 elem

makePairs :: [Int] -> [Int] -> [([Int],[Int])]
makePairs [] _ = [([],[])]
makePairs (x:xs) ys = (ys ++ [x],xs) : makePairs xs (ys ++[x])

-- >>> allSplits [1,1,2]
-- [([1],[1,2]),([1,1],[2]),([1,2],[1]),([2],[1,1])]

subsets :: [Int] -> [[Int]]
subsets [] = [[]]
subsets xs = removeDuplicates (subsequences xs ++ subsequences (reverse xs) ++ tail (permutations xs))

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs)   | x `elem` xs   = removeDuplicates xs
                          | otherwise     = x : removeDuplicates xs

-- >>> subsets [1,1,2]
-- [[1,2],[],[2],[1],[2,1],[1,1],[1,1,2],[2,1,1],[1,2,1]]

calc :: Expr -> Int
calc (Const n) = n
calc (Op Plus exp1 exp2) = calc exp1 + calc exp2
calc (Op Times exp1 exp2) = calc exp1 * calc exp2

-- >>> calc sampleExpr2
-- 15

makeExpression :: Expr -> Expr -> [Expr]
makeExpression exp1 exp2 = [Op op exp1 exp2 | op <- [Plus, Times]]

allExpressions :: [Int] -> [Expr]
allExpressions [] = []
allExpressions [n] = [Const n]
allExpressions xs = [expr | (exprs1, exprs2) <- allSplits xs,
                            exp1 <- allExpressions exprs1,
                            exp2 <- allExpressions exprs2,
                            expr <- makeExpression exp1 exp2]

valuesOfExpr :: Expr -> [Int]
valuesOfExpr (Const n) = [n]
valuesOfExpr (Op op exp1 exp2) = valuesOfExpr exp1 ++ valuesOfExpr exp2

isSolution :: Expr -> [Int] -> Int -> Bool
isSolution expr ns n = elem (valuesOfExpr expr) (subsets ns) && calc expr == n

-- TESTS
-- >>> countdownAllSolutions [1,2,3] 7
-- [Op Plus (Const 1) (Op Times (Const 2) (Const 3)),Op Plus (Const 1) (Op Times (Const 3) (Const 2)),Op Plus (Op Times (Const 2) (Const 3)) (Const 1),Op Plus (Op Times (Const 3) (Const 2)) (Const 1),Op Plus (Op Times (Const 3) (Const 2)) (Const 1),Op Plus (Op Times (Const 2) (Const 3)) (Const 1),Op Plus (Const 1) (Op Times (Const 3) (Const 2)),Op Plus (Const 1) (Op Times (Const 2) (Const 3)),Op Plus (Op Times (Const 2) (Const 3)) (Const 1),Op Plus (Op Times (Const 3) (Const 2)) (Const 1),Op Plus (Const 1) (Op Times (Const 2) (Const 3)),Op Plus (Const 1) (Op Times (Const 3) (Const 2)),Op Plus (Const 1) (Op Times (Const 3) (Const 2)),Op Plus (Const 1) (Op Times (Const 2) (Const 3)),Op Plus (Op Times (Const 3) (Const 2)) (Const 1),Op Plus (Op Times (Const 2) (Const 3)) (Const 1)]

-- >>> countdownAllSolutions [1,2,3] 3
-- [Op Plus (Const 1) (Const 2),Op Times (Const 1) (Const 3),Const 3,Op Times (Const 3) (Const 1),Op Plus (Const 2) (Const 1)]
