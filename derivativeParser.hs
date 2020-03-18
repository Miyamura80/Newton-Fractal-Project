data Expression = Var String
                | C String
                | Expression `Add` Expression
                | Expression `Mul` Expression
                | Expression `Pow` Expression
                | Expression `Sub` Expression
                | Expression `Div` Expression           
                | Log Expression                          deriving (Show)


foldExpression :: (String -> b) -> (String -> b) -> (b -> b -> b) -> (b -> b -> b)-> (b -> b -> b)-> (b -> b -> b)-> (b -> b -> b) -> (b -> b) -> Expression-> b
foldExpression v c a m p s d l (Var xs) = v xs
foldExpression v c a m p s d l (C n) = c n
foldExpression v c a m p s d l (exp1 `Add` exp2) = (foldExpression v c a m p s d l exp1) `a` (foldExpression v c a m p s d l exp2)
foldExpression v c a m p s d l (exp1 `Mul` exp2) = (foldExpression v c a m p s d l exp1) `m` (foldExpression v c a m p s d l exp2)
foldExpression v c a m p s d l (exp1 `Pow` exp2) = (foldExpression v c a m p s d l exp1) `p` (foldExpression v c a m p s d l exp2)
foldExpression v c a m p s d l (exp1 `Sub` exp2) = (foldExpression v c a m p s d l exp1) `s` (foldExpression v c a m p s d l exp2)
foldExpression v c a m p s d l (exp1 `Div` exp2) = (foldExpression v c a m p s d l exp1) `d` (foldExpression v c a m p s d l exp2)
foldExpression v c a m p s d l (Log exp1) = l (foldExpression v c a m p s d l exp1)


--Differentiation function
diff :: Expression -> Expression
diff (Var xs) = C "1"
diff (C n) = C "0"
diff (exp1 `Add` exp2) = (diff exp1) `Add` (diff exp2)
diff (exp1 `Mul` exp2) = ((diff exp1) `Mul` exp2) `Add` ((diff exp2) `Mul` exp1)
diff (exp1 `Pow` (C n)) = (diff exp1) `Mul` ((C n) `Mul` (exp1 `Pow` ((C n) `Sub` (C "1"))))
diff (exp1 `Pow` exp2) = ((((diff exp1) `Div` exp1) `Mul` exp2) `Add` ((Log exp1) `Mul` (diff exp2))) `Mul` (exp1 `Pow` exp2) 
diff (exp1 `Sub` exp2) = (diff exp1) `Sub` (diff exp2)
diff (exp1 `Div` exp2) = (((diff exp1) `Mul` exp2) `Sub` ((diff exp2) `Mul` exp1)) `Div` (exp2 `Pow` (C "2"))
diff (Log exp1) = (diff exp1) `Div` (exp1)



type Parser a = String -> [(a,String)]



--Integer n indicates the number of unresolved open brackets
takeNextExpr :: Int -> String -> (String,String)
takeNextExpr _ [] = ([],[])
takeNextExpr 0 xs = ([],xs)
takeNextExpr n (x:xs) | x==')' && n==1 = ([], xs)
                      | x==')' = (x:(fst (takeNextExpr (n-1) xs)), snd (takeNextExpr (n-1) xs))
                      | x=='(' = (x:(fst (takeNextExpr (n+1) xs)), snd (takeNextExpr (n+1) xs))
                      | otherwise = (x:(fst (takeNextExpr n xs)), snd (takeNextExpr n xs))

atomicHandle :: String -> Expression
atomicHandle [] = C "0"
atomicHandle (x:xs) | op=='+' = (correctExpType exp1) `Add` (correctExpType exp2)
                    | op=='*' = (correctExpType exp1) `Mul` (correctExpType exp2)
                    | op=='^' && (isConst exp1) = (correctExpType exp1) `Pow` (convertToExpr exp2)
                    | op=='^' = (correctExpType exp1) `Pow` (correctExpType exp2)
                    | op=='-' = (correctExpType exp1) `Sub` (correctExpType exp2)
                    | op=='/' = (correctExpType exp1) `Div` (correctExpType exp2)
                    | op==' ' = correctExpType exp1
                         where (exp1,op,exp2) = findBinOp (x:xs)

isDigit :: Char -> Bool
isDigit c = ((fromEnum c) >= 48) && ((fromEnum c) <= 57) 

isConst :: String -> Bool
isConst xs = foldr f True xs
               where f x fl = ((isDigit x) || x=='e') && fl    

correctExpType :: String -> Expression
correctExpType xs | checkLn xs = Log (convertToExpr (drop 2 xs))
                  | (isConst xs) = C xs
                  | otherwise = Var xs

findBinOp :: String -> (String,Char,String)
findBinOp [] = ([],' ',[])
findBinOp (x:xs) | x=='+' = ([],'+',xs)
                 | x=='*' = ([],'*',xs)
                 | x=='^' = ([],'^',xs)
                 | x=='-' = ([],'-',xs)
                 | x=='/' = ([],'/',xs)
                 | otherwise = (x:(trip 0 done), mid done, trip 2 done)
                      where done = findBinOp xs

mid :: (a,b,a) -> b
mid (x,y,z) = y

trip :: Int -> (a,b,a) -> a
trip 0 (x,y,z) = x
trip 2 (x,y,z) = z

checkLn :: String -> Bool
checkLn [] = False
checkLn (_:[]) = False
checkLn (x1:(x2:xs)) = (x1=='l') && (x2=='n')
--May have to consider that they have to ln(g(x)), but should be fine

convertToExpr :: String -> Expression
convertToExpr [] = C "0"
convertToExpr (x:xs) | x=='(' && second==[] = fstExpr
                     | checkLn (x:xs) && second=="" = Log fstExpr                   --For below, we know its the first argument like this
                     | (checkLn (x:xs) || x=='(') && (head second) == '+' = (padWithLn (x:xs) fstExpr) `Add` sndExpr
                     | (checkLn (x:xs) || x=='(') && (head second) == '*' = (padWithLn (x:xs) fstExpr) `Mul` sndExpr
                     | (checkLn (x:xs) || x=='(') && (head second) == '^' = (padWithLn (x:xs) fstExpr) `Pow` sndExpr
                     | (checkLn (x:xs) || x=='(') && (head second) == '-' = (padWithLn (x:xs) fstExpr) `Sub` sndExpr
                     | (checkLn (x:xs) || x=='(') && (head second) == '/' = (padWithLn (x:xs) fstExpr) `Div` sndExpr
                     | otherwise = atomicHandle (x:xs)
                         where rest = takeNextExpr 1 (lnCaseOrNotStr (x:xs)) 
                               first = fst rest
                               second = snd rest
                               fstExpr = (convertToExpr first)
                               sndExpr = (convertToExpr (tail second))

lnCaseOrNotStr :: String -> String
lnCaseOrNotStr (x:xs) | checkLn (x:xs) = drop 3 (x:xs)
                      | otherwise = xs

padWithLn :: String -> Expression -> Expression
padWithLn xs expr | checkLn xs = Log expr
                  | otherwise = expr



--TODO: Optimize this using function comp instead of concatenation
exprToStr :: Expression -> String
exprToStr (Var xs) = xs
exprToStr (C n) = n
exprToStr (exp1 `Add` exp2) = (exprToStrBracketer exp1) ++ " + " ++ (exprToStrBracketer exp2)
exprToStr (exp1 `Mul` exp2) = (exprToStrBracketer exp1) ++ "*" ++ (exprToStrBracketer exp2)
exprToStr (exp1 `Pow` exp2) = (exprToStrBracketer exp1) ++ "^" ++ (exprToStrBracketer exp2)
exprToStr (exp1 `Sub` exp2) = (exprToStrBracketer exp1) ++ " - " ++ (exprToStrBracketer exp2)
exprToStr (exp1 `Div` exp2) = (exprToStrBracketer exp1) ++ "/" ++ (exprToStrBracketer exp2)
exprToStr (Log exp1) = "ln("++(exprToStr exp1)++")"

--Simplify atomic expressions
exprToStrBracketer :: Expression -> String
exprToStrBracketer (Var xs) = xs
exprToStrBracketer (C n) = n
exprToStrBracketer exp = '(':(exprToStr exp) ++ ")"



differentiate :: String -> String
differentiate xs = exprToStr (simp (diff (simp (convertToExpr (removeSpace xs)))))

removeSpace :: String -> String
removeSpace cs = filter (/=' ') cs


simplify :: Expression -> Expression 
simplify (exp1 `Add` (C "0")) = exp1
simplify ((C "0") `Add` exp2) = exp2
simplify (exp1 `Mul` (C "0")) = C "0"
simplify ((C "0") `Mul` exp2) = C "0"
simplify (exp1 `Mul` (C "1")) = exp1
simplify ((C "1") `Mul` exp2) = exp2
simplify ((C "0") `Pow` exp2) = C "0"
simplify (exp1 `Pow` (C "0")) = C "1"
simplify (exp1 `Pow` (C "1")) = exp1
simplify ((C "1") `Pow` exp2) = C "1"
simplify ((C "0") `Div` exp2) = C "0"
simplify (exp1 `Sub` (C "0")) = exp1
simplify (Log (C "1")) = C "0"
simplify (Log (C "e")) = C "1"
simplify ((C a) `Add` (C b)) = C (show ((read a :: Int) + (read b :: Int)))
simplify ((C a) `Sub` (C b)) = C (show ((read a :: Int) - (read b :: Int)))
simplify ((C a) `Mul` (C b)) = C (show ((read a :: Int) * (read b :: Int)))
simplify exp = exp


simp :: Expression -> Expression
simp (exp1 `Add` exp2) = simplify ((simp exp1) `Add` (simp exp2))
simp (exp1 `Mul` exp2) = simplify ((simp exp1) `Mul` (simp exp2))
simp (exp1 `Pow` exp2) = simplify ((simp exp1) `Pow` (simp exp2))
simp (exp1 `Sub` exp2) = simplify ((simp exp1) `Sub` (simp exp2))
simp (exp1 `Div` exp2) = simplify ((simp exp1) `Div` (simp exp2))
simp (Log exp1) = simplify (Log (simp exp1))
simp exp = exp 
