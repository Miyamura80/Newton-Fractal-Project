data Expression = Var String
                | C Int
                | Expression `Add` Expression
                | Expression `Mul` Expression
                | Expression `Pow` Expression
                | Expression `Sub` Expression
                | Expression `Div` Expression           
                | Log Expression                          deriving (Show)


--Differentiation function
diff :: Expression -> Expression
diff (Var xs) = (C 1)
diff (C n) = C 0
diff (exp1 `Add` exp2) = (diff exp1) `Add` (diff exp2)
diff (exp1 `Mul` exp2) = ((diff exp1) `Mul` exp2) `Add` ((diff exp2) `Mul` exp1)
diff (exp1 `Pow` exp2) = ((((diff exp1) `Div` exp1) `Mul` exp2) `Add` ((Log exp1) `Mul` (diff exp2))) `Mul` (exp1 `Pow` exp2) 
diff (exp1 `Sub` exp2) = (diff exp1) `Sub` (diff exp2)
diff (exp1 `Div` exp2) = (((diff exp1) `Mul` exp2) `Sub` ((diff exp2) `Mul` exp1)) `Div` (exp2 `Pow` (C 2))
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
atomicHandle [] = C 0
atomicHandle (x:xs) | op=='+' = (correctExpType exp1) `Add` (correctExpType exp2)
                    | op=='*' = (correctExpType exp1) `Mul` (correctExpType exp2)
                    | op=='^' = (correctExpType exp1) `Pow` (correctExpType exp2)
                    | op=='-' = (correctExpType exp1) `Sub` (correctExpType exp2)
                    | op=='/' = (correctExpType exp1) `Div` (correctExpType exp2)
                    | op==' ' = correctExpType exp1
                         where (exp1,op,exp2) = findBinOp (x:xs)

isDigit :: Char -> Bool
isDigit c = ((fromEnum c) >= 48) && ((fromEnum c) <= 57) 

isConst :: String -> Bool
isConst xs = foldr f True xs
               where f x fl = (isDigit x) && fl    

correctExpType :: String -> Expression
correctExpType xs | checkLn xs = Log (convertToExpr (drop 2 xs))
                  | (isConst xs) = C (read xs::Int)
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
convertToExpr [] = C 0
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


--Optimize in a sec, after git commit, might just be able to handle
--ln case in the atomic handler

--TODO: Optimize this using function comp instead of concatenation
exprToStr :: Expression -> String
exprToStr (Var xs) = xs
exprToStr (C n) = show n
exprToStr (exp1 `Add` exp2) = '(':(exprToStr exp1) ++ ") + (" ++ (exprToStr exp2) ++ ")"
exprToStr (exp1 `Mul` exp2) = '(':(exprToStr exp1) ++ ") * (" ++ (exprToStr exp2) ++ ")"
exprToStr (exp1 `Pow` exp2) = '(':(exprToStr exp1) ++ ") ^ (" ++ (exprToStr exp2) ++ ")"
exprToStr (exp1 `Sub` exp2) = '(':(exprToStr exp1) ++ ") - (" ++ (exprToStr exp2) ++ ")"
exprToStr (exp1 `Div` exp2) = '(':(exprToStr exp1) ++ ") / (" ++ (exprToStr exp2) ++ ")"
exprToStr (Log exp1) = "ln("++(exprToStr exp1)++")"


differentiate :: String -> String
differentiate xs = exprToStr (diff (convertToExpr xs))

removeSpace :: String -> String
removeSpace cs = filter (/=' ') cs
