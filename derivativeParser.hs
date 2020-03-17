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
diff (exp1 `Pow` exp2) = ((((diff exp1) `Div` exp1) `Mul` exp2) `Add` ((Log exp1) `Mul` (diff exp1))) `Mul` (exp1 `Pow` exp2) 
diff (exp1 `Sub` exp2) = (diff exp1) `Sub` (diff exp2)
diff (exp1 `Div` exp2) = (((diff exp1) `Mul` exp2) `Sub` ((diff exp2) `Mul` exp1)) `Div` (exp2 `Pow` (C 2))
diff (Log exp1) = (diff exp1) `Div` (exp1)



--AHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHhh


type Parser a = String -> [(a,String)]



--Integer n indicates the number of unresolved open brackets
takeNextExpr :: Int -> String -> (String,String)
takeNextExpr _ [] = ([],[])
takeNextExpr 0 xs = ([],xs)
takeNextExpr n (x:xs) | x==')' = (x:(fst (takeNextExpr (n-1) xs)), snd (takeNextExpr (n-1) xs))
                      | x=='(' = (x:(fst (takeNextExpr (n+1) xs)), snd (takeNextExpr (n+1) xs))
                      | otherwise = (x:(fst (takeNextExpr n xs)), snd (takeNextExpr n xs))

convertToExpr :: String -> Expression
convertToExpr [] = C 0
convertToExpr (x:xs) | x=='(' = 
	                       where rest = takeNextExpr 1 xs

removeSpace :: String -> String
removeSpace cs = filter (/=' ') cs
