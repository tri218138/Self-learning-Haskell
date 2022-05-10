-- "[]" -> True
-- "[{]}" -> True
-- "({{{])" -> True
isValid :: String -> Bool
isValid ns = helper ns ""
    where       helper :: String -> String -> Bool
                helper "" "" = True;
		helper "" a = False
		helper (n:ns) a
		    | n == '(' = helper ns (a++"(")
		    | n == ')' && (drop (length a - 1) a == "(") = helper ns (init a)
		    | n == ')' = False
		    | n == '{' = helper ns (a++"{")
		    | n == '}' && (drop (length a - 1) a == "{") = helper ns (init a)
		    | n == '}' = False
		    | n == '[' = helper ns (a++"[")
		    | n == ']' && (drop (length a - 1) a == "[") = helper ns (init a)
		    | n == ']' = False
		    | otherwise = False
