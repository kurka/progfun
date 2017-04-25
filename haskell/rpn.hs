
solveRPN :: String -> Float
solveRPN str = head . foldl rpn [] . words $ str
  where rpn (x:y:ys) "*" = (y * x):ys
        rpn (x:y:ys) "+" = (y + x):ys
        rpn (x:y:ys) "-" = (y - x):ys
        rpn (x:y:ys) "/" = (y / x):ys
        rpn (x:y:ys) "^" = (y ** x):ys
        rpn (x:ys) "ln" = (log x):ys
        rpn xs "sum" = [sum xs]
        rpn xs number = read number:xs


