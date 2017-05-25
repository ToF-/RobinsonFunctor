import Total

main = do
    q <- getLine
    p <- getLine
    putStrLn $ show $ total (read q) (read p)
