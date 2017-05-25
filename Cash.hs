import Total

showTotal s = let 
    [q,p] = words s
    in show (total (read q) (read p))

main = interact (unlines . map showTotal . lines)
