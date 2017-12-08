getlast :: [a] -> a
getlast [x] = x
getlast (_:xs) = getlast xs

getslast (x:xs) = if (length) xs > 1
    then getslast xs
    else x

rply :: String -> IO()
rply x = if x == ""
        then putStrLn "Fine!"
    else if (getlast x) == '?'
        then putStrLn "sure"
    else if ((getlast x) == '!') && ((getslast x) == '!') 
        then putStrLn "Woah!"
    else if (getlast x) == '!'
        then putStrLn "Whatever!"
    else putStrLn "fine!"

main :: IO()
main = do
    rply "abc!!"

    
