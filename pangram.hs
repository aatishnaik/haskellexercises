--http://exercism.io/exercises/haskell/pangram/readme

checkpan b = if ((filter (=='a') b) == "a") && ((filter (=='b') b) == "b") && ((filter (=='c') b) == "c") && ((filter (=='d') b) == "d") && ((filter (=='e') b) == "e") && ((filter (=='f') b) == "f") && ((filter (=='g') b) == "g") && ((filter (=='h') b) == "h") && ((filter (=='i') b) == "i") && ((filter (=='j') b) == "j") && ((filter (=='k') b) == "k") && ((filter (=='l') b) == "l") && ((filter (=='m') b) == "m") && ((filter (=='n') b) == "n") && ((filter (=='o') b) == "o") && ((filter (=='p') b) == "p") && ((filter (=='q') b) == "q") && ((filter (=='r') b) == "r") && ((filter (=='s') b) == "s") && ((filter (=='t') b) == "t") && ((filter (=='u') b) == "u") && ((filter (=='v') b) == "v") && ((filter (=='w') b) == "w") && ((filter (=='x') b) == "x") && ((filter (=='y') b) == "y") && ((filter (=='z') b) == "z")
    then "Pangram"
    else "Not Pangram"
main :: IO()
main = do
    print (checkpan "abcdefghijklmnopqrstuvwxyz")