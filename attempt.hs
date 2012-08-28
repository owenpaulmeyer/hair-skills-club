import Control.Monad
import Char

sublist [] _ = True
sublist _ [] = False
sublist (x:xs) (y:ys)
	|x==y = sublist xs ys
	|x/=y = sublist (x:xs) ys

subseq [] _ = True
subseq (_:_) [] = False
subseq xs yy@(y:ys) =  frontseq xs yy || subseq xs ys

frontseq [] _ = True
frontseq (_:_) [] = False
frontseq (x:xs) (y:ys) = (x==y) && frontseq xs ys

getLines = liftM lines . readFile

main = do
    list <- getLines "dictionary.txt"
    mapM_ putStrLn list


isVowel x = 
	x=='a'||
	x=='e'||
	x=='i'||
	x=='o'||
	x=='u'||
	x=='A'||
	x=='E'||
	x=='I'||
	x=='O'||
	x=='U'||
	x=='y'||
	x=='Y'

num1 = liftM (((filter (subseq "tantan")).(map(map toLower)))) $ getLines "linux.words"

num2 = liftM
	(filter  (\x -> length x >= 7).
	filter (\x -> length(filter isVowel x)==1).
	filter (\x -> length(filter (=='s') x)==0).
	filter (\x -> isLower (head x))) $ getLines "linux.words"

