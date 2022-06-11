-- Ejercicio 2

five :: a -> Int 
five _ = 5

apply :: (a -> b) -> a ->b
apply f x = f x

identity :: a -> a
identity a = a

first :: (a,b)-> a
first (a,b) = a

-- derive :: Num a  => (a,a) -> a
derive 0 = 0 
derive x = (derive (x + 0.001) - derive (x)) /(0.001)


sign :: (Num a , Ord a) => a -> Int
sign x  | x < 0 = -1
        | x > 0 = 1
        | otherwise = 0


abs_sing :: Int -> Int 
abs_sing x | sign x == -1 = -x
           | sign x == 1 = x

my_abs :: (Num a , Ord a) => a -> a
my_abs x | x < 0 = -x
         | x >= 0 = x


my_xor :: Bool -> Bool -> Bool
my_xor False False = False
my_xor _ _ = True


max3 :: (Num a , Ord a) => a -> a -> a -> a
max3 x y z | (x > y && x > z) = x
           | (y > x && y > z) = y
           | otherwise = z


swap :: (a,a) -> (a,a)
swap (x,y) = (y,x)


sumsqrt :: Int -> Int 
sumsqrt 1 = 1
sumsqrt n = n^2 + sumsqrt (n-1)

-- Ejercicio 4

-- suma :: Num a => [a]-> a
suma [] = 0
suma (x:xs) = x + suma xs

my_any :: [Bool] -> Bool
my_any [x] = x
my_any (x:xs) | x == True = True
           | x == False = my_any xs


my_all :: [Bool] -> Bool
my_all [x] = x
my_all (x:xs) | x == False = False
              | x == True = my_all xs


multipy :: Num a => [a] -> a
multipy [x] = x
multipy (x:xs) = x * multipy xs


resto :: Integral a => [a]-> a -> [a]
resto [] _ = []
resto (x:xs) b = x `mod` b : resto xs b


square :: Num a => [a]->[a]
square [] = []
square (x:xs) = x*x : square xs


my_lengh :: [a]-> Int
my_lengh [] = 0
my_lengh (x:xs) = 1 + my_lengh xs


lengh_list :: [[a]] -> [Int]
lengh_list [] = []
lengh_list (x:xs) = my_lengh x : lengh_list xs


triple_second :: (Int,Int) -> Bool
triple_second (x,y) | 3*y > x = True
                    | otherwise = False


order :: [(Int,Int)] -> [(Int,Int)]
order [] = []
order (x:xs) | triple_second x = x: order xs
             | otherwise = order xs


pares :: [Int] -> [Int]
pares [] = []
pares (x:xs) | x `mod` 2 == 0 = x: pares xs
             | otherwise = pares xs 


is_member :: [Char] -> Char -> Bool
is_member [] _ = False
is_member (x:xs) a | x == a = True
                   | otherwise = is_member xs a


letras :: [Char] -> [Char]
letras [] = []
letras (x:xs) | is_member ['a'..'z'] x = x : letras xs
              | is_member ['A'..'Z'] x  = x : letras xs
              | otherwise = letras xs


masDe :: [[a]] -> Int ->[[a]]
masDe [] _ = []
masDe (x:xs) n | my_lengh x == n = x: masDe xs n
               | otherwise = masDe xs n


-- Ejercicio 1 cp 2

collaztSeq :: Int -> [Int]
collaztSeq 1 = [1]
collaztSeq x | x `mod` 2 == 0 = x: collaztSeq (x `div` 2)
              | otherwise = x : collaztSeq (3 * x + 1)


collaztSeqLeng :: Int -> Int -> [[Int]]
collaztSeqLeng n m =  masDe (map collaztSeq [1..n]) m


-- Ejercicio 2 cp2

cumplen :: (a-> Bool) -> [a]-> Bool
cumplen f [] = True
cumplen f (x:xs) | f x = cumplen f xs 
                 | otherwise = False 

-- Ejercicio 3 cp 2

my_div :: Int -> Int -> Bool
my_div _ 1 = False
my_div a b | a `mod` b == 0 = True 
           | otherwise = my_div a (b-1)


esPrimo :: Int -> Bool
esPrimo a | my_div a (a-1) = False
          | otherwise = True


elimina :: Int -> [Int] -> [Int]
elimina n xs = [x | x <- xs , x `mod` n/=0]

criba :: [Int] -> [Int]
criba []   = []
criba (n:ns) = n : criba (elimina n ns)


primos :: Int -> [Int]
primos n = criba [2..n]


producto :: [Int] -> Int
producto [] = 1
producto (x:xs) = x * producto xs


factores :: Int -> [Int]
factores 1 = [1]
factores n | esPrimo n = [1,n]
           | otherwise = divisores (primos n) n

divisores :: [Int] -> Int -> [Int]
divisores [] _ = []
divisores (x:xs) n | n `mod` x == 0 = x: divisores xs n
                   | otherwise = divisores xs n

-- Ejercicio cp 2.2
pal :: [Char] -> Bool
pal [] = True
pal (x:xs) | reverse (x:xs) == (x:xs) = True
           | otherwise = False 


longPro :: [[a]] -> Int
longPro [] = 0
longPro (x:xs) =  suma ( map my_lengh (x:xs)) `div` my_lengh (x:xs)


ave :: [Int] -> Int
ave [] = 0
ave (x:xs) = (suma (x:xs)) `div` (my_lengh (x:xs))


adyac:: [a] ->[(a,a)]
adyac [] = []
adyac [x] = []
adyac (x:y:xs) = (x,y) : adyac (y:xs) 


remDups ::Eq a => [a] -> [a]
remDups [] = []
remDups [x] = [x]
remDups (x:y:xs) | x == y = remDups (x:xs)
                 | otherwise = x: remDups (y:xs)


takeUtil :: (a -> Bool) -> [a] -> [a]
takeUtil f (x:xs) | f x == False = x:takeUtil f (xs)
                  | otherwise = []

-- Ejercicio 3 cp 2.2
data PeopleName = PeopleName (String,String,String)

data People = People PeopleName Int

liaZer :: People
liaZer = People (PeopleName "Lia"  "Zerquera" "Ferrer") 21

danielaZer :: People
danielaZer = People (PeopleName "Daniela" "Ferrer" "Ferrer") 18

lauHer :: People 
lauHer = People (PeopleName "Laurent" "Hernandez" "Ferrer") 15

getName :: People -> (String,String)
getName (People n _) = n


ifLastName :: (String , String,String) -> (String,String)
ifLastName (n ,f, s) | f == s = True
                   | otherwise = False


aps_same :: [People] -> [People]
aps_same [] = []
aps_same (x:xs) | ifLastName x =  x : aps_same xs
                | otherwise = aps_same xs