doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

tripleMe x = x + x + x

doubleSmallNumber x = if x > 100
                        then x
                        else x*2

doubleSmallNumber' x = (if x > 100
                        then x
                        else x*2) + 1

tripleSmallNumber x = if x > 100 then x else x*3

conanO'Brien = "¡Soy yo, Conan O'Brien!"

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial1 :: Integer -> Integer
factorial1 n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r

lucky :: (Integral a) => a -> String
lucky 7 = "El siete de la suerte!"
lucky x = "Lo siento, no es tu dia de la suerte!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "Uno!"
sayMe 2 = "Dos!"
sayMe 3 = "Tres!"
sayMe 4 = "Cuatro!"
sayMe 5 = "Cinco!"
sayMe x = "No entre 1 y 5"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)

addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

head' :: [a] -> a
head' [] = error "¡Hey, no puedes utilizar head con una lista vacía!"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell []       = "La lista esta vacia"
tell (x:[])   = "La lista tiene un elemento: " ++ show x
tell (x:y:[]) = "La lista tiene dos elementos: " ++ show x ++ " y " ++ show y
tell (x:y:_)  = "La lista es larga. Los primeros elementos son: " ++ show x ++ " y " ++ show y

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital "" = "Una cadena vacia!!"
capital all@(x:_) = "La primera letra de " ++ all ++ " es " ++ [x]

bmiTell' :: (RealFloat a) => a -> String
bmiTell' bmi
 | bmi <= 18.5 = "Tienes infrapeso. Eres emo??"
 | bmi <= 25.0 = "Supuestamente eres normal... Espero que seas feo."
 | bmi <= 30.0 = "Estas gordo!! Pierde algo de peso gordito."
 | otherwise   = "Enhorabuena, eres una ballena!!"

bmiTell'' :: (RealFloat a) => a -> a -> String
bmiTell'' weight height
 | weight / height ^ 2 <= 18.5 = "Tienes infrapeso. Eres emo??"
 | weight / height ^ 2 <= 25.0 = "Supuestamente eres normal... Espero que seas feo."
 | weight / height ^ 2 <= 30.0 = "Estás gordo!! Pierde algo de peso gordito."
 | otherwise                   = "Enhorabuena, eres una ballena!!"

max' :: (Ord a) => a -> a -> a
max' a b
 | a > b     = a
 | otherwise = b

enelmedio :: (Ord a) => a -> a -> a -> a
enelmedio a b c
 | a < b && a > c = a
 | a < c && a > b = a
 | b < a && b > c = b
 | b < c && b > a = b
 | c < a && c > b = c
 | c < b && c > a = c

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
 | a > b     = GT
 | a == b    = EQ
 | otherwise = LT

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
 | bmi <= skinny = "Tienes infrapeso ¿Eres emo?"
 | bmi <= normal = "Supuestamente eres normal... Espero que seas feo."
 | bmi <= fat    = "Estas gordo! Pierde algo de peso gordito."
 | otherwise     = "Enhorabuena, eres una ballena!!"
 where bmi = weight / height ^ 2
       skinny = 18.5
       normal = 25.0
       fat = 30.0

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in  sideArea + 2 * topArea

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

calcBmisF :: (RealFloat a) => [(a, a)] -> [a]
calcBmisF xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

describeList :: [a] -> String
describeList xs = "La lista es " ++ case xs of [] -> "una lista vacia."
                                               [x] -> "una lista unitaria."
                                               xs -> "una lista larga."

describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
 where what [] = "empty."
       what [x] = "a singleton list."
       what xs = "a longer list."       -- Hay que alinear bien los 'whats' si no no funciona

fibo n = f (n - 1) + f (n - 2)
    where f 0 = 0
          f 1 = 1

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Máximo de una lista vacía"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs

maxiMax :: (Ord a) => [a] -> a
maxiMax []      = error "maximumnmn of empty list"
maxiMax [x]     = x
maxiMax (x:xs)  = x `max` (maxiMax xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n - 1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0    = []
take' _ []     = []
take' n (x:xs)  = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) :zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = a `elem` xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted  = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted

-- lo mismo que lo anterior, pero usando "filter"
quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) =
    let smallerSorted = quicksort' (filter (<=x) xs)
        biggerSorted = quicksort' (filter (>x) xs)
    in smallerSorted ++ [x] ++ biggerSorted

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

multTwoWithNine = multThree 9

multWithEighteen = multTwoWithNine 2

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

pruebeSustr = (subtract 4) --no se puede hacer "-4", para haskell quiere decir núm negativo
                           -- también podría se ((-) 4)                           

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd n = n:chain (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

longChains :: Int -> Int
longChains = length.chain

sum'' :: (Num a) => [a] -> a
sum'' xs = foldl (\acc x -> acc + x) 0 xs

sum''' :: (Num a) => [a] -> a
sum''' = foldl (+) 0

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) +1

oddSquareSum :: Integer
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

oddSquareSum' :: Integer
oddSquareSum' = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

oddSquareSum'' :: Integer
oddSquareSum'' =
    let oddSquares = filter odd $ map (^2) [1..]
        belowLimit = takeWhile (<10000) oddSquares
    in sum belowLimit
