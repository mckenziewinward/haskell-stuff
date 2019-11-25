module AlgebraicDatatypes.ChapterExercises where

import Data.Char 
import Data.Ord
import Data.Monoid (Any(..))
import Data.List
import Test.Hspec

{-
1. Given the following datatype : -} 
data Weekday = 
    Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
{-we can say : 
a)Weekday is a type with five data constructors
b)Weekday is a tree with five branches
c)Weekday is a product type
d)Weekday takes five arguments
a

2.and with the same datatype definition in mind, what is the type of the following function, f? -}
f :: Weekday -> String
f Friday = "Miller Time"
f _  = "Not Miller Time"
{-
a) f :: [Char]
b) f :: String -> String
c) f :: Weekday -> String
d) f :: Day -> Beer
c

3. Types defined with the data keyword
a) must have at least one argument
b) must begin with a capital letter
c) must be polymorphic
d) cannot be imported from modules
b

4. The function -}
g :: [a] -> a
g xs = xs !! (length xs - 1)
{-
a) is recursive and may not terminate
b) delivers the head of xs
c) delivers the final element of xs
d) has the same type as xs
c
-}

--Vigenere cipher
encode :: String -> String -> String
encode msg keyword = 
    addSpaces (removePunc msg) $ zipWith enc (upperLetters msg) (cycle keyword)
    where enc l r = chr $ (ord l + ord r) `mod` 26 + ord 'A'
          removePunc = filter (getAny . (Any . isAlpha <> Any . isSpace))

decode :: String -> String -> String
decode msg keyword =
    addSpaces msg $ zipWith dec (upperLetters msg) (cycle keyword)
    where dec l r = chr $ (ord l - ord r) `mod` 26 + ord 'A'

upperLetters :: String -> String
upperLetters = map toUpper . filter isLetter
        
addSpaces :: String -> String -> String
addSpaces _ [] = []
addSpaces [] _ = []
addSpaces (' ':xs) (y:ys) =  ' ' : addSpaces xs (y:ys)
addSpaces (_:xs) (y:ys) = y : addSpaces xs ys

testVigenere :: Spec
testVigenere = 
    describe "Vigenere test" $ do
        it "\"MEET AT DAWN\" should encode to \"MPPR AE OYWY\"" $
            encode "MEET AT DAWN" "ALLY" `shouldBe` "MPPR AE OYWY"
        it "\"MPPR AE OYWY\" should decode to \"MEET AT DAWN\"" $
            decode "MPPR AE OYWY" "ALLY" `shouldBe` "MEET AT DAWN"

--As Patterns 
{-
1. This should return True if (and only if) all the values in 
   the first list appear in the second list, though they need
   not be contiguous. -}
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool 
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf p@(x:xs) (y:ys) 
    | x == y = isSubseqOf xs ys
    | otherwise = isSubseqOf p ys 
{-
The following are examples of how this function should work:
-}
testIsSubseqOf :: Spec
testIsSubseqOf =
    describe "IsSubseqOf" $ do
        it "blah is a subsequence of blahwoot" $
            isSubseqOf "blah" "blahwoot" `shouldBe` True
        it "blah is a subsequence of wootblah" $
            isSubseqOf "blah" "wootblah" `shouldBe` True
        it "blah is a subsequence of wboloath" $
            isSubseqOf "blah" "wboloath" `shouldBe` True
        it "blah is a subsequence of wootbla" $
            isSubseqOf "blah" "wootbla" `shouldBe` False
        it "blah is a subsequence of halbwoot" $
            isSubseqOf "blah" "halbwoot" `shouldBe` False
        it "blah is a subsequence of blawhoot" $
            isSubseqOf "blah" "blawhoot" `shouldBe` True
{-
2. Split a sentence into words, then tuple each word with the
   capitalized form of each. -}
capitalizeWords :: String -> [(String,String)]
capitalizeWords str = upperPair <$> words str
        where upperPair [] = ("", "")
              upperPair word@(w:ws) = (word, toUpper w : ws)

testCapitalizeWords :: Spec
testCapitalizeWords =
    describe "Capitalize Words" $
        it "should capitalize words and put in a list of tuples" $
            capitalizeWords "hello world" `shouldBe` [("hello", "Hello"), ("world", "World")]

{-Language exercises
1. Write a function that capitalizes a word. -}
capitalizeWord :: String -> String 
capitalizeWord [] = ""
capitalizeWord (x:xs) = toUpper x : xs
{-Example output.
Prelude> capitalizeWord "Chortle"
"Chortle"
Prelude> capitalizeWord "chortle"
"Chortle"-}
{-
2. Write a function that capitalizes sentences in a paragraph.
   Recognize when a new sentence has begun by checking
   for periods. Reuse the capitalizeWord function. -}
capitalizeParagraph :: String -> String
capitalizeParagraph = capitalizeWord . unwords . go . words
    where go [] = []
          go [x] = [x]
          go (x:y:zs) 
            | last x == '.' = x : go (capitalizeWord y : zs)
            | otherwise     = x : go (y : zs)
{-Example result you should get from your function:
Prelude> let s = "blah. woot ha."
Prelude> capitalizeParagraph s 
"Blah. Woot ha."-}

testLanguageExercises :: Spec
testLanguageExercises =
    describe "Language Exercises" $ do
        it "should leave a capitalized word alone" $
            capitalizeWord "Chortle" `shouldBe` "Chortle"
        it "should capitalize a word" $
            capitalizeWord "chortle" `shouldBe` "Chortle"
        it "should capitalize a paragraph" $
            capitalizeParagraph "blah. woot ha." `shouldBe` "Blah. Woot ha."

{-Phone Exercise-}
newtype DaPhone = DaPhone [(Char, String)]

convo :: [String]
convo = 
    [ "Wanna play 20 questions"
    , "Ya"
    , "U 1st haha"
    , "Lol ok. Have u ever tasted alcohol"
    , "Lol ya"
    , "Wow ur cool haha. Ur turn"
    , "Ok. Do u think I am pretty Lol"
    , "Lol ya"
    , "Just making sure rofl ur turn"
    ]

type Digit = Char

type Presses = Int

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps phone char
    | isUpper char = ('*', 1) : [look phone (toLower char)]
    | otherwise = [look phone char]
    where look (DaPhone []) _ = (' ', 0)
          look (DaPhone ((key, val) : kvs)) c = 
                case elemIndex c val of
                    Just i -> (key, i + 1)
                    Nothing -> look (DaPhone kvs) c

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone = concatMap (reverseTaps phone)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\(_, p) a -> a + p) 0 

mostPopularLetter :: String -> Char
mostPopularLetter = head 
                  . maximumBy (comparing length) 
                  . group 
                  . sort 
                  . map toLower
                  . filter isAlpha

coolestLetter :: [String] -> Char
coolestLetter = mostPopularLetter 
              . concat

coolestWord :: [String] -> String
coolestWord = head 
            . maximumBy (comparing length) 
            . group 
            . sort 
            . map (map toLower) 
            . concatMap words

oldPhone :: DaPhone
oldPhone = DaPhone
    [ ('1', "1")
    , ('2', "abc2")
    , ('3', "def3")
    , ('4', "ghi4")
    , ('5', "jkl5")
    , ('6', "mno6")
    , ('7', "pqrs7")
    , ('8', "tuv8")
    , ('9', "wxyz9")
    , ('0', " 0")
    , ('*', "^")
    , ('#', ".,") ]

testPhone :: Spec
testPhone =
    describe "Phone exercise" $ do
        it "head convo should create this sequence of button presses" $
            cellPhonesDead oldPhone (head convo) `shouldBe` [('*', 1), ('9', 1), ('2', 1), ('6', 2), ('6', 2), ('2', 1), ('0', 1), ('7', 1), ('5', 3), ('2', 1), ('9', 3), ('0', 1), ('2', 4), ('0', 2), ('0', 1), ('7', 2), ('8', 2), ('3', 2), ('7', 4), ('8', 1), ('4', 3), ('6', 3), ('6', 2), ('7', 4)]
        it "should return that 'a' is the coolest letter" $
            coolestLetter convo `shouldBe` 'a'
        it "should return that \"lol\" is the coolest word" $
            coolestWord convo `shouldBe` "lol"

{-Hutton's razor-}

data Expr =
    Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit i) = i
eval (Add e1 e2) = eval e1 + eval e2

printExpr :: Expr -> String
printExpr (Lit i) = show i
printExpr (Add e1 e2) = printExpr e1 ++ " + " ++ printExpr e2

testHutton :: Spec
testHutton =
    describe "Hutton's razor" $ do
        it "eval (Add (Lit 1) (Lit 9001)) should be 9002" $
            eval (Add (Lit 1) (Lit 9001)) `shouldBe` 9002
        it "should print the Expr correctly" $ 
            printExpr (Add (Lit 9001) (Lit 1)) `shouldBe` "9001 + 1"
        it "should print the Exprs correctly" $ do
            let a1 = Add (Lit 9001) (Lit 1)
                a2 = Add a1 (Lit 20001)
                a3 = Add (Lit 1) a2
            printExpr a3 `shouldBe` "1 + 9001 + 1 + 20001"

test :: IO ()
test = hspec $ do
    testVigenere
    testIsSubseqOf
    testCapitalizeWords
    testLanguageExercises
    testPhone
    testHutton