rule :: String -> Int -> Int -> String
rule s n num = if num `mod` n == 0 then s else mempty

fizzBuzz :: Int -> String
fizzBuzz = max <$> show <*> rule "Fizz" 3 <> rule "Buzz" 5

main = mapM_ (putStrLn . fizzBuzz) [1..100]
