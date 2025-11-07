import GHC.Base (VecElem(Int16ElemRep))
import Distribution.Simple (KnownExtension(DoAndIfThenElse))
countDigits :: Int -> Int
countDigits n 
    | n < 0 = countDigits $ abs n
    | n>= 0 && n < 10 = 1
    | otherwise = 1 + countDigits(n `div` 10)

-- eta reduction -> divisorsSum : int -> int , a for: int -> int-> int->, no for 1 : int->int
-- raboti samo ako e posleden argument i otlqvo na funkciqta i otdqsno na for , toest poselden argument i ot dvete strani

--divisorsSum :: Int -> Int
--divisorsSum  = for 1 
 --   where for :: Int -> Int -> Int
  --         for i n
   --        | i == n = n
    --       | n `rem` i == 0 = i + for(i+1) n
     --      | otherwise = for(i + 1) n

toBinary :: Int -> Int
toBinary 0 = 0
toBinary n = n `rem` 2 + 10 * toBinary( n `div` 2)

--evalPolynomial :: Double -> Double -> Double -> Double
--evalPolynomial x a b = evalPolynomialIter 0 x a b
 --   where evalPolynomialIter :: Double -> Double -> Double -> Double -> Double
   --     evalPolynomialIter result a b x 
     --   | a > b = result
       -- | otherwise = evalPolynomialIter (result * x + a) (a + 1) b x

fibonacci :: Int-> Int
fibonacci 1  = 1
fibonacci 2  = 1

fibonacci n = fibonacciIter n 1 1
    where 
        fibonacciIter :: Int -> Int -> Int -> Int
        fibonacciIter i f1 f2 
            | i == n = f1 + f2
            | otherwise = fibonacciIter (i + 1) (f1 + f2) f1

palindrome :: Int->Bool
palindrome n = n == reverse' n 0
    where 
        reverse' :: Int -> Int
        reverse' n result
            | n == 0 = result
            | otherwise = reverse' (n`div`10) (result * 10 +   n `rem` 10)


--prime :: Int -> Bool
--prime 1 = False
--prime = for 2 
  --  where 
    --    for :: Int -> Int -> Bool
      --  for i n 
        --    | fromIntegral i >= sqrt $ fromIntegral n = True
          --  | n `rem` i == 0 = False
            -- | otherwise = for (i+1) n 