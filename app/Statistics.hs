module Statistics where

mean :: [Int] -> Double
mean xs = s / n
  where
    s = fromIntegral $ sum xs
    n = fromIntegral $ length xs

var :: [Int] -> Double
var xs = ss / (n - 1)
  where
    m = mean xs
    ss = sum $ (\x -> (fromIntegral x - m) ** 2) <$> xs
    n = fromIntegral $ length xs

sd :: [Int] -> Double
sd = sqrt . var
