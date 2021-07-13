import Data.List ((\\), elemIndex, intersect, nub)
import Data.Bifunctor (bimap, first)
 
combs 0 _ = [[]]
combs _ [] = []
combs k (x:xs) = ((x :) <$> combs (k - 1) xs) ++ combs k xs
 
toposort :: [(String, String)] -> [String]
toposort xs
  | (not . null) cycleDetect =
    error $ "Dependency cycle detected for libs " ++ show cycleDetect
  | otherwise = foldl makePrecede [] dB
  where
    dB = (\(x, y) -> (x, y \\ x)) . bimap return words <$> xs
    makePrecede ts ([x], xs) =
      nub $
      case elemIndex x ts of
        Just i -> uncurry (++) $ first (++ xs) $ splitAt i ts
        _ -> ts ++ xs ++ [x]
    cycleDetect =
      filter ((> 1) . length) $
      (\[(a, as), (b, bs)] -> (a `intersect` bs) ++ (b `intersect` as)) <$>
      combs 2 dB
