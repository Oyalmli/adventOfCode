{-#LANGUAGE QuasiQuotes,TypeOperators#-}
import Text.Scanf
main = interact $ show . foldl 0 (\a w -> let take 4 w) . words

-- . map (fromEnum . (\(Just(x:+y:+c:+s:+())) -> (s!!(x-1)==c) /= (s!!(y-1)==c))




