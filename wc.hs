import Data.List

main = interact wordCount
    where wordCount input = (intercalate " " $
            [show (length $ lines input), show (length $ words input), show (length input)]
            ) ++ "\n"