import Data.List
import HSH


{-
 - Remember Textdifference between IO String and IO (String)
 - IO (String) is a function which does IO and returns a String
 - IO String function returns type IO String
 - -}
getAllCommitHash :: IO String
--getAllCommitHash = run $ "git --no-pager log" :: IO String
getAllCommitHash = run $ "git --no-pager -C ../../supa/ log" :: IO String

-- Checks if the string contains commit Hash
isCommitString :: String -> Bool
isCommitString x = not $ ( isInfixOf "commit" x ) && ( (length x) == 47 )

-- Takes raw output string of git log and gives out the oldest hash
extractOldestHash :: String -> String
extractOldestHash commitHashString = ( dropWhile isCommitString  $ reverse $ lines commitHashString ) !! 0


main :: IO ()
main = do
            myString <- getAllCommitHash
            putStrLn $ "Oldest Hash "  ++  extractOldestHash myString
