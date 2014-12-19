{-# LANGUAGE ScopedTypeVariables #-}
import Control.Exception
import Data.List (isInfixOf)
import HSH (run)

{-
 - Remember Textdifference between IO String and IO (String)
 - IO (String) is a function which does IO and returns a String
 - IO String function returns type IO String
 - -}
--getAllCommitHash = run $ "git --no-pager log" :: IO String
getAllCommitHash :: IO String
--getAllCommitHash = run $ "git --no-pager -C ../../supa/ log" :: IO String
getAllCommitHash = run $ "git --no-pager log" :: IO String

getTrackedFiles :: IO String
getTrackedFiles = run $ "git ls-files" :: IO String

isOldestHash :: String -> String -> Bool
-- Match the first ten digits of oldest hash
isOldestHash oldestHash blameLine = if (isInfixOf (take 10 oldestHash) blameLine) then  True else  False

--getAllLines 
getAllOldestLine :: String -> String -> IO [String]
getAllOldestLine oldestHash myFile = do 
    --let rawGitBlame = run $ "git --no-pager -C ../../supa/ " ++ " blame -l " ++ myFile
    let rawGitBlame = run $ "git --no-pager " ++ " blame -l " ++ myFile :: IO String 
    gitBlameContent <- rawGitBlame

    return [ blameLine | blameLine <- lines gitBlameContent, isOldestHash oldestHash blameLine  ]

-- Checks if the string contains commit Hash
isCommitString :: String -> Bool
isCommitString x = not $ ( isInfixOf "commit" x ) && ( (length x) == 47 )

-- Takes raw output string of git log and gives out the oldest Hash
-- drop 7 drops the initial word "commit " text
--extractOldestHash :: Either SomeException String -> String
extractOldestHash :: String -> String
extractOldestHash commitHashString = drop 7 $ ( dropWhile isCommitString  $ reverse $ lines commitHashString ) !! 0


main :: IO ()
main = do 
            myString <- getAllCommitHash
            allFiles <- fmap lines getTrackedFiles

            out <- mapM (getAllOldestLine (extractOldestHash myString)) allFiles
            putStrLn "OUT "
            print out
