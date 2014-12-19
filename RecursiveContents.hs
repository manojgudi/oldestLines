module RecursiveContents (getRecursiveContents) where
{- Minor tweak of module from Real World Haskell book
 - -}


import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)

import System.FilePath ((</>))

getRecursiveContents :: FilePath -> IO [String]

getRecursiveContents topdir = do
    names <- getDirectoryContents topdir
    let properNames = filter (`notElem` [".", "..", ".git"]) names
    paths <- forM properNames $ \name -> do 
        let path = topdir </> name
        isDirectory <- doesDirectoryExist path 
        if isDirectory
            then getRecursiveContents path 
        else return [path]
    return (concat paths)
