module Main where

import           Control.Applicative
import           Control.Exception   (IOException, catch)
import           Control.Monad
import           Data.Char           (ord)
import           Data.Picture
import qualified Data.PNM            as PNM
import           Prelude             hiding (catch)
import           System.Environment
import           System.IO           (hPutStr, stderr)

printPNM :: FilePath -> IO ()
printPNM path = do
    putStrLn "Loading pix"
    pix <- PNM.load path
    print pix

printFile :: FilePath -> IO ()
printFile path = do
    content <- catch (readFile path)
                     (\e -> do let err = show (e :: IOException)
                               hPutStr stderr ("Warning: Couldn't open " ++ path ++ ": " ++ err)
                               return "")
    putStrLn content

main1 :: IO ()
main1 = do
    (path:_) <- getArgs
    putStrLn $ "Reading file " ++ path
    printFile path

main2 :: IO ()
main2 = do
    (path:_) <- lines `liftM` getContents
    putStrLn $ "Reading file " ++ path
    printFile path

main3 :: IO ()
main3 = do
    (path:_) <- lines `liftM` getContents
    print $ map ord path
    print $ map ord "essai.pnm"

-- | main entry point
main :: IO ()
main = do
    {-
    args <- getArgs
    path <- getPath args
    -}
    path <- getArgs >>= getPath
    printPNM path
 where -- getPath = [String] -> IO String
       getPath [] = getFirstContents
       getPath (s:_) = return s
       -- getFirstContents = IO String
       getFirstContents = head `liftM` lines `liftM` getContents


