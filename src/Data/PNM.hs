module Data.PNM
    ( load
    ) where

import           Control.Monad               (liftM, when)

import qualified Data.ByteString.Lazy        as L
import qualified Data.Map                    as Map

import           Data.Char                   (digitToInt, ord)

--import Text.Parsec.Prim
import           Text.Parsec.Combinator      (chainl1, count, eof, lookAhead,
                                              manyTill, optional, sepEndBy)

import           Text.Parsec.Char            (anyChar, char, digit, newline,
                                              oneOf, spaces)

import           Text.Parsec.Error           (ParseError, errorMessages,
                                              messageString)

import           Text.Parsec.ByteString.Lazy (Parser, parseFromFile)

import           Data.Picture

-- Portable Any Map
data Type = PPM -- Color image format
          | PGM -- Grayscale image format
          | PBM -- Bi-level image format

data Encoding = ASCII
              | BINARY

toRGB :: [Int] -> [Color]
toRGB (r:g:b:others) = (RGB r g b) : toRGB others
toRGB _ = []

{- Basic parser -}
digitInt :: Parser Int
digitInt = digit >>= (return . digitToInt)

natural :: Parser Int
natural =  digitInt `chainl1` return (\m n -> 10*m + n)

oneOrZero :: Parser Int
oneOrZero = oneOf "01" >>= (return . digitToInt)

magicNumber :: Parser Int
magicNumber = char 'P' >> oneOf "123456" >>= (return . digitToInt)

word8 :: Parser Int
word8 = anyChar >>= (return . ord)

{- Parse the magic number to get the type and the encoding of the PNM file -}

getFormat :: Parser (Type, Encoding)
getFormat = do spaces
               m <- magicNumber
               return $ selectPNM m
    where selectPNM m = pnms !! (m-1)
          pnms = zip [PBM,   PGM,   PPM,   PBM,    PGM,    PPM ]
                     [ASCII, ASCII, ASCII, BINARY, BINARY, BINARY]

{- header parsers -}

headerPNM :: Type -> Parser Header
headerPNM PPM = headerPPM
headerPNM PGM = headerPGM
headerPNM PBM = headerPBM

headerPPM :: Parser Header
headerPPM = do spaces
               width <- natural
               spaces
               height <- natural
               spaces
               maxValue <- natural
               return $ Header width height maxValue

headerPGM :: Parser Header
headerPGM = headerPPM

headerPBM :: Parser Header
headerPBM = do width <- natural
               height <- natural
               return $ Header width height 1

{- values parser -}

rawValues :: Encoding -> Parser [Int]
rawValues ASCII = optional spaces >> natural `sepEndBy` spaces
rawValues BINARY = newline >> (word8 `manyTill` eof)

{- raster parser -}

rasterPNM :: Type -> Encoding -> Header -> Parser (Raster Color)
rasterPNM t enc (Header w h m) =
    do values <- rawValues enc
       let build = Map.fromList $ zip [ (1,i) | i <- [1..size] ] $ toRGB values
           size = length values
       return build


{-
rasterPNM1 :: Type -> Encoding -> Header -> Parser (Raster Color)
rasterPNM1 t ASCII (Header w h m) = liftM build raw
    where build = Map.fromList . ( zip [ (i,m-j) | j <- [1..h], i <- [1..w] ] ) . concat
          raw = count h (spaces >> line)
          line = case t of
                 PPM -> count w color
                 _   -> count w grey
          color = do [r,g,b] <- count 3 value
                     return $ RGB r g b
          grey  = do v <- value
                     return $ RGB v v v
          value = do spaces
                     v <- lookAhead natural
                     when (v > m) (fail $ errorMsg v m)
                     natural
                     return v
          errorMsg v m = "Raster error " ++ show v ++ " is greater than " ++ show m
rasterPNM1 t BINARY (Header w h m)  = do newline
                                         [r,g,b] <- count 3 word8
                                         return (Map.fromList [((0,0),RGB r g b)])
-}

{- PNM parser -}

parserPNM :: Parser (Picture Color)
parserPNM = do (t,e) <- getFormat   -- get the type and the encoding via the magic number
               h <- headerPNM t     -- get the complete header
               r <- rasterPNM t e h -- get the raster
               return $ Picture h r

load :: String -> IO (Picture Color)
load s = do c <- parseFromFile parserPNM s
            either throwErrorMsg return c
    where throwErrorMsg = error . messageString . head . errorMessages

{-
         #####   #    #  #    #  #####
         #    #  #    #  ##  ##  #    #
         #    #  #    #  # ## #  #    #
         #    #  #    #  #    #  #####
         #    #  #    #  #    #  #
         #####    ####   #    #  #
-}

{-
-- Header loader -- test purpose only
loadH :: String -> IO (Either ParseError Header)
loadH s = parseFromFile parserHeader s
    where parserHeader = do (t,e) <- getFormat
                            headerPNM t
-}

{-
load :: FilePath -> IO (Picture Color)
load s = openFile s ReadMode >>= hGetContents >>= treatPix
    where
        treatPix = either throwErrorMsg return . parsePixFrom
        parsePixFrom = parse parserPNM "erreur"
        throwErrorMsg = error . messageString . head . errorMessages
-}

{-
parserP1 :: Parser (Picture Int)
parserP1 = do string "P1"
              return $ Picture 1 1 0 Map.empty

parserP2 :: Parser (Picture Int)
parserP2 = do string "P2"
              return $ Picture 1 1 0 Map.empty
-}


