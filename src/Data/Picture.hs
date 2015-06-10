module Data.Picture (
      Header (..)
    , Raster (..)
    , Picture (..)
    , Color (..)
    ) where

import           Data.Map (Map)

data Header = Header { width    :: Int
                     , height   :: Int
                     , maxValue :: Int }
    deriving (Read,Eq,Ord)

instance Show Header where
    show (Header w h m)  = show (w,h,m)

type Raster e = Map (Int,Int) e

data Picture e = Picture { header :: Header
                         , raster :: Raster e }
    deriving (Show,Read,Eq,Ord)

data Color = RGB {-# UNPACK #-} !Int !Int !Int
    deriving (Show,Read,Eq,Ord)
