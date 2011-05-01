module Data.Sfnt.Types
       (
         SfntFile(..)
       , SfntTable(..)
       ) where

import qualified Data.ByteString.Lazy as B
import Data.Word

data SfntFile = SfntFile { sfFlavor :: Word32
                         , sfTables :: [SfntTable] }
                deriving (Show)
                
data SfntTable = SfntTable { sftTag :: Word32
                           , sftChecksum :: Word32
                           , sftData :: B.ByteString }
                 deriving (Show)