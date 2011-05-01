module Data.Woff.Types
       (
         WoffFile(..)
       ) where

import qualified Data.ByteString.Lazy as B
import Data.Sfnt.Types

data WoffFile = WoffFile { woffSfnt :: SfntFile
                         , woffMetadata :: B.ByteString
                         , woffPrivate :: B.ByteString }
                deriving (Show)
