module Data.Sfnt.Put
       (
         module Data.Sfnt.Types,
         putSfntFile
       ) where

import Control.Monad
import Data.Binary.Put
import qualified Data.ByteString.Lazy as B
import Data.Sfnt.Types

putTableDirent :: Integral a => a -> SfntTable -> PutM a
putTableDirent offset table = do
  putWord32be $ sftTag table
  putWord32be $ sftChecksum table
  putWord32be $ fromIntegral $ offset
  let length = B.length $ sftData table
  putWord32be $ fromIntegral $ length
  let unaligned = offset + (fromIntegral length)
  return $ unaligned + (4 - mod unaligned 4)

putTable :: SfntTable -> Put
putTable table = do
  putLazyByteString $ sftData table
  let length = fromIntegral $ B.length $ sftData table
  replicateM_ (4 - mod length 4) $ putWord8 0

putSfntFile :: SfntFile -> Put
putSfntFile file = do
  putWord32be $ sfFlavor file  
  let tables = sfTables file
  putWord16be $ fromIntegral $ length tables
  let sr = (truncate $ logBase 2 $ fromIntegral $ length tables) * 16
  putWord16be $ fromIntegral $ sr
  putWord16be $ fromIntegral $ truncate $ logBase 2 $ fromIntegral $ div sr 16
  putWord16be $ fromIntegral $ (length tables) * 16 - sr
  foldM_ putTableDirent (12 + 16 * length tables) tables
  mapM_ putTable tables
  
