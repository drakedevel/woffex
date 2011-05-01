module Data.Woff.Get
       (       
         module Data.Woff.Types
       , module Data.Sfnt.Types
       , getWoffFile
       ) where

import Codec.Compression.Zlib
import Control.Monad
import Data.Binary.Get
import qualified Data.ByteString.Lazy as B
import Data.Sfnt.Types
import Data.Woff.Types
import Data.Word

data FileHeader = FileHeader { hFlavor :: Word32
                             , hLength :: Word32
                             , hNumTables :: Word16
                             , hTotalSfntSize :: Word32
                             , hMajorVersion :: Word16
                             , hMinorVersion :: Word16
                             , hMetaOffset :: Word32
                             , hMetaLength :: Word32
                             , hMetaOrigLength :: Word32
                             , hPrivOffset :: Word32
                             , hPrivLength :: Word32
                             }
                  deriving (Show)

data TableDirent = TableDirent { dTag :: Word32
                               , dOffset :: Word32
                               , dCompLength :: Word32
                               , dOrigLength :: Word32
                               , dOrigChecksum :: Word32 }
                 deriving (Show)

gFileHeader :: Get FileHeader
gFileHeader = do
  signature <- getWord32be
  unless (signature == 0x774F4646) $ fail "Invalid signature."
  flavor <- getWord32be
  length <- getWord32be
  numTables <- getWord16be
  reserved <- getWord16be
  unless (reserved == 0) $ fail "Invalid reserved field."
  totalSfntSize <- getWord32be
  majorVersion <- getWord16be
  minorVersion <- getWord16be
  metaOffset <- getWord32be
  metaLength <- getWord32be
  metaOrigLength <- getWord32be
  privOffset <- getWord32be
  privLength <- getWord32be  
  return $ FileHeader flavor length numTables totalSfntSize majorVersion
    minorVersion metaOffset metaLength metaOrigLength privOffset privLength
  -- FIXME: Reject if meta/private fields overlap or are too large

gTableDirent :: Get TableDirent
gTableDirent = do
  tag <- getWord32be
  offset <- getWord32be
  compLength <- getWord32be
  origLength <- getWord32be
  origChecksum <- getWord32be
  return $ TableDirent tag offset compLength origLength origChecksum

gBytes :: Integral a => a -> a -> a -> Get B.ByteString
gBytes offset size origSize = do
  pos <- bytesRead
  skip $ fromIntegral offset - fromIntegral pos
  
  maybeCompBits <- getLazyByteString $ fromIntegral size
  let bits = if size == origSize
             then maybeCompBits
             else decompress maybeCompBits
  unless (fromIntegral origSize == B.length bits) $ fail "Table truncated."
  return bits

gTable :: TableDirent -> Get SfntTable
gTable ent = do
  bits <- gBytes (dOffset ent) (dCompLength ent) (dOrigLength ent)
  return $ SfntTable (dTag ent) (dOrigChecksum ent) bits

gMeta :: FileHeader -> Get B.ByteString
gMeta hdr = gBytes (hMetaOffset hdr) (hMetaLength hdr) (hMetaOrigLength hdr)

gPriv :: FileHeader -> Get B.ByteString
gPriv hdr = gBytes (hPrivOffset hdr) (hPrivLength hdr) (hPrivLength hdr)

getWoffFile :: Get WoffFile
getWoffFile = do
  header <- gFileHeader
  tableDir <- replicateM (fromIntegral $ hNumTables header) gTableDirent
  tables <- mapM (fmap lookAhead gTable) tableDir
  meta <- lookAhead $ gMeta header
  priv <- lookAhead $ gPriv header
  let sfnt = SfntFile (hFlavor header) tables
  return $ WoffFile sfnt meta priv