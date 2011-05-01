import Control.Monad
import qualified Data.ByteString.Lazy as B
import Data.Binary.Get
import Data.Binary.Put
import Data.Sfnt.Put
import Data.Woff.Get
import System.Environment
import System.FilePath

main = do
  args <- getArgs
  unless (length args >= 1) $ fail "Usage: woffex <in> [outprefix]"
  let input = head args
  let output = if length args == 1 then input else args !! 1
  
  woff <- fmap (runGet getWoffFile) $ B.readFile (head args)
  let sfnt = woffSfnt woff
  let sfntBits = runPut $ putSfntFile sfnt

  let ext = case sfFlavor sfnt of
        0x00010000 -> "ttf" -- Fixed-pont "1.0" - TrueType 1.0
        0x74727565 -> "ttf" -- "true" - Apple TrueType (?)
        0x74797031 -> "ttf" -- "typ1" - Apple TrueType (?)
        0x4f54544f -> "otf" -- "OTTO" - OpenType
        _ -> "sfnt"
  B.writeFile (replaceExtension output ext) sfntBits
  when (B.length (woffMetadata woff) > 0) $
    B.writeFile (replaceExtension output ".xml") $ woffMetadata woff
  when (B.length (woffPrivate woff) > 0) $
    B.writeFile (replaceExtension output ".dat") $ woffPrivate woff
