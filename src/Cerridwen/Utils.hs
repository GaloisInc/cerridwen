{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module Utils where
import Data.Word
import qualified Data.Map as Map
import           Data.Binary.Get (Get)
import qualified Data.Binary.Get as Get
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ElfEdit as Elf
import qualified Data.Foldable as F
import qualified System.IO as IO
import qualified Data.Macaw.Memory as MM
import qualified Data.Macaw.Memory.ElfLoader as MEL

-- From Symbolic/Testing.hs
loadELF :: Elf.ElfHeaderInfo w
        -> IO (MM.Memory w, [MEL.MemSymbol w])
loadELF ehi = do
  let loadOpts = MEL.defaultLoadOptions
  case MEL.resolveElfContents loadOpts ehi of
    Left _err -> error "in loadELF"
    Right (warnings, mem, _mentry, nameAddrList) -> do
      F.forM_ warnings $ \w -> do
        IO.hPutStrLn IO.stderr w
      return (mem, nameAddrList)

-- From Symbolic/Testing.hs
toAddrSymMap :: MM.Memory w
             -> [MEL.MemSymbol w]
             -> Map.Map (MEL.MemSegmentOff w) BS.ByteString
toAddrSymMap _mem nameAddrList =
  Map.fromList [ (MEL.memSymbolStart msym, MEL.memSymbolName msym)
               | msym <- nameAddrList
               ]     

-- From Elf-edit.Util (adapted)
strictRunGet :: Get.Get a
                   -> BS.ByteString
                   -> a
strictRunGet m bs =
  case Get.pushEndOfInput (Get.pushChunk (Get.runGetIncremental m) bs) of
    Get.Fail _ pos msg ->  error $ "strictRunGet at position " ++ show pos ++ ": " ++ msg
    Get.Partial _cont -> error $ "internal error: Get partial failed."
    Get.Done _ _ r -> r

-- Used in Corpus to serialize the hash
intBE :: Int -> BSB.Builder
intBE i = BSB.int64BE (fromIntegral i) 


getIntBE :: Get Int
getIntBE = do
  i <- Get.getInt64be
  return (fromIntegral i)



-- Used in Corpus to encode True or False
getBoolBS :: Get Bool
getBoolBS = do
  i <- Get.getWord8
  return (maxBound == i)
    
boolBS :: Bool -> BSB.Builder
boolBS True = BSB.word8 maxBound
boolBS False = BSB.word8 minBound


-- Map the space of hashes into unsigned integers
hashW64 :: Int -> Word64
hashW64 i = fromIntegral i

