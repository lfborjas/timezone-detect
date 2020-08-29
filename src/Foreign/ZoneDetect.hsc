{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Foreign.ZoneDetect where

import Foreign
import Foreign.C.Types
import Foreign.C.String

#include <zonedetect.h>

-- https://github.com/BertoldVdb/ZoneDetect/blob/05567e367576d7f3efa00083b7661a01e43dc8ca/library/zonedetect.c#L61
-- note that we define the non-Windows version. This library will not compile in Windows systems! (sorry, I just
-- don't know enough Haskell CPP directive-fu yet!)
data ZoneDetectInfo = ZoneDetectInfo
    { fd :: Int
    , length :: CInt -- as per: https://hackage.haskell.org/package/bindings-posix-1.2.4/docs/Bindings-Posix-Sys-Types.html
    , closeType :: Word8
    , mapping :: Ptr Word8
    , tableType :: Word8
    , version :: Word8
    , precision :: Word8
    , numFields :: Word8
    , notice :: CString
    , fieldNames :: Ptr CString
    , bboxOffset :: Word32
    , metadataOffset :: Word32
    , dataOffset :: Word32
    }

foreign import ccall unsafe "zonedetect.h ZDOpenDatabase"
    c_ZDOpenDatabase :: CString -> IO (Ptr ZoneDetectInfo)

foreign import ccall unsafe "zonedetect.h ZDHelperSimpleLookupString"
    c_ZDHelperSimpleLookupString :: Ptr ZoneDetectInfo -> CFloat -> CFloat -> IO CString
