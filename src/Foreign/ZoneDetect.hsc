{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Foreign.ZoneDetect where

import Foreign
import Foreign.C.Types
import Foreign.C.String

#include <zonedetect.h>

-- | Opaque pointer to the underlying C struct:
-- https://wiki.haskell.org/FFI_cook_book#Passing_opaque_structures.2Ftypes
-- https://github.com/BertoldVdb/ZoneDetect/blob/05567e367576d7f3efa00083b7661a01e43dc8ca/library/zonedetect.h
data ZoneDetectInfo = ZoneDetectInfo

foreign import ccall unsafe "zonedetect.h ZDOpenDatabase"
    c_ZDOpenDatabase :: CString -> IO (Ptr ZoneDetectInfo)

foreign import ccall unsafe "zonedetect.hs ZDCloseDatabase"
    c_ZDCloseDatabase :: Ptr ZoneDetectInfo -> IO ()

foreign import ccall unsafe "zonedetect.h ZDHelperSimpleLookupString"
    c_ZDHelperSimpleLookupString :: Ptr ZoneDetectInfo -> CFloat -> CFloat -> IO CString
