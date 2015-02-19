{-# LANGUAGE CPP, LANGUAGE ForeignFunctionInterface #-}

import Foreign
import Foreign.C.Types

#include "CodecDefines.h"

constantToCodec code = case code of mp3 -> MP3
                                    flac -> FLAC
                                    ogg -> OGG_VORBIS
                                    mp4 -> MP4
                                    mpeg -> MPEG
                                    none -> NONE
                                    unknown -> UNKNOWN
                       where mp3 = #const MP3_CODEC
                             flac = #const FLAC_CODEC
                             ogg = #const OGG_VORBIS_CODEC 
                             mp4 = #const MP4_CODEC
                             mpeg = #const MPEG_CODEC
                             none = #const NO_EXTENSION
                             unknown = #const UNKNOWN_EXTENSION


data Codec = MP3 | FLAC | OGG_VORBIS | MP4 | MPEG | NONE | UNKNOWN

data Metadata = Metadata { codec :: Codec,
                      length :: Int,
                      bitrate :: Int,
                      channels :: Int,
                      track :: Int,
                      title :: String,
                      artist :: String,
                      album :: String,
                      comment :: String,
                      genre :: String } deriving (Show)
