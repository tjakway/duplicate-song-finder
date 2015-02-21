{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module MusicReader
( Codec,
  Metadata
) where

import Foreign
import Foreign.C.Types

#include "CodecDefines.h"

constantToCodec code
                    | code == mp3 = MP3
                    | code == flac = FLAC
                    | code == ogg = OGG_VORBIS
                    | code == mp4 = MP4
                    | code == mpeg = MPEG
                    | code == none = NONE
                    | code == unknown = UNKNOWN
                       where mp3 = #const MP3_CODEC
                             flac = #const FLAC_CODEC
                             ogg = #const OGG_VORBIS_CODEC 
                             mp4 = #const MP4_CODEC
                             mpeg = #const MPEG_CODEC
                             none = #const NO_EXTENSION
                             unknown = #const UNKNOWN_EXTENSION


data Codec = MP3 | FLAC | OGG_VORBIS | MP4 | MPEG | NONE | UNKNOWN deriving (Show)

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



