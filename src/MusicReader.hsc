{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module MusicReader
( Codec,
  Metadata,
  readMusicMetadata
) where

import Control.Monad
import Foreign
import Foreign.C.Types
import Foreign.C.String
import System.IO.Unsafe as Unsafe

#include "CodecDefines.h"
#include "MusicReader.h"

constantToCodec code
                    | code == mp3 = MP3
                    | code == flac = FLAC
                    | code == ogg = OGG_VORBIS
                    | code == mp4 = MP4
                    | code == mpeg = MPEG
                    | code == none = NONE
                    | code == unknown = UNKNOWN
                    | otherwise = UNKNOWN
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

instance Storable Metadata where
    sizeOf _ = (#size struct music_metadata)
    alignment _ = alignment (undefined::CDouble)
    peek a = do
        codec <- liftM constantToCodec $ (((#peek struct music_metadata, codec) a) :: IO Int)
        length <- ((#peek struct music_metadata, length) a) :: IO Int
        bitrate <- ((#peek struct music_metadata, bitrate) a) :: IO Int
        channels <- ((#peek struct music_metadata, channels) a) :: IO Int
        track <- ((#peek struct music_metadata, bitrate) a) :: IO Int
        title <-  ((#peek struct music_metadata, title) a) :: IO CString
        artist <- ((#peek struct music_metadata, artist) a) :: IO CString
        album <- ((#peek struct music_metadata, album) a) :: IO CString
        comment <- ((#peek struct music_metadata, comment) a) :: IO CString
        genre <- ((#peek struct music_metadata, genre) a) :: IO CString
        --FIXME: find replacement for temporary names
        marshalledTitle <- peekCString title
        marshalledArtist <- peekCString artist
        marshalledAlbum <- peekCString album
        marshalledComment <- peekCString comment
        marshalledGenre <- peekCString genre
        return (Metadata codec length bitrate channels track marshalledTitle marshalledArtist marshalledAlbum marshalledComment marshalledGenre)
    poke a = undefined

--This is the "primitive" FFI call--calls the C function and gets a pointer
--in return
--TODO: write a higher level function this module should export that calls
--primReadMusicMetadata and converts the C Pointer into the Haskell data
--Metadata
foreign import ccall unsafe "read_metadata" primReadMusicMetadata :: CString -> IO (Ptr Metadata)

--convert the Haskell string to a CString, call into the FFI then
--dereference the resulting pointer
readMusicMetadata a = Unsafe.unsafePerformIO $ 
                        withCString a $ \cs -> ((liftM peek) $ primReadMusicMetadata cs)
