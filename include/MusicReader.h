#ifndef MUSICREADER_H_
#define MUSICREADER_H_

#include <cstdint>

/**
 * Everything in the extern C block is the interop between C++ and Haskell's Foreign Function Interface
 */

/* Use C linkage to guarantee compatibility with GHC FFI */
extern "C" {

//use an int to store codec type instead of an enum because we need to know the byte offsets in the struct and enums are technically not always ints
//see http://stackoverflow.com/questions/1113855/is-the-sizeofenum-sizeofint-always
#define NO_EXTENSION -1
#define UNKNOWN_EXTENSION -2
#define MP3_CODEC 1
#define FLAC_CODEC 2
#define OGG_VORBIS_CODEC 3
#define MP4_CODEC 4
#define MPEG_CODEC 5

    struct music_metadata
    {
        int32_t codec;
        int32_t length,
                bitrate,
                channels;
        //on x86, pointers will be 4 bytes
        //on AMD64, pointers will be 8 bytes
        char *title,
             *artist,
             *album,
             *comment,
             *genre;
        /*This is implemented as an unsigned int in TagLib::Tag
         * but it is highly unlikely an album contains enough
         * tracks to overflow the sign bit and this means Haskell
         * doesn't have to know about unsigned integers*/
        int32_t track;
    };

    /**
     * read metadata from file and return new music_metadata
     */
    struct music_metadata* read_metadata(char*);
}

#endif
