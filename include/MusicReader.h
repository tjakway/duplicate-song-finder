#ifndef MUSICREADER_H_
#define MUSICREADER_H_

#include <cstdint>
#include "CodecDefines.h"

/**
 * Everything in the extern C block is the interop between C++ and Haskell's Foreign Function Interface
 */

/* Use C linkage to guarantee compatibility with GHC FFI */
extern "C" {

    struct music_metadata
    {
        int32_t codec;
        int32_t length,
                bitrate,
                channels;
        /*This is implemented as an unsigned int in TagLib::Tag
         * but it is highly unlikely an album contains enough
         * tracks to overflow the sign bit and this means Haskell
         * doesn't have to know about unsigned integers*/
        int32_t track;
        char *title,
             *artist,
             *album,
             *comment,
             *genre;
    };

    /**
     * read metadata from file and return new music_metadata
     */
    struct music_metadata* read_metadata(char*);
}

#endif
