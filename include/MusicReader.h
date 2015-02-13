#ifndef MUSICREADER_H_
#define MUSICREADER_H_

/* Use C linkage to guarantee compatibility with GHC FFI */
extern "C" {

    enum codec
    {
        MP3 = 1,
        FLAC = 2,
        OGG_VORBIS = 3,
        MP4 = 4,
        MPEG = 5
    };

    struct music_metadata
    {
        enum codec codec_type;
        int length,
            bitrate,
            channels;
        char *title,
             *artist,
             *album,
             *comment,
             *genre;
        /*This is implemented as an unsigned int in TagLib::Tag
         * but it is highly unlikely an album contains enough
         * tracks to overflow the sign bit and this means Haskell
         * doesn't have to know about unsigned integers*/
        int track;
    };

}

#endif
