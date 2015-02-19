#ifndef CODEC_DEFINES_H_
#define CODEC_DEFINES_H_

//use an int to store codec type instead of an enum because we need to know the byte offsets in the struct and enums are technically not always ints
//see http://stackoverflow.com/questions/1113855/is-the-sizeofenum-sizeofint-always
#define NO_EXTENSION -1
#define UNKNOWN_EXTENSION -2
#define MP3_CODEC 1
#define FLAC_CODEC 2
#define OGG_VORBIS_CODEC 3
#define MP4_CODEC 4
#define MPEG_CODEC 5

#endif
