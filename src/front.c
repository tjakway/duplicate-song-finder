#include <stdio.h>
#include "MusicReader.h"

#define FILENAME "Its_My_Life.m4a"

int main()
{
    struct music_metadata* metadata = read_metadata(FILENAME);   
    printf("music_metadata\ntitle: %s,\tartist: %s,\talbum: %s\n",
            metadata->title, metadata->artist, metadata->album);
    printf("comment: %s,\tgenre: %s,\ttrack: %d,\n", 
            metadata->comment, metadata->genre, metadata->track);
    printf("length: %d,\tbitrate: %d,\tchannels: %d,\n",
            metadata->length, metadata->bitrate, metadata->channels);
    printf("codec: %d\n");

}
