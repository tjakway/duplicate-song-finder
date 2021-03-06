#include "MusicReader.h"

#include <taglib/tag.h>
#include <taglib/fileref.h>
#include <taglib/audioproperties.h>
#include <cstdlib>
#include <string>
#include <algorithm> //std::copy
#include <cassert>
#include "CodecDefines.h"


/**
 * returns the extension of the passed file or NULL if it doesn't have one
 * see http://stackoverflow.com/questions/51949/how-to-get-file-extension-from-string-in-c
 *
 * WARNING: THIS WILL FAIL IF THE FILE HAS NO EXTENSION AND THE DIRECTORY CONTAINS A PERIOD (then again, TagLib detects filetype from extensions so it'll fail too)
 */
static int get_extension(const char* c_file_str)
{
    std::string filename(c_file_str);

    //get the position of the last period in the filename
    size_t last_dot = filename.find_last_of(".");
    //if there is no period it doesn't have an extension
    if(last_dot == std::string::npos)
        return NO_EXTENSION;

    std::string extension = filename.substr(filename.find_last_of(".") + 1);
    if(extension == "mp3")
    {
        return MP3_CODEC;
    }
    else if(extension == "flac")
    {
        return FLAC_CODEC;   
    }
    else if(extension == "ogg")
    {
        return OGG_VORBIS_CODEC;
    }
    else if(extension == "mp4")
    {
        return MP4_CODEC;
    }
    else if(extension == "mpeg")
    {
        return MPEG_CODEC;
    }
    else
        return UNKNOWN_EXTENSION;
}

/**
 * copy a C++ string into a new c string
 * std::string.c_str() is not sufficient because it only has the lifetime
 * of the C++ string object
 *
 * if the C++ string is empty it will return a C string containing only a null terminator
 *
 * this function is essentially strdup
 *
 * WARNING: DOES *NOT* SUPPORT UNICODE
 */
static char* copy_to_cstr(TagLib::String taglib_string)
{
    std::string cpp_string = taglib_string.to8Bit(false);

    //use malloc, not new
    //leave 1 extra place for the null terminator
    char* str = (char*) malloc(sizeof(char) * cpp_string.length() + 1);
    std::copy(cpp_string.begin(), cpp_string.end(), str);
    str[cpp_string.size()] = '\0';

    return str;
}

/**
 * WARNING: THIS FUNCTION DOES NO ERROR CHECKING
 */
struct music_metadata* read_metadata(char* filename)
{
    //allocate with malloc instead of new so Haskell can deallocate with
    //free (C standard library functions are much easier to call from Haskell
    //than C++)
    music_metadata* metadata = (music_metadata*) malloc(sizeof(music_metadata));
       
   try
   {    
    TagLib::FileRef fileRef(filename);
    if(fileRef.isNull())
    {
        //FIXME: handle error, probably by throwing an exception
        assert(1);
    }


    TagLib::Tag* tag = fileRef.tag();
    metadata->title = copy_to_cstr(tag->title());
    metadata->artist = copy_to_cstr(tag->artist());
    metadata->album = copy_to_cstr(tag->album());
    metadata->comment = copy_to_cstr(tag->comment());
    metadata->genre = copy_to_cstr(tag->genre());
    metadata->track = tag->track();

    TagLib::AudioProperties* properties = fileRef.audioProperties();
    metadata->length = properties->length();
    metadata->bitrate = properties->bitrate();
    metadata->channels = properties->channels();

    //get the file type
    //this is very simplistic: it only checks the filename extension 
    metadata->codec = get_extension(filename);

    //next handle audio properties
    return metadata;
   }
   catch(...)
   {
       //FIXME: need a strategy for propagating errors from C -> Haskell
       assert(1);
     return NULL;
   } 
}

