#include "MusicReader.h"

#include <taglib/tag.h> //Taglib
#include <taglib/fileref.h> //Taglib
#include <cstdlib>
#include <string>
#include <algorithm> //std::copy

/**
 * copy a C++ string into a new c string
 * std::string.c_str() is not sufficient because it only has the lifetime
 * of the C++ string object
 *
 * if the C++ string is empty it will return a C string containing only a null terminator
 *
 * WARNING: DOES *NOT* SUPPORT UNICODE
 */
char* copy_to_cstr(TagLib::String taglib_string)
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
    TagLib::Tag* tag = fileRef.tag();
    metadata->title = copy_to_cstr(tag->title());
    metadata->artist = copy_to_cstr(fileRef.tag()->artist());
    metadata->album = copy_to_cstr(fileRef.tag()->album());
    metadata->comment = copy_to_cstr(fileRef.tag()->comment());
    metadata->genre = copy_to_cstr(fileRef.tag()->genre());

    //next handle audio properties
    return metadata;
   }
   catch(...)
   {
     return NULL;
   } 
}

int main() { }
