project(duplicate-music-finder-cpp)

cmake_minimum_required(VERSION 2.8.0)

#find taglib
list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake/modules)

find_package(Taglib REQUIRED)

if(NOT ${TAGLIB_FOUND})
 message(SEND_ERROR "Taglib not found!")
endif()


#main library target

include_directories(${TAGLIB_CFLAGS})

include_directories("${CMAKE_SOURCE_DIR}/include")

set(SRC "${CMAKE_SOURCE_DIR}/src")

add_library(MusicReader SHARED
    ${SRC}/MusicReader.cpp)

target_link_libraries(MusicReader ${TAGLIB_LIBRARIES})


#test target

#google test is not meant to be installed locally, see https://code.google.com/p/googletest/wiki/FAQ#Why_is_it_not_recommended_to_install_a_pre-compiled_copy_of_Goog
#instead download at compile time, see http://stackoverflow.com/questions/9689183/cmake-googletest
