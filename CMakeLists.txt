project(duplicate-music-finder-cpp)

cmake_minimum_required(VERSION 2.8.0)

#find taglib
list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake/modules)

find_package(Taglib REQUIRED)

if(NOT ${TAGLIB_FOUND})
 message(SEND_ERROR "Taglib not found!")
endif()

include_directories(${TAGLIB_CFLAGS})

include_directories("${CMAKE_SOURCE_DIR}/include")

set(SRC "${CMAKE_SOURCE_DIR}/src")

add_library(MusicReader
    ${SRC}/MusicReader.cpp)
