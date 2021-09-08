# CMake version
cmake_minimum_required(VERSION 3.13 FATAL_ERROR)
cmake_policy(SET CMP0063 NEW) # visibility

# Include cmake modules
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/Builds/CMake")

# Declare project
project(FortFlow LANGUAGES CUDA Fortran C)

# CUDA
option(USE_CUDA "Use CUDA features" OFF)
option(USE_OMP "Use OMP features" ON)
find_package(Threads)

# Useful paths
set(LIBRARY_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Libraries)

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/build/mod)

# Set output directories
#set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib)
set(DEFAULT_CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY})

# Set enable output of compile commands during generation
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Build type - Release by default
if (NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
elseif (CMAKE_BUILD_TYPE MATCHES "Debug")
    add_definitions(-DDEBUG)
endif()

if (USE_CUDA)
    find_package(CUDA)
    enable_language(CUDA)
endif()


# Compile options
include(BuildScript/CMake/FortranCompileOptions.cmake)
# ugly part to set submodule compile  flags
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${DEFAULT_COMPILE_OPTIONS}")

# Add submodules
include(BuildScript/CMake/AddSubmodules.cmake)


message(STATUS "CMake build type: ${CMAKE_BUILD_TYPE}")



# Project modules

## ThirdParty modules
add_subdirectory(Sources)
#
add_subdirectory(Examples/HelloCFD)
#if (USE_CUDA)
#    #add_subdirectory(Examples/HelloCuda)
#endif()
