#
# Platform and architecture setup
#

# Set warnings as errors flag
option(FORTFLOW_WARNINGS_AS_ERRORS "Treat all warnings as errors" ON)

# Get upper case system name
string(TOUPPER ${CMAKE_SYSTEM_NAME} SYSTEM_NAME_UPPER)

# Determine architecture (32/64 bit)
set(X64 OFF)
if(CMAKE_SIZEOF_VOID_P EQUAL 8)
    set(X64 ON)
endif()

#
# Include directories
#
set(DEFAULT_INCLUDE_DIRECTORIES)

#
# Libraries
#

set(DEFAULT_LIBRARIES
    PUBLIC
    PRIVATE
)

#
# Compile definitions
#

set(DEFAULT_COMPILE_DEFINITIONS
    SYSTEM_${SYSTEM_NAME_UPPER}
)


#
# Compile options
#

set(DEFAULT_COMPILE_OPTIONS)

if (CMAKE_Fortran_COMPILER_ID MATCHES "NVHPC")
    set(DEFAULT_COMPILE_OPTIONS ${DEFAULT_COMPILE_OPTIONS}
    -Mbackslash
    )

endif()



#
# Linker options
#

set(DEFAULT_LINKER_OPTIONS)

# Use pthreads on mingw and linux
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU" OR CMAKE_SYSTEM_NAME MATCHES "Linux")
    set(DEFAULT_LINKER_OPTIONS
        -pthread
    )
endif()

# Code coverage - Debug only
# NOTE: Code coverage results with an optimized (non-Debug) build may be misleading
if(CMAKE_BUILD_TYPE MATCHES Debug AND CMAKE_Fortran_COMPILER_ID MATCHES Intel)
    set(DEFAULT_COMPILE_OPTIONS ${DEFAULT_COMPILE_OPTIONS}
        -g
        -O0
    )

    set(DEFAULT_LINKER_OPTIONS ${DEFAULT_LINKER_OPTIONS}
        -fprofile-arcs
        -ftest-coverage
    )
endif()


