
set(target FortFlow)

# Define
set(root_dir ${CMAKE_CURRENT_SOURCE_DIR}/../..)

# Includes
#include_directories(
#    ${CMAKE_CURRENT_SOURCE_DIR}
#    ${root_dir}/toml-f
#    
#)

# Sources

if (USE_CUDA)
    file(GLOB_RECURSE sources
        ${CMAKE_CURRENT_SOURCE_DIR}/*.f90
        ${CMAKE_CURRENT_SOURCE_DIR}/CUDA/*.cuf)
    set_source_files_properties(${sources} PROPERTIES LANGUAGE Fortran)
else()
    file(GLOB_RECURSE sources
        ${CMAKE_CURRENT_SOURCE_DIR}/*.f90)
endif()

# Build library
add_library(${target} STATIC
    ${sources}
)


# Project options
#set_target_properties(${target}
#    PROPERTIES
#    ${DEFAULT_PROJECT_OPTIONS}
#)
set_target_properties(${target} PROPERTIES POSITION_INDEPENDENT_CODE ON)
set_target_properties(${target} PROPERTIES CUDA_SEPARABLE_COMPILATION ON)

# Compile options
target_compile_options(${target}
    PRIVATE
    PUBLIC
    "-cuda"

    INTERFACE
)

target_link_libraries(${target}
    PUBLIC
    ${CUDA_RUNTIME_LIBRARY}
    ${CUDA_LIBRARY}
    ${SUBMODULES}
    ${DEFAULT_LINKER_OPTIONS}

    PRIVATE

    INTERFACE
)

message(${sources})

# Install
install(TARGETS ${target} DESTINATION lib)
install(DIRECTORY ${header_dir} DESTINATION include)