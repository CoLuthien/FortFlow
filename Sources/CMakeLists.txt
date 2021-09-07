
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
        ${CMAKE_CURRENT_SOURCE_DIR}/CUDA/*.f90)
else()
    file(GLOB_RECURSE sources
        ${CMAKE_CURRENT_SOURCE_DIR}/*.f90)
endif()

# Build library
if (USE_CUDA)
    cuda_add_library(${target} ${sources} STATIC
        OPTIONS ${CUDA_TARGET_OPTIONS}
    )
else()
    add_library(${target} STATIC
        ${sources}
        )
endif()

# Project options
#set_target_properties(${target}
#    PROPERTIES
#    ${DEFAULT_PROJECT_OPTIONS}
#)

# Compile options
target_compile_options(${target}
    PRIVATE

    PUBLIC
    ${DEFAULT_COMPILE_OPTIONS}

    INTERFACE
)

target_link_libraries(${target}
    ${SUBMODULES}

    PUBLIC
    PRIVATE

    INTERFACE
)

# Install
install(TARGETS ${target} DESTINATION lib)
install(DIRECTORY ${header_dir} DESTINATION include)