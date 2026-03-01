add_library(bw_runtime STATIC
    String/string.c
    # Array/array.c
    # Math/math.c ...
)

target_include_directories(bw_runtime PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

set_target_properties(bw_runtime PROPERTIES
    LANGUAGE C
    C_STANDARD 11
)
