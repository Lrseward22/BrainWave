cmake_minimum_required (VERSION 3.20.0)

if(CMAKE_SOURCE_DIR STREQUAL CMAKE_CURRENT_SOURCE_DIR)
    project(calc)

    set(CMAKE_CXX_STANDARD 17)

    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-rtti")
    set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -g")
    set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -g")

    add_executable(calc src/driver.cpp)
    target_sources(calc PRIVATE
        src/lib/Utils/Diagnostics.cpp
        src/lib/Utils/TokenKinds.cpp
        src/lib/Lexer/Lexer.cpp
        src/lib/Parser/Parser.cpp
        src/lib/Generator/CodeGen.cpp
    )
endif()

find_package(LLVM REQUIRED CONFIG)
message("Found LLVM ${LLVM_PACKAGE_VERSION}, build type ${LLVM_BUILD_TYPE}")
list(APPEND CMAKE_MODULE_PATH ${LLVM_DIR})

separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})
target_include_directories(calc PUBLIC ${PROJECT_SOURCE_DIR}/src/include)
llvm_map_components_to_libnames(LLVM_LIBS
    Core
    Support
    IRReader
    Passes
    Analysis
    AsmParser
    AsmPrinter
    BitReader
    BitWriter
    CodeGen
    InstCombine
    Instrumentation
    Interpreter
    Linker
    Object
    Option
    ProfileData
    Symbolize
    ScalarOpts
    Target
    TransformUtils
    Vectorize
    native
)

#llvm_map_components_to_libnames(LLVM_LIBS
#    Core
#    Support
#    IRReader
#    Passes
#    Analysis
#    AsmParser
#    AsmPrinter
#    BitReader
#    BitWriter
#    CodeGen
#    InstCombine
#    Instrumentation
#    Interpreter
#    Linker
#    Object
#    Option
#    ProfileData
#    Symbolize
#    ScalarOpts
#    Target
#    TransformUtils
#    Vectorize
#    AllTargetAsmParsers
#    AllTargetsCodeGens
#    AllTargetsDescs
#    AllTargetsInfos
#)

target_link_libraries(calc ${LLVM_LIBS})

#add_subdirectory ("src")
