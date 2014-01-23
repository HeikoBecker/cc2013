cmake_minimum_required (VERSION 2.6)
project (c4)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "~/c4/build/default/")

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

include_directories(
   "${CMAKE_SOURCE_DIR}"
   "${CMAKE_SOURCE_DIR}/lexer"
   "${CMAKE_SOURCE_DIR}/parser"
   "${CMAKE_SOURCE_DIR}/utils"
)

add_subdirectory(lexer)
add_subdirectory(parser)
add_subdirectory(utils)

add_executable(c4 main.cc)
target_link_libraries(c4 parser lexer utils ast semantic token)
