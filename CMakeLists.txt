cmake_minimum_required(VERSION 2.8)

project("job-interview-assignment-phonexia")

set(vision_version 0.1)

set (CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall")
set(THREADS_PREFER_PTHREAD_FLAG ON)
find_package(Threads REQUIRED)

SET(EXECUTABLE_OUTPUT_PATH ${PROJECT_SOURCE_DIR}/bin)

include_directories(${PROJECT_SOURCE_DIR}/fast-cpp-csv-parser)

add_subdirectory(src)
