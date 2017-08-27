cmake_minimum_required(VERSION 2.8)

project("job-interview-assignment-phonexia")

set(vision_version 0.1)

set (CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall")
set(THREADS_PREFER_PTHREAD_FLAG ON)
find_package(Threads REQUIRED)

SET(EXECUTABLE_OUTPUT_PATH ${PROJECT_SOURCE_DIR}/bin)

enable_testing()

add_subdirectory(src)
add_subdirectory(test)
