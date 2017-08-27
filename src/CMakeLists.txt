set(SRC
    parser.cpp
    statistics.cpp
    constants.cpp
)

set(HDR
    constants.h
    parser.h
    statistics.h
)

include_directories(${PROJECT_SOURCE_DIR}/lib/fast-cpp-csv-parser)

add_library(${CMAKE_PROJECT_NAME}-lib STATIC ${SRC} ${HDR})

add_executable(${CMAKE_PROJECT_NAME} main.cpp)
target_link_libraries(${CMAKE_PROJECT_NAME} ${CMAKE_PROJECT_NAME}-lib)
target_link_libraries(${CMAKE_PROJECT_NAME} Threads::Threads)
