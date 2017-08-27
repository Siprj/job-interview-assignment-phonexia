set(SRC
    statistics.cpp
)

set(HDR
)

include_directories(${PROJECT_SOURCE_DIR}/lib/Catch/single_include)
include_directories(${PROJECT_SOURCE_DIR}/src/)

add_executable(${CMAKE_PROJECT_NAME}-test main.cpp ${SRC} ${HDR})
target_link_libraries(${CMAKE_PROJECT_NAME}-test ${CMAKE_PROJECT_NAME}-lib)

add_test(NAME test COMMAND ${CMAKE_PROJECT_NAME}-test)
