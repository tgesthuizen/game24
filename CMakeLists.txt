cmake_minimum_required(VERSION 3.0.2)

project(Game24 C)

set(CMAKE_C_STANDARD 99)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS OFF)

add_executable(game24 game24.c)
add_executable(game24it1 iteration1.c)
add_executable(game24it3 iteration3.c)

enable_testing()
add_subdirectory(tests)
