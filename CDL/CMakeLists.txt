# CMakeList.txt : CMake project for CDL, include source and define
# project specific logic here.
#
cmake_minimum_required (VERSION 3.8)

find_package(Microsoft.GSL CONFIG)

set(CMAKE_CXX_CPPLINT "cpplint")

# Add source to this project's executable.
add_executable (CDL "CDL.cpp" "CDL.h" "persistent/chunked_array.hpp" "persistent/detail/helpers.hpp" "persistent/hamt.hpp" "persistent/map.hpp" "persistent/list.hpp" "persistent/vector.hpp")

target_link_libraries(CDL PRIVATE Microsoft.GSL::GSL)
set_property(TARGET CDL PROPERTY CXX_STANDARD 20)

# TODO: Add tests and install targets if needed.
