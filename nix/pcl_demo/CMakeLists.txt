cmake_minimum_required(VERSION 3.10 FATAL_ERROR)

project(pcl_visualizer_viewports)

find_package(PCL 1.2 REQUIRED)
find_package(Boost REQUIRED COMPONENTS thread)

include_directories(${PCL_INCLUDE_DIRS})
link_directories(${PCL_LIBRARY_DIRS})
add_definitions(${PCL_DEFINITIONS})

add_executable (pcl_visualizer_demo pcl_visualizer_demo.cpp)
target_link_libraries (pcl_visualizer_demo ${PCL_LIBRARIES} ${Boost_LIBRARIES} pthread)
install(TARGETS pcl_visualizer_demo RUNTIME DESTINATION "bin")
