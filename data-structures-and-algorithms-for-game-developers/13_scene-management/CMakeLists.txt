cmake_minimum_required(VERSION 3.10)
project(SceneGraph)

find_package(OpenGL REQUIRED)
find_package(FreeGLUT CONFIG REQUIRED)

add_executable(scene-graph 13-1_scene-graph.cpp)

target_link_libraries(scene-graph OpenGL::GL OpenGL::GLU FreeGLUT::freeglut)