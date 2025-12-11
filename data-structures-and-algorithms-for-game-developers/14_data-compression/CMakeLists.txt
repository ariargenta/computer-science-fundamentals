cmake_minimum_required(VERSION 3.10)
project(TextureCompression)

find_package(OpenGL REQUIRED)
find_package(FreeGLUT CONFIG REQUIRED)
find_package(GLEW REQUIRED)

add_executable(texture-compression 14-4_texture-compression.cpp)

target_link_libraries(texture-compression OpenGL::GL OpenGL::GLU GLEW::GLEW FreeGLUT::freeglut)