cmake_minimum_required(VERSION 3.10)
project(TextureCompression)

find_package(OpenGL REQUIRED)
find_package(FreeGLUT CONFIG REQUIRED)
find_package(GLEW REQUIRED)

add_executable(texture-compression 14-4_texture-compression.cpp)

configure_file(
    ${CMAKE_SOURCE_DIR}/image.dds
    ${CMAKE_CURRENT_BINARY_DIR}/image.dds
    COPYONLY
)

target_link_libraries(texture-compression OpenGL::GL OpenGL::GLU GLEW::GLEW FreeGLUT::freeglut)