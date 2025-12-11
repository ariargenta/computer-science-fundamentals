#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <gl/glew.h>
#include <gl/glut.h>
#include <gl/glu.h>

enum DDS_COMPRESSION {DDS_NULL = 0, DDS_DXT1, DDS_DXT3, DDS_DXT5};

struct ImageInfoDDS {
    ImageInfoDDS() : m_width(0), m_height(0), m_type(DDS_NULL), m_components(0), m_numMipMaps(0) {}

    int m_width;
    int m_height;
    DDS_COMPRESSION m_type;
    int m_components;
    int m_numMipMaps;
};

unsigned char* LoadDDS(char* file, ImageInfoDDS& info);

#ifndef MAKE4CC
    #define MAKE4CC(ch0, ch1, ch2, ch3) \
        ((unsigned long)(unsigned char)(ch0) \
        | ((unsigned long)(unsigned char)(ch1) << 8) \
        | ((unsigned long)(unsigned char)(ch2) << 16) \
        | ((unsigned long)(unsigned char)(ch3) << 24))
#endif

#define DS_FOURCC_DXT1 (MAKE4CC('D', 'X', 'T', '1'))
#define DS_FOURCC_DXT2 (MAKE4CC('D', 'X', 'T', '2'))
#define DS_FOURCC_DXT3 (MAKE4CC('D', 'X', 'T', '3'))
#define DS_FOURCC_DXT4 (MAKE4CC('D', 'X', 'T', '4'))
#define DS_FOURCC_DXT5 (MAKE4CC('D', 'X', 'T', '5'))

enum ENDIAN {ENDIAN_UNKNOWN = 0, ENDIAN_LITTLE, ENDIAN_BIG};

ENDIAN GetEndian() {
    unsigned long data = 0x12345678;
    unsigned char* ptr = (unsigned char*)& data;

    if(*ptr == 0x12 && *(ptr + 1) == 0x34 && *(ptr + 2) == 0x56 && *(ptr + 3) == 0x78) {
        return ENDIAN_BIG;
    }
    else if(*ptr == 0x78 && *(ptr + 1) == 0x56 && *(ptr + 2) == 0x34 && *(ptr + 3) == 0x12) {
        return ENDIAN_LITTLE;
    }

    return ENDIAN_UNKNOWN;
}

void SwapBytes(char* data, int size) {
    assert((size & 1) == 0);

    char* ptr = data;
    char temp = 0;

    for(int i = 0, j = size - 1; i < size / 2; ++i, --j) {
        temp = ptr[i];
        ptr[i] = ptr[j];
        ptr[j] = temp;
    }
}

unsigned char* LoadDDS(char* file, ImageInfoDDS& info) {
    const int ddsHeightOffset = 12;
    const int ddsWidthOffset = 16;
    const int ddsLinearSizeOffset = 20;
    const int ddsMipMapNumOffset = 28;
    const int ddsFourCCOffset = 84;
    const int ddsImageDataOffset = 128;

    ENDIAN e = GetEndian();
    bool byteSwap = false;

    if(e == ENDIAN_BIG) {
        byteSwap = true;
    }

    FILE* fp = fopen(file, "rb");

    if(fp == NULL) {
        return NULL;
    }

    char imageID[5] = {0};

    fread(imageID, 1, 4, fp);

    imageID[4] = '\0';

    if(strncmp(imageID, "DDS ", 4) != 0) {
        fclose(fp);

        return false;
    }

    unsigned int dwHeight = 0;
    unsigned int dwWidth = 0;
    unsigned int dwLinearSize;
    unsigned int dwMipMaps = 0;
    unsigned int dwFourCC = 0;

    fseek(fp, ddsHeightOffset, SEEK_SET);
    fread(&dwHeight, sizeof(unsigned int), 1, fp);

    if(byteSwap == true) {
        SwapBytes((char*)& dwHeight, sizeof(unsigned int));
    }

    fseek(fp, ddsWidthOffset, SEEK_SET);
    fread(&dwWidth, sizeof(unsigned int), 1, fp);

    if(byteSwap == true) {
        SwapBytes((char*)& dwWidth, sizeof(unsigned int));
    }

    fseek(fp, ddsLinearSizeOffset, SEEK_SET);
    fread(&dwLinearSize, sizeof(unsigned int), 1, fp);

    if(byteSwap == true) {
        SwapBytes((char*)& dwLinearSize, sizeof(unsigned int));
    }

    fseek(fp, ddsMipMapNumOffset, SEEK_SET);
    fread(&dwMipMaps, sizeof(unsigned int), 1, fp);

    if(byteSwap == true) {
        SwapBytes((char*)& dwMipMaps, sizeof(unsigned int));
    }

    fseek(fp, ddsFourCCOffset, SEEK_SET);
    fread(&dwFourCC, sizeof(unsigned int), 1, fp);

    if(byteSwap == true) {
        SwapBytes((char*)& dwFourCC, sizeof(unsigned int));
    }

    if(dwLinearSize == 0) {
        dwLinearSize = dwHeight * dwWidth;
    }

    if(dwLinearSize <= 0) {
        fclose(fp);

        return NULL;
    }

    info.m_numMipMaps = dwMipMaps;
    info.m_width = dwWidth;
    info.m_height = dwHeight;

    int mipFactor = 0;

    switch(dwFourCC) {
        case DS_FOURCC_DXT1:
            mipFactor = 2;
            info.m_components = 3;
            info.m_type = DDS_DXT1;

            break;

        case DS_FOURCC_DXT3:
            mipFactor = 4;
            info.m_components = 4;
            info.m_type = DDS_DXT3;

            break;

        case DS_FOURCC_DXT5:
            mipFactor = 4;
            info.m_components = 4;
            info.m_type = DDS_DXT5;

            break;

        default:
            fclose(fp);

            return NULL;
            break;
    }

    int totalSize = 0;

    if(dwMipMaps > 1) {
        totalSize = dwLinearSize * mipFactor;
    }
    else {
        totalSize = dwLinearSize;
    }

    unsigned char* image = NULL;

    image = new unsigned char[totalSize * sizeof(unsigned char)];

    if(image != NULL) {
        fseek(fp, ddsImageDataOffset, SEEK_SET);
        fread(image, 1, totalSize, fp);
    }

    fclose(fp);

    return image;
}

GLuint g_texture;

void Resize(int width, int height) {
    glViewport(0, 0, width, height);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluPerspective(45.0, (double)width / (double)height, 0.1, 200.0);
    glMatrixMode(GL_MODELVIEW);
}

void KeyDown(unsigned char key, int x, int y) {
    switch(key) {
        case 27:
            exit(0);

            break;
    }
}

void ShutdownApp() {
    glDeleteTextures(1, &g_texture);
}

bool InitializeApp() {
    glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
    glShadeModel(GL_SMOOTH);
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_TEXTURE_2D);

    ImageInfoDDS info;

    unsigned char* image = LoadDDS("image.dds", info);

    if(image == NULL) {
        return false;
    }

    glGenTextures(1, &g_texture);
    glBindTexture(GL_TEXTURE_2D, g_texture);

    int w = info.m_width;
    int h = info.m_height;
    int mipFactor = 0;

    if(info.m_type == DDS_DXT1) {
        mipFactor = 8;
    }
    else {
        mipFactor = 16;
    }

    int mipSize;
    int mipOffset = 0;
    int type = 0;

    switch(info.m_type) {
        case DDS_DXT1:
            type = GL_COMPRESSED_RGBA_S3TC_DXT1_EXT;

            break;

        case DDS_DXT3:
            type = GL_COMPRESSED_RGBA_S3TC_DXT3_EXT;

            break;

        case DDS_DXT5:
            type = GL_COMPRESSED_RGBA_S3TC_DXT5_EXT;

            break;
    }

    for(int i = 0; i < info.m_numMipMaps; ++i) {
        mipSize = ((w + 3) / 4) * ((h + 3) / 4) * mipFactor;

        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        glCompressedTexImage2DARB(GL_TEXTURE_2D, i, type, w, h, 0, mipSize, image + mipOffset);

        w >>= 1;
        h >>= 1;

        mipOffset += mipSize;
    }

    delete[] image;

    return true;
}

void RenderScene() {
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glLoadIdentity();
    glTranslatef(0.0f, 0.0f, -3.0f);
    glBindTexture(GL_TEXTURE_2D, g_texture);
    glBegin(GL_QUADS);

    glTexCoord2f(0, 0);
    glVertex3f(-1, -1, 0);

    glTexCoord2f(1, 0);
    glVertex3f(1, -1, 0);

    glTexCoord2f(1, 1);
    glVertex3f(1, 1, 0);

    glTexCoord2f(0, 1);
    glVertex3f(-1, 1, 0);

    glEnd();
    glutSwapBuffers();
    glutPostRedisplay();
}

int main(int argc, char** argv) {
    glutInitWindowSize(640, 480);
    glutInitWindowPosition(100, 100);
    glutInitDisplayMode(GLUT_RGB | GLUT_DOUBLE | GLUT_DEPTH);
    glutInit(&argc, argv);
    glutCreateWindow("Texture Compression");

    GLenum glewErr = glewInit();

    if (glewErr != GLEW_OK) {
        printf("GLEW init failed: %s\n", glewGetErrorString(glewErr));

        return 1;
    }

    printf("GLEW initialized. OpenGL version: %s\n", glGetString(GL_VERSION));

    glutDisplayFunc(RenderScene);
    glutReshapeFunc(Resize);
    glutKeyboardFunc(KeyDown);

    if(InitializeApp() == true) {
        glutMainLoop();
    }
    else {
        printf("Error in InitializeApp()!\n\n");
    }

    ShutdownApp();

    return 1;
}