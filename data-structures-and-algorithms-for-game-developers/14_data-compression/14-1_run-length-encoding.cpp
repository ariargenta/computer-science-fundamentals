#include <iostream>
#include <string>

void Encode(std::string& src, std::string& dst) {
    int index = 0;
    char runTotal = 0;
    char currentChar = 0;

    while(index < (int)src.length()) {
        runTotal = 0;
        currentChar = src[index];

        while(currentChar == src[index] && runTotal < 255) {
            runTotal++;
            index++;
        }

        if(runTotal > 3) {
            dst += '~';
            dst += runTotal;
            dst += currentChar;
        }
        else {
            for(int i = 0; i < runTotal; ++i) {
                dst += currentChar;
            }
        }
    }
}

void Decode(std::string& src, std::string& dst) {
    int index = 0;
    int runTotal = 0;
    char currentChar = 0;

    while(index < (int)src.length()) {
        if(src[index++] == '~') {
            runTotal = (int)src[index++];
            currentChar = src[index++];

            for(int i = 0; i < runTotal; ++i) {
                dst += currentChar;
            }
        }
        else {
            dst += src[index - 1];
        }
    }
}

int main(int argc, char* argv[]) {
    std::cout << "Run Length Encoding" << std::endl;
    std::cout << "Chapter 14: Data Compression and Encryption" << std::endl;
    std::cout << std::endl;

    std::string str = "AAAAaaBBBBBBCCCCCCddddddEEEEEeeeeFFFFGGGG";
    std::string encodedStr;
    std::string decodedStr;

    std::cout << "Original Data Size: " << str.length() << std::endl;
    std::cout << "Original Data:" << std::endl;
    std::cout << str << std::endl << std::endl;

    Encode(str, encodedStr);

    std::cout << "Compressed Size: " << encodedStr.length() << std::endl;
    std::cout << "Compressed Data:" << std::endl;
    std::cout << encodedStr << std::endl << std::endl;

    Decode(encodedStr, decodedStr);

    std::cout << "Decompressed Size: " << decodedStr.length() << std::endl;
    std::cout << "Decompressed Data:" << std::endl;
    std::cout << decodedStr << std::endl << std::endl;

    return 1;
}