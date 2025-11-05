#include <iostream>
#include <bitset>

int main(int argc, char** argv) {
    std::bitset<16> bitArray;

    std::cout << "STL Bitset Example" << std::endl;
    std::cout << "Data structures and Algorithms for Game Developers" << std::endl;
    std::cout << "Aria Argenta" << std::endl << std::endl;
    std::cout << "Number of bits: " << bitArray.size() << std::endl;
    std::cout << "Bits not set (T or F): " << bitArray.none() << std::endl;
    std::cout << std::endl;
    std::cout << "Initial bit values for 2, 3 and 14" << std::endl << std::endl;
    std::cout << "Bit 2 = " << bitArray[2] << "." << std::endl;
    std::cout << "Bit 3 = " << bitArray[3] << "." << std::endl;
    std::cout << "Bit 14 = " << bitArray.test(14) << "." << std::endl<< std::endl;
    std::cout << "Set bits 2 and 14" << std::endl << std::endl;

    bitArray[2] = true;
    bitArray[14] = true;

    std::cout << "Bit 2 = " << bitArray[2] << "." << std::endl;
    std::cout << "Bit 3 = " << bitArray[3] << "." << std::endl;
    std::cout << "Bit 14 = " << bitArray.test(14) << "." << std::endl << std::endl;
    std::cout << "Set all bits" << std::endl << std::endl;

    bitArray.set();

    std::cout << "Bit 2 = " << bitArray[2] << "." << std::endl;
    std::cout << "Bit 3 = " << bitArray[3] << "." << std::endl;
    std::cout << "Bit 14 = " << bitArray.test(14) << "." << std::endl << std::endl;
    std::cout << "Clear all bits" << std::endl << std::endl;

    bitArray.reset();

    std::cout << "Bit 2 = " << bitArray[2] << "." << std::endl;
    std::cout << "Bit 3 = " << bitArray[3] << "." << std::endl;
    std::cout << "Bit 14 = " << bitArray.test(14) << "." << std::endl << std::endl;

    return 1;
}