#include <iostream>
#include <valarray>

void printValArray(const std::valarray<int>& valArray) {
    std::cout << "Contents of valArray: ";

    for(int i = 0; i < 10; ++i) {
        std::cout << valArray[i] << " ";
    }

    std::cout << std::endl << std::endl;
}

int main(int argc, char** argv) {
    std::cout << "STL Val Array example" << std::endl;
    std::cout << "Data structures and Algorithms for Game Developers" << std::endl;
    std::cout << "Aria Argenta" << std::endl;

    std::valarray<int> valArray(10);

    for(int i = 0; i < 10; ++i) {
        valArray[i] = i;
    }

    printValArray(valArray);

    std::valarray<int>::value_type rVal = 5;

    std::cout
        << "The value of rVal before multiplication: "
        << rVal
        << std::endl
        << std::endl;

    valArray *= rVal;

    printValArray(valArray);

    return 1;
}