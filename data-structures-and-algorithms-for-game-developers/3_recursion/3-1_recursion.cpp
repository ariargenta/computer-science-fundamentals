#include <iostream>
#include <cassert>

void PrintNumReverse(int x) {
    if(x <= 0) {
        return;
    }

    std::cout << " " << x;

    PrintNumReverse(x - 1);
}

int main(int argc, char** argv) {
    std::cout << "Recursion example" << std::endl;
    std::cout << "Chapter 3: Recursion" << std::endl << std::endl;
    std::cout << "Example of a recursive call:";

    PrintNumReverse(10);

    std::cout << "." << std::endl << std::endl;

    return 1;
}