#include <iostream>
#include <cassert>

int TriNumLoop(int term) {
    int value = 0;

    for(; term > 0; --term) {
        value += term;
    }

    return value;
}

int TriNumRecursion(int term) {
    assert(term >= 1);

    if(term == 1) {
        return 1;
    }

    return(TriNumRecursion(term - 1) + term);
}

int main(int argc, char** argv) {
    std::cout << "Triangular numbers example" << std::endl;
    std::cout << "Chapter 3: Recursion" << std::endl << std::endl;
    std::cout << "The value of the 18th term using a loop: ";
    std::cout << TriNumLoop(18);
    std::cout << "." << std::endl;
    std::cout << "The value of the 25th term using recursion: ";
    std::cout << TriNumRecursion(25);
    std::cout << "." << std::endl;
    std::cout << std::endl;

    return 1;
}