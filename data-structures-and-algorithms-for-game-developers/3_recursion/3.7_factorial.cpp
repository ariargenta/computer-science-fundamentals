#include <iostream>
#include <cassert>

int factorial(int x) {
    assert(x >= 0);

    if(x == 0) {
        return 1;
    }

    return(factorial(x - 1) * x);
}

int doubleFactorial(int x) {
    assert(x >= 0);

    if(x == 0) {
        return 1;
    }

    return(doubleFactorial(x - 2) * x);
}

int main(int argc, char** argv) {
    std::cout << "Factorials" << std::endl;
    std::cout << "Chapter 3: Recursion" << std::endl << std::endl;
    std::cout << "The factorial of three: ";
    std::cout << factorial(3);
    std::cout << "." << std::endl;
    std::cout << "The double factorial of four: ";
    std::cout << doubleFactorial(4);
    std::cout << "." << std::endl;
    std::cout << std::endl;
}