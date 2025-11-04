#include <iostream>
#include <vector>

void printVector(std::vector<int>& array) {
    std::cout
        << "Contents ("
        << "Size: "
        << (int)array.size()
        << " Max: "
        << (int)array.capacity()
        << ") - ";

    for(int i = 0; i < (int)array.size(); ++i) {
        std::cout << array[i] << " ";
    }

    std::cout << std::endl;
}

int main(int argc, char** argv) {
    std::cout << "STL vector example" << std::endl;
    std::cout << "Data Structures for Game Developers" << std::endl;
    std::cout << "Aria Silva" << std::endl << std::endl;

    std::vector<int> array;

    array.reserve(5);
    array.push_back(10);
    array.push_back(20);
    array.push_back(30);
    array.push_back(40);

    std::cout << "Inserted into vector.";

    printVector(array);

    array.pop_back();
    array.pop_back();

    std::cout << "Popped two from vector.";

    printVector(array);

    array.clear();

    std::cout << "Cleared vector.";

    printVector(array);

    std::cout << std::endl;

    if(array.empty() == true) {
        std::cout << "Vector is empty.";
    }
    else {
        std::cout << "Vector is NOT empty.";
    }

    std::cout << std::endl << std::endl;

    return 1;
}