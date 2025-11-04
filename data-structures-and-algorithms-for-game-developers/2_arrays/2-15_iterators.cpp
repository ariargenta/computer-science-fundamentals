#include <iostream>
#include <vector>
#include <algorithm>
#include <numeric>
#include <iterator>

void printVector(std::vector<int>& array) {
    std::cout
        << "Contents ("
        << "Size: "
        << (int)array.size()
        << " Max: "
        << (int)array.capacity()
        << ") - ";

    std::ostream_iterator<int> output(std::cout, " ");

    std::cout << std::endl;
}

int main(int argc, char* argv[]) {
    std::cout << "STL vector example 2: Iterators" << std::endl;
    std::cout << "Data Structures for Game Developers" << std::endl;
    std::cout << "Aria Argenta" << std::endl << std::endl;

    std::vector<int> array;

    array.reserve(5);
    array.push_back(10);
    array.push_back(20);
    array.push_back(30);
    array.push_back(40);
    array.push_back(50);

    std::vector<int> array2;

    for(int i = 0; i < 5; ++i) {
        array2.push_back(0);
    }

    std::copy(array.begin(), array.end(), array2.begin());

    std::cout << "Inserted into vector: ";

    printVector(array);

    std::cout
        << "Accumulate: "
        << std::accumulate(array.begin(), array.end(), 0)
        << std::endl;

    array.pop_back();
    array.pop_back();

    std::cout << "Popped two from vector: ";

    printVector(array);

    array.clear();

    std::cout << "Cleared vector: ";

    printVector(array);

    std::cout << std::endl;
}