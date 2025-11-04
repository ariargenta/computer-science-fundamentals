#include <iostream>
#include <vector>
#include <algorithm>

class ExampleClass {
    public:
        ExampleClass() {
            std::cout << "Item created!" << std::endl;
        }

        ~ExampleClass() {
            std::cout << "Item deleted!" << std::endl;
        }
};

struct DeleteMemObj {
    template<typename T>
    void operator()(const T* ptr) const {
        delete ptr;

        ptr = NULL;
    }
};

int main(int argc, char* argv[]) {
    std::cout << "Deleting new pointers" << std::endl;
    std::cout << "Data structures and algorithms for game developers" << std::endl;
    std::cout << "Aria Argenta" << std::endl;

    std::vector<ExampleClass*> array;

    array.reserve(5);
    array.push_back(new ExampleClass);
    array.push_back(new ExampleClass);
    array.push_back(new ExampleClass);
    array.push_back(new ExampleClass);
    array.push_back(new ExampleClass);

    std::cout << std::endl;

    for_each(array.begin(), array.end(), DeleteMemObj());

    std::cout << std::endl;
    std::cout << "Array items deleted!";
    std::cout << std::endl << std::endl;

    return 1;
}