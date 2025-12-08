#include <iostream>
#include <set>

int main(int argc, char* argv[]) {
    std::cout << "STL Set example" << std::endl;
    std::cout << "Chapter 12: STL Algorithms" << std::endl;
    std::cout << std::endl;

    std::set<int> setContainer;

    setContainer.insert(300);
    setContainer.insert(150);
    setContainer.insert(400);
    setContainer.insert(375);

    std::set<int> setCopy(setContainer);

    std::cout << "Displaying all items" << std::endl;

    for(std::set<int>::iterator it = setCopy.begin(); it != setCopy.end(); ++it) {
        std::cout << "Key/Value: " << (*it) << "." << std::endl;
    }

    std::cout << std::endl;

    std::set<int>::iterator itPos = setContainer.find(150);

    if(itPos != setContainer.end()) {
        std::cout << "Found Key 150" << std::endl << std::endl;
    }

    return 1;
}