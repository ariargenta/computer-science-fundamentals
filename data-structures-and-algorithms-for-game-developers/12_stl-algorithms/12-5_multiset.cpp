#include <iostream>
#include <set>

int main(int argc, char** argv) {
    std::cout << "STL Multi-Set example" << std::endl;
    std::cout << "Chapter 12: STL algorithms" << std::endl;
    std::cout << std::endl;

    std::multiset<int> setContainer;
    std::multiset<int>::iterator it;

    setContainer.insert(423);
    setContainer.insert(634);
    setContainer.insert(124);
    setContainer.insert(756);

    std::cout << "Displaying all items:" << std::endl;

    for(it = setContainer.begin(); it != setContainer.end(); ++it) {
        std::cout << "Key/Value: " << (*it) << "." << std::endl;
    }

    std::cout << std::endl;

    std::multiset<int>::iterator itPos = setContainer.find(124);

    if(itPos != setContainer.end()) {
        setContainer.erase(124);

        std::cout << "Found and erased Key 124" << std::endl << std::endl;
    }

    std::cout << "Displaying all items:" << std::endl;

    for(it = setContainer.begin(); it != setContainer.end(); ++it) {
        std::cout << "Key/Value: " << (*it) << "." << std::endl;
    }

    std::cout << std::endl;

    return 1;
}