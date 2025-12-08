#include <iostream>
#include <map>
#include <string>
#include <algorithm>

int main(int argc, char** argv) {
    std::cout << "STL Multi-Maps example" << std::endl;
    std::cout << "Chapter 12: STL algorithms" << std::endl;
    std::cout << std::endl;

    std::multimap<int, std::string> mapPair;

    mapPair.insert(std::map<int, std::string>::value_type(300, "Test 1"));
    mapPair.insert(std::map<int, std::string>::value_type(150, "Test 2"));
    mapPair.insert(std::map<int, std::string>::value_type(100, "Test 3"));
    mapPair.insert(std::map<int, std::string>::value_type(275, "Test 4"));
    mapPair.insert(std::map<int, std::string>::value_type(150, "Test 5"));

    std::cout << "Displaying all items:" << std::endl;

    std::multimap<int, std::string>::iterator it;

    for(it = mapPair.begin(); it != mapPair.end(); ++it) {
        std::cout << "Key: " << (*it).first << " Value: " << (*it).second << "." << std::endl;
    }

    std::cout << std::endl;

    std::pair<std::multimap<int, std::string>::iterator, std::multimap<int, std::string>::iterator> range;

    range = mapPair.equal_range(150);

    std::cout << "Displaying all items in a range:" << std::endl;

    for(it = range.first; it != range.second; ++it) {
        std::cout << "Key: " << (*it).first << " Value: " << (*it).second << "." << std::endl;
    }

    std::cout << std::endl;

    std::multimap<int, std::string>::iterator itPos = mapPair.find(150);

    if(itPos != mapPair.end()) {
        std::cout << "Found Key 100" << std::endl;
    }

    std::cout << std::endl;

    return 1;
}