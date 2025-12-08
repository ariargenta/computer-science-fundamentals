#include <iostream>
#include <map>
#include <string>

int main(int argc, char* argv[]) {
    std::cout << "Maps example" << std::endl;
    std::cout << "Chapter 12: STL Algorithms" << std::endl;
    std::cout << std::endl;

    std::map<int, std::string> mapPair;

    mapPair.insert(std::map<int, std::string>::value_type(300, "Test 1"));
    mapPair.insert(std::map<int, std::string>::value_type(150, "Test 2"));
    mapPair.insert(std::map<int, std::string>::value_type(400, "Test 3"));
    mapPair.insert(std::map<int, std::string>::value_type(600, "Test 4"));

    mapPair[100] = "One hundred";

    std::cout << "Displaying 400: " << mapPair[400].c_str() << std::endl << std::endl;
    std::cout << "Displaying all items:" << std::endl;

    for(std::map<int, std::string>::iterator it = mapPair.begin(); it != mapPair.end(); ++it) {
        std::cout << "Key: " << (*it).first << " value: " << (*it).second << "." << std::endl;
    }

    std::cout << std::endl;

    std::map<int, std::string>::iterator itPos = mapPair.find(150);

    if(itPos != mapPair.end()) {
        std::cout << "Found key 150" << std::endl;
    }

    std::cout << std::endl;

    return 1;
}