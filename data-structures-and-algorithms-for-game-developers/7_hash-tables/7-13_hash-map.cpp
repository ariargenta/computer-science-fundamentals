#include <iostream>
#include <cstring>
#include <unordered_map>

struct cmp {
    bool operator() (const char* str1, const char* str2) const {
        return strcmp(str1, str2) == 0;
    }
};

int main(int argc, char* argv[]) {
    std::cout << "Hash tables - Hash map example" << std::endl;
    std::cout << "Chapter 7: Hash Tables" << std::endl;
    std::cout << std::endl;

    std::unordered_map<const char*, int, std::hash<const char*>, cmp> hashTable;

    if(hashTable.empty() == true) {
        std::cout << "The hash table is now empty" << std::endl;
    }

    hashTable["DVD"] = 30;
    hashTable["Apple"] = 1;
    hashTable["Video Game"] = 59;

    std::cout << "The hash table has " << hashTable.size() << " items after insertions" << std::endl << std::endl;

    std::cout << "DVD - " << hashTable["DVD"] << std::endl;
    std::cout << "Apple - " << hashTable["Apple"] << std::endl;
    std::cout << "Video Game - " << hashTable["Video Game"] << std::endl;
    std::cout << "PS5 - " << hashTable["PS3"] << std::endl;

    std::cout << std::endl;

    return 1;
}