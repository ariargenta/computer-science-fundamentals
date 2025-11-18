#include <iostream>
#include <cstring>
#include <unordered_map>

struct cmp {
    bool operator() (const char* str1, const char* str2) const {
        return strcmp(str1, str2) == 0;
    }
};

typedef std::unordered_multimap<const char*, int, std::hash<const char*>, cmp> hashType;

void Find(const hashType& hTable, const char* str) {
    std::cout << str << " - ";

    std::pair<hashType::const_iterator, hashType::const_iterator> range = hTable.equal_range(str);

    hashType::const_iterator it;

    for(it = range.first; it != range.second; ++it) {
        std::cout << (*it).second << " ";
    }

    std::cout << std::endl;
}

int main(int argc, char** argv) {
    std::cout << "Hash tables - Hash multi hTable example" << std::endl;
    std::cout << "Chapter 7: Hash Tables" << std::endl;
    std::cout << std::endl;

    hashType hashTable;

    if(hashTable.empty() == true) {
        std::cout << "The hash table is now empty" << std::endl;
    }

    hashTable.insert(hashType::value_type("DVD", 30));
    hashTable.insert(hashType::value_type("Apple", 1));
    hashTable.insert(hashType::value_type("Video Game", 59));

    std::cout << "The hash table has " << hashTable.size() << " items after insertions" << std::endl << std::endl;

    Find(hashTable, "DVD");
    Find(hashTable, "Apple");
    Find(hashTable, "Video Game");
    Find(hashTable, "PS5");

    std::cout << std::endl;

    return 1;
}