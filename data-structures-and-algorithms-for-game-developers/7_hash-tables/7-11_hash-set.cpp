#include <iostream>
#include <cstring>
#include <unordered_set>

struct cmp {
    bool operator() (const char* str1, const char* str2) const {
        return strcmp(str1, str2) == 0;
    }
};

void Find(const std::unordered_set<const char*, std::hash<const char*>, cmp>& c, const char* str) {
    std::unordered_set<const char*, std::hash<const char*>, cmp>::const_iterator it = c.find(str);

    if(it == c.end()) {
        std::cout << str << " - was not found!" << std::endl;
    }
    else {
        std::cout << str << " - was found in the hash table" << std::endl;
    }
}

int main(int argrc, char** argv) {
    std::cout << "Hash tables - Hash set example" << std::endl;
    std::cout << "Chapter 7: Hash Tables" << std::endl;
    std::cout << std::endl;

    std::unordered_set<const char*, std::hash<const char*>, cmp> hashTable;

    hashTable.insert("Hello");
    hashTable.insert("Goodbye");
    hashTable.insert("So Long");
    hashTable.insert("Take Care");
    Find(hashTable, "Test");
    Find(hashTable, "Example");
    Find(hashTable, "Hello");
    Find(hashTable, "Take Care");
    Find(hashTable, "Good Night");

    std::cout << std::endl;

    return 1;
}