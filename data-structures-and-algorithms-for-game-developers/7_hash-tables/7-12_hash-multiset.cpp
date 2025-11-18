#include <iostream>
#include <cstring>
#include <unordered_set>

struct cmp {
    bool operator()(const char* str1, const char* str2) const {
        return strcmp(str1, str2) == 0;
    }
};

void Find(const std::unordered_multiset<const char*, std::hash<const char*>, cmp>& c, const char* str) {
    std::unordered_multiset<const char*, std::hash<const char*>, cmp>::const_iterator it = c.find(str);

    if(it == c.end()) {
        std::cout << str << " - was not found!" << std::endl;
    }
    else {
        std::cout << str << " - was found in the hash table" << std::endl;
    }
}

int main(int argc, char* argv[]) {
    std::cout << "Hash Tables - Hash multi-set example" << std::endl;
    std::cout << "Chapter 7: Hash Tables" << std::endl;
    std::cout << std::endl;

    std::unordered_multiset<const char*, std::hash<const char*>, cmp> hashTable;

    if(hashTable.empty() == true) {
        std::cout << "The hash table is now empty" << std::endl;
    }

    hashTable.insert("Data Structures");
    hashTable.insert("and");
    hashTable.insert("Algorithms");
    hashTable.insert("For Game Developers");

    std::cout << "The hash table has " << hashTable.size() << " items after insettions" << std::endl << std::endl;

    Find(hashTable, "Wow");
    Find(hashTable, "and");
    Find(hashTable, "Data Structures");

    std::cout << std::endl;

    return 1;
}