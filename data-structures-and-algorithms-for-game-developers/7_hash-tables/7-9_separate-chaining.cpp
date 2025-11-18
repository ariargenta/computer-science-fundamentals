#include <iostream>
#include <list>

template<typename T>
class HashItem {
    public:
        HashItem() : m_key(-1) {}
        ~HashItem() {}

        int GetKey() {return m_key;}
        void SetKey(int k) {m_key = k;}
        T GetObject() {return m_obj;}
        void SetObj(T obj) {m_obj = obj;}

        bool operator == (HashItem& item) {
            if(m_key == item.GetKey()) {
                return true;
            }

            return false;
        }

        void operator = (HashItem item) {
            m_key = item.GetKey();
            m_obj = item.GetObject();
        }

    private:
        int m_key;
        T m_obj;
};

template<typename T>
class HashTable {
    public:
        HashTable(int size) : m_size(0) {
            if(size > 0) {
                m_size = GetNextPrimeNum(size);
                m_table = new std::list<HashItem<T>> [m_size];
            }
        }

        ~HashTable() {
            if(m_table != NULL) {
                delete[] m_table;

                m_table = NULL;
            }
        }

        bool isNumPrime(int val) {
            for(int i = 2; (i * i) <= val; ++i) {
                if((val % i) == 0) {
                    return false;
                }
            }

            return true;
        }

        int GetNextPrimeNum(int val) {
            for(int i = val + 1; ; ++i) {
                if(isNumPrime(i)) {
                    return i;
                }
            }
        }

        void Insert(int key, T& obj) {
            HashItem<T> item;

            item.SetKey(key);
            item.SetObj(obj);

            int hash = HashFunction(key);

            m_table[hash].push_back(item);
        }

        void Delete(int key) {
            int hash = HashFunction(key);

            std::list<HashItem<T>>* ptr = &m_table[hash];

            typename std::list<HashItem<T>>::iterator it;

            for(it = ptr -> begin(); it != ptr -> end(); ++it) {
                if((*it).GetKey() == key) {
                    ptr -> erase(it);

                    break;
                }
            }
        }

        bool Find(int key, T* obj) {
            int hash = HashFunction(key);

            std::list<HashItem<T>>* ptr = &m_table[hash];

            typename std::list<HashItem<T>>::iterator it;

            for(it = ptr -> begin(); it != ptr -> end(); ++it) {
                if((*it).GetKey() == key) {
                    if(obj != NULL) {
                        *obj = (*it).GetObject();
                    }

                    return true;
                }
            }

            return false;
        }

        int HashFunction(int key) {
            return key % m_size;
        }

        int HashFunction(std::string& str) {
            int hash = 0;
            int i = 0;

            for(i = 0; i < (int)str.size(); ++i) {
                int val = (int)str[i];
                hash = (hash * 256 + val) % m_size;
            }

            return hash;
        }

        int GetSize() {
            return m_size;
        }

    private:
        std::list<HashItem<T>>* m_table;
        int m_size;
};

int main(int argc, char** argv) {
    std::cout << "Hash tables - Separate chaining example" << std::endl;
    std::cout << "Chapter 7: Hash Tables" << std::endl;
    std::cout << std::endl;

    HashTable<int> hashTable(20);

    int item = 0;

    item = 142;

    hashTable.Insert(31, item);

    item = 756;

    hashTable.Insert(42, item);

    item = 432;

    hashTable.Insert(24, item);

    item = 124;

    hashTable.Insert(51, item);

    item = 786;

    hashTable.Insert(12, item);

    if(hashTable.Find(31, &item)) {
        std::cout << "Item: 31 has a value of " << item << "." << std::endl;
    }
    else {
        std::cout << "Item: 31 not found" << std::endl;
    }

    if(hashTable.Find(84, &item)) {
        std::cout << "Item 84 has a value of " << item << "." << std::endl;
    }
    else {
        std::cout << "Item: 84 not found" << std::endl;
    }

    if(hashTable.Find(99, &item)) {
        std::cout << "Item: 99 has a value of " << item << "." << std::endl;
    }
    else {
        std::cout << "Item: 99 not found" << std::endl;
    }

    if(hashTable.Find(51, &item)) {
        std::cout << "Item: 51 has a value of " << item << "." << std::endl;
    }
    else {
        std::cout << "Item: 51 not found" << std::endl;
    }

    if(hashTable.Find(12, &item)) {
        std::cout << "Item: 12 has a value of " << item << "." << std::endl;
    }
    else {
        std::cout << "Item: 12 not found" << std::endl;
    }

    std::cout << std::endl;

    return 1;
}