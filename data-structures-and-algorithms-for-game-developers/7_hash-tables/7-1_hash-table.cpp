#include <iostream>
#include <string>

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
        HashTable(int size) : m_size(0), m_totalItems(0) {
            if(size > 0) {
                m_size = GetNextPrimeNum(size);
                m_table = new HashItem<T>[m_size];
            }
        }

        ~HashTable() {
            if(m_table != NULL) {
                delete[] m_table;

                m_table = NULL;
            }
        }

        bool Insert(int key, T& obj) {
            if(m_totalItems == m_size) {
                return false;
            }

            int hash = HashFunction(key);

            while(m_table[hash].GetKey() != -1) {
                hash++;

                hash %= m_size;
            }

            m_table[hash].SetKey(key);
            m_table[hash].SetObj(obj);

            m_totalItems++;

            return true;
        }

        void Delete(int key) {
            int hash = HashFunction(key);
            int originalHash = hash;

            while(m_table[hash].GetKey() != -1) {
                if(m_table[hash].GetKey() == key) {
                    m_table[hash].SetKey(-1);

                    m_totalItems--;

                    return;
                }

                hash++;

                hash %= m_size;

                if(originalHash == hash) {
                    return;
                }
            }
        }

        bool Find(int key, T* obj) {
            int hash = HashFunction(key);
            int originalHash = hash;

            while(m_table[hash].GetKey() != -1) {
                if(m_table[hash].GetKey() == key) {
                    if(obj != NULL) {
                        *obj = m_table[hash].GetObject();
                    }

                    return true;
                }

                hash++;

                hash %= m_size;

                if(originalHash == hash) {
                    return false;
                }
            }

            return false;
        }

        int HashFunction(int key) {
            return key % m_size;
        }

        int HashFunction(std::string& str) {
            int hash = 0;

            for(int i = 0; i < (int)str.size(); ++i) {
                int val = (int)str[i];

                hash = (hash * 256 + val) % m_size;
            }

            return hash;
        }

        int GetSize() {
            return m_size;
        }

        int GetTotalItems() {
            return m_totalItems;
        }

    private:
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

        HashItem<T>* m_table;
        int m_size;
        int m_totalItems;
};

int main(int argc, char** argv) {
    std::cout << "Hash Tables - Linear Probing Example" << std::endl;
    std::cout << "Chapter 7: Hash Tables" << std::endl;
    std::cout << std::endl;

    HashTable<int> hashTable(20);

    int item = 0;

    item = 348;

    hashTable.Insert(112, item);

    item = 841;

    hashTable.Insert(87, item);

    item = 654;

    hashTable.Insert(24, item);

    item = 11;

    hashTable.Insert(66, item);

    item = 156;

    hashTable.Insert(222, item);

    if(hashTable.Find(87, &item)) {
        std::cout << "Item: 87 has a value of " << item << "." << std::endl;
    }
    else {
        std::cout << "Item: 87 not found" << std::endl;
    }

    if(hashTable.Find(112, &item)) {
        std::cout << "Item: 112 has a value of " << item << "." << std::endl;
    }
    else {
        std::cout << "Item: 112 not found" << std::endl;
    }

    if(hashTable.Find(66, &item)) {
        std::cout << "Item: 66 has a value of " << item << "." << std::endl;
    }
    else {
        std::cout << "Item: 66 not found" << std::endl;
    }

    if(hashTable.Find(100, &item)) {
        std::cout << "Item: 100 has a value of " << item << "." << std::endl;
    }
    else {
        std::cout << "Item: 100 not found" << std::endl;
    }

    std::cout << "\n";

    std::string str("cats");

    int stringHash = hashTable.HashFunction(str);

    std::cout << "The string cats hash to " << stringHash << "." << std::endl << std::endl;

    return 0;
}