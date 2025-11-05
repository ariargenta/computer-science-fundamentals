#include <iostream>
#include <cassert>
#include <cstring>

template<class T>
class OrderedArray {
    public:
        OrderedArray(int size, int growBy = 1):
            m_array(NULL)
            , m_maxSize(0)
            , m_growSize(0)
            , m_numElements(0)
        {
            if(size) {
                m_maxSize = size;
                m_array = new T[m_maxSize];

                memset(m_array, 0, sizeof(T) * m_maxSize);

                m_growSize = ((growBy > 0) ? growBy : 0);
            }
        }

        virtual ~OrderedArray() {
            if(m_array != NULL) {
                delete[] m_array;

                m_array = NULL;
            }
        }

        void push(T val) {
            assert(m_array != NULL);

            int i = 0;

            if(m_numElements >= m_maxSize) {
                Expand();
            }

            for(i; i < m_numElements; ++i) {
                if(m_array[i] > val) {
                    break;
                }
            }

            for(int k = m_numElements; k > i; --k) {
                m_array[k] = m_array[k - 1];
            }

            m_array[i] = val;
            m_numElements++;
        }

        void pop() {
            if(m_numElements > 0) {
                m_numElements--;
            }
        }

        void remove(int index) {
            assert(m_array != NULL);

            if(index >= m_maxSize) {
                return;
            }

            for(int k = index; k < m_maxSize - 1; ++k) {
                m_array[k] = m_array[k + 1];
            }

            m_maxSize--;

            if(m_numElements >= m_maxSize) {
                m_numElements = m_maxSize - 1;
            }
        }

        //Read only
        virtual const T& operator[](int index) {
            assert(m_array != NULL && index <= m_numElements);

            return m_array[index];
        }

        int search(T searchKey) {
            return binarySearch(searchKey, 0, m_numElements - 1);
        }

        void clear() {m_numElements = 0;}
        int GetSize() {return m_numElements;}
        int GetMaxSize() {return m_maxSize;}
        int GetGrowSize() {return m_growSize;}

        void SetGrowSize(int val) {
            assert(val >= 0);

            m_growSize = val;
        }

    private:
        bool Expand() {
            if(m_growSize <= 0) {
                return false;
            }

            T* temp = new T[m_maxSize + m_growSize];

            assert(temp != NULL);

            memcpy(temp, m_array, sizeof(T) * m_maxSize);

            delete[] m_array;

            m_array = temp;
            m_maxSize += m_growSize;

            return true;
        }

        int binarySearch(T searchKey, int lowerBound, int upperBound) {
            assert(m_array != NULL);
            assert(lowerBound >= 0);
            assert(upperBound < m_numElements);

            int current = (lowerBound + upperBound) >> 1;

            if(m_array[current] == searchKey) {
                return current;
            }
            else if(lowerBound > upperBound) {
                return -1;
            }
            else {
                if(m_array[current] < searchKey) {
                    return binarySearch(searchKey, current + 1, upperBound);
                }
                else {
                    return binarySearch(searchKey, lowerBound, current - 1);
                }
            }

            return -1;
        }

        T* m_array;
        int m_maxSize;
        int m_growSize;
        int m_numElements;
};

int main(int argc, char** argv) {
    std::cout << "Recursive binary search example" << std::endl;
    std::cout << "Chapter 3: Recursion" << std::endl << std::endl;

    OrderedArray<int> array(3);

    array.push(43);
    array.push(8);
    array.push(23);
    array.push(94);
    array.push(17);
    array.push(83);
    array.push(44);
    array.push(28);

    std::cout << "Ordered array contents:";

    for(int i = 0; i < array.GetSize(); ++i) {
        std::cout << " " << array[i];
    }

    std::cout << "." << std::endl;
    std::cout << "Search for 43 was found at index: ";
    std::cout << array.search(43) << ".";
    std::cout << std::endl << std::endl;

    return 1;
}