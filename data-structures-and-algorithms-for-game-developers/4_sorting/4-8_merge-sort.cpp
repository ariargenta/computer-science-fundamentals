#include <iostream>
#include <cassert>
#include <cstring>

template<class T>
class UnorderedArray {
    public:
        UnorderedArray(int size, int growBy = 1):
            m_array(NULL)
            , m_maxSize(0)
            , m_growSize(0)
            , m_numElements(0)
        {
            if(size) {
                m_maxSize = size;
                m_array = new T[m_maxSize];

                std::memset(m_array, 0, sizeof(T) * m_maxSize);

                m_growSize = ((growBy > 0) ? growBy : 0);
            }
        }

        virtual ~UnorderedArray() {
            if(m_array != NULL) {
                delete[] m_array;

                m_array = NULL;
            }
        }

        virtual void push(T val) {
            assert(m_array != NULL);

            if(m_numElements >= m_maxSize) {
                Expand();
            }

            m_array[m_numElements] = val;
            m_numElements++;
        }

        void pop() {
            if(m_numElements > 0) {
                m_numElements--;
            }
        }

        void remove(int index) {
            assert(m_array != NULL);

            if(index >= m_numElements) {
                return;
            }

            for(int k = index; k < m_numElements - 1; ++k) {
                m_array[k] = m_array[k + 1];
            }

            m_numElements--;
        }

        virtual T& operator[](int index) {
            assert(m_array != NULL && index <= m_numElements);

            return m_array[index];
        }

        virtual int search(T val) {
            assert(m_array != NULL);

            for(int i = 0; i < m_numElements; ++i) {
                if(m_array[i] == val) {
                    return i;
                }
            }

            return -1;
        }

        void clear() {m_numElements = 0;}
        int GetSize() {return m_numElements;}
        int GetMaxSize() {return m_maxSize;}
        int GetGrowSize() {return m_growSize;}

        void SetGrowSize(int val) {
            assert(val >= 0);

            m_growSize = val;
        }

        void MergeSort() {
            assert(m_array != NULL);

            T* tempArray = new T[m_numElements];

            assert(tempArray != NULL);

            MergeSort(tempArray, 0, m_numElements - 1);

            delete[] tempArray;
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

        void MergeSort(T* tempArray, int lowerBound, int upperBound) {
            if(lowerBound == upperBound) {
                return;
            }

            int mid = (lowerBound + upperBound) >> 1;

            MergeSort(tempArray, lowerBound, mid);
            MergeSort(tempArray, mid + 1, upperBound);
            Merge(tempArray, lowerBound, mid + 1, upperBound);
        }

        void Merge(T* tempArray, int low, int mid, int upper) {
            int tempLow = low;
            int tempMid = mid - 1;
            int index = 0;

            while(low <= tempMid && mid <= upper) {
                if(m_array[low] < m_array[mid]) {
                    tempArray[index++] = m_array[low++];
                }
                else {
                    tempArray[index++] = m_array[mid++];
                }
            }

            while(low <= tempMid) {
                tempArray[index++] = m_array[low++];
            }

            while(mid <= upper) {
                tempArray[index++] = m_array[mid++];
            }

            for(int i = 0; i < upper - tempLow + 1; ++i) {
                m_array[tempLow + i] = tempArray[i];
            }
        }

        T* m_array;
        int m_maxSize;
        int m_growSize;
        int m_numElements;
};

int main(int argc, char** argv) {
    std::cout << "Merge sort algorithm" << std::endl;
    std::cout << "Chapter 4: Sorting" << std::endl << std::endl;

    UnorderedArray<int> array(5);

    array.push(645);
    array.push(294);
    array.push(777);
    array.push(789);
    array.push(119);
    array.push(100);
    array.push(823);

    std::cout << "Before merge sort:";

    for(int i = 0; i < 5; ++i) {
        std::cout << " " << array[i];
    }

    std::cout << std::endl;

    array.MergeSort();

    std::cout << "After merge sort:";

    for(int i = 0; i < 5; ++i) {
        std::cout << " " << array[i];
    }

    std::cout << std::endl << std::endl;

    return 1;
}