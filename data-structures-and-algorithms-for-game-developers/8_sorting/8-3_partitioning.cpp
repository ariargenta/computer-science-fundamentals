#include <iostream>
#include <cstring>
#include <cassert>

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

        void SwapElements(int index1, int index2) {
            assert(index1 >= 0 && index1 < m_numElements);
            assert(index2 >= 0 && index2 < m_numElements);
            assert(m_array != NULL);

            T temp = m_array[index1];
            m_array[index1] = m_array[index2];
            m_array[index2] = temp;
        }

        int Partition(T pivot) {
            return Partition(0, m_numElements - 1, pivot);
        }

        int Partition(int lIndex, int rIndex, T pivot) {
            int currentLeft = lIndex;
            int currentRight = rIndex;

            while(1) {
                while(currentLeft < rIndex && m_array[currentLeft] < pivot) {
                    currentLeft++;
                }

                while(currentRight > lIndex && m_array[currentRight] > pivot) {
                    currentRight--;
                }

                if(currentLeft >= currentRight) {
                    break;
                }

                SwapElements(currentLeft, currentRight);
            }

            return currentLeft;
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

        T* m_array;
        int m_maxSize;
        int m_growSize;
        int m_numElements;
};

int main(int argc, char* argv[]) {
    std::cout << "Partitioning Algorithm" << std::endl;
    std::cout << "Chapter 8: Advanced Sorting" << std::endl << std::endl;

    const int size = 10;
    int i = 0;
    int pivotValue = 60;

    UnorderedArray<int> array(size);

    for(i = 0; i < size; ++i) {
        array.push(rand() % 100);
    }

    std::cout
        << "Array size - "
        << size
        << "; pivot value - "
        << pivotValue
        << "."
        << std::endl
        << std::endl;

    std::cout << "Before partitioning:";

    for(i = 0; i < size; ++i) {
        std::cout << " " << array[i];
    }

    std::cout << std::endl << std::endl;

    int pivot = array.Partition(0, size - 1, pivotValue);

    std::cout << "After partitioning (pivot index - " << pivot << "):";

    for(i = 0; i < size; ++i) {
        std::cout << " " << array[i];
    }

    std::cout << std::endl << std::endl;

    return 1;
}