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

        void SelectionSort() {
            assert(m_array != NULL);

            T temp;

            int min = 0;

            for(int k = 0; k < m_numElements - 1; ++k) {
                min = k;

                for(int i = k + 1; i < m_numElements; ++i) {
                    if(m_array[i] < m_array[min]) {
                        min = i;
                    }
                }

                if(m_array[k] > m_array[min]) {
                    temp = m_array[k];
                    m_array[k] = m_array[min];
                    m_array[min] = temp;
                }
            }
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
    std::cout << "Selection sort algorithm" << std::endl;
    std::cout << "Chapter 5: Sorting" << std::endl << std::endl;

    UnorderedArray<int> array(5);

    array.push(136);
    array.push(489);
    array.push(28);
    array.push(1);
    array.push(393);

    std::cout << "Before selection sort:";

    for(int i = 0; i < 5; ++i) {
        std::cout << " " << array[i];
    }

    std::cout << std::endl;

    array.SelectionSort();

    std::cout << "After selection sort:";

    for(int i = 0; i < 5; ++i) {
        std::cout << " " << array[i];
    }

    std::cout << std::endl << std::endl;

    return 1;
}