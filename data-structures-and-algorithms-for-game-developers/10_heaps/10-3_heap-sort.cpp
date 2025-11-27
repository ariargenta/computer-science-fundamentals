#include <iostream>
#include <vector>

template<typename KEY>
class Heap {
    public:
        Heap() {}

        Heap(int minSize) {
            m_heap.reserve(minSize);
        }

        void push(KEY key) {
            m_heap.push_back(key);

            int index = (int)m_heap.size() - 1;
            KEY temp = m_heap[index];
            int parentIndex = (index - 1) / 2;

            while(index > 0 && temp >= m_heap[parentIndex]) {
                m_heap[index] = m_heap[parentIndex];
                index = parentIndex;
                parentIndex = (parentIndex - 1) / 2;
            }

            m_heap[index] = temp;
        }

        void pop() {
            int index = 0;

            m_heap[index] = m_heap[(int) m_heap.size() - 1];

            m_heap.pop_back();

            KEY temp = m_heap[index];

            int currentIndex = 0;
            int leftIndex = 0;
            int rightIndex = 0;

            while(index < (int)m_heap.size() / 2) {
                leftIndex = 2 * index + 1;
                rightIndex = leftIndex + 1;

                if(rightIndex < (int)m_heap.size() && m_heap[leftIndex] < m_heap[rightIndex]) {
                    currentIndex = rightIndex;
                }
                else {
                    currentIndex = leftIndex;
                }

                if(temp >= m_heap[currentIndex]) {
                    break;
                }

                m_heap[index] = m_heap[currentIndex];

                index = currentIndex;
            }

            m_heap[index] = temp;
        }

        KEY peek() {
            return m_heap[0];
        }

        int size() {
            return (int)m_heap.size();
        }

        void remove() {
            remove(m_heap);
        }

        void remove(std::vector<KEY> m_heap) {
            int lastElement = m_heap[m_heap.size() - 1];

            m_heap[0] = lastElement;
        }

    private:
        std::vector<KEY> m_heap;
};

void HeapSortAscending(std::vector<int>& array) {
    Heap<int> heap;

    int i;

    for(i = 0; i < (int)array.size(); ++i) {
        heap.push(array[i]);
    }

    for(i = (int)array.size() - 1; i >= 0; --i) {
        array[i] = heap.peek();

        heap.pop();
    }
}

void HeapSortDescending(std::vector<int>& array) {
    Heap<int> heap;

    int i;

    for(i = 0; i < (int)array.size(); ++i) {
        heap.push(array[i]);
    }

    for(i = 0; i <(int)array.size(); ++i) {
        array[i] = heap.peek();

        heap.pop();
    }
}

void DisplayVector(std::vector<int>& array) {
    for(int i = 0; i < (int)array.size(); ++i) {
        std::cout << " " << array[i];
    }

    std::cout << ".";
}

int main(int argc, char** argv) {
    std::cout << "Heap Sort" << std::endl;
    std::cout << "Chapter 10: Heaps" << std::endl;
    std::cout << std::endl;

    std::vector<int> array;

    array.push_back(33);
    array.push_back(43);
    array.push_back(23);
    array.push_back(20);
    array.push_back(10);
    array.push_back(22);
    array.push_back(90);
    array.push_back(95);
    array.push_back(86);

    std::cout << "Array contents before sort:";

    DisplayVector(array);

    std::cout << std::endl;

    HeapSortAscending(array);

    std::cout << "Array contents after sort (ascending):";

    DisplayVector(array);

    std::cout << std::endl;

    HeapSortDescending(array);

    std::cout << "Array contents after sort (descending):";

    DisplayVector(array);

    std::cout << std::endl << std::endl;

    return 1;
}