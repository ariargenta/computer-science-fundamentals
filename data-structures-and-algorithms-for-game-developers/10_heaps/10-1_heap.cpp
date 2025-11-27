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

int main(int argc, char** argv) {
    std::cout << "Heap" << std::endl;
    std::cout << "Chapter 10: Heaps" << std::endl;
    std::cout << std::endl;

    Heap<int> heap(10);

    heap.push(30);
    heap.push(33);
    heap.push(43);
    heap.push(23);
    heap.push(20);
    heap.push(10);
    heap.push(22);
    heap.push(90);
    heap.push(95);
    heap.push(86);

    std::cout << "Heap Contents:";

    while(heap.size() != 0) {
        std::cout << " " << heap.peek();

        heap.pop();
    }

    std::cout << "." << std::endl << std::endl;

    return 1;
}