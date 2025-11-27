#include <iostream>
#include <vector>

template<typename KEY, typename CMP>
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

            CMP cmp;

            while(index > 0 && (cmp(temp, m_heap[parentIndex]) || temp == m_heap[parentIndex])) {
                m_heap[index] = m_heap[parentIndex];
                index = parentIndex;
                parentIndex = (parentIndex - 1) / 2;
            }

            m_heap[index] = temp;
        }

        void pop() {
            int index = 0;

            m_heap[index] = m_heap[(int)m_heap.size() - 1];
            m_heap.pop_back();

            KEY temp = m_heap[index];

            int currentIndex = 0;
            int leftIndex = 0;
            int rightIndex = 0;

            while(index < (int)m_heap.size() / 2) {
                leftIndex = 2 * index + 1;
                rightIndex = leftIndex + 1;

                CMP cmp;

                if(rightIndex < (int)m_heap.size() && (cmp(m_heap[rightIndex], m_heap[leftIndex]) || m_heap[rightIndex] == m_heap[leftIndex])) {
                    currentIndex = rightIndex;
                }
                else {
                    currentIndex = leftIndex;
                }

                if(cmp(temp, m_heap[currentIndex]) || temp == m_heap[currentIndex]) {
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

    private:
        std::vector<KEY> m_heap;
};

template<typename T, typename CMP>
class PriorityQueue {
    public:
        PriorityQueue() {}

        void push(T val) {
            m_elements.push(val);
        }

        void pop() {
            m_elements.pop();
        }

        T peek() {
            return m_elements.peek();
        }

        int size() {
            return m_elements.size();
        }

        bool empty() {
            return (m_elements.size() == 0);
        }

    private:
        Heap<T, CMP> m_elements;
};

template<typename T>
class less_cmp {
    public:
        inline bool operator()(T lVal, T rVal) {
            return (lVal < rVal);
        }
};

template<typename T>
class greater_cmp {
    public:
        inline bool operator()(T lVal, T rVal) {
            return !(lVal < rVal);
        }
};

int main(int argc, char* argv[]) {
    std::cout << "Heap used in a Priority Queue" << std::endl;
    std::cout << "Chapter 10: Heaps" << std::endl;
    std::cout << std::endl;

    PriorityQueue<int, less_cmp<int>> pq;

    pq.push(33);
    pq.push(43);
    pq.push(23);
    pq.push(20);
    pq.push(10);
    pq.push(22);
    pq.push(90);
    pq.push(95);
    pq.push(86);

    std::cout << "Contents of the priority queue:";

    while(pq.empty() != true) {
        std::cout << " " << pq.peek();

        pq.pop();
    }

    std::cout << "." << std::endl << std::endl;

    return 1;
}