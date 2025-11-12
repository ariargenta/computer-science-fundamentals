#include <iostream>
#include <cassert>

template<typename T> class LinkIterator;
template<typename T> class LinkList;

template<typename T>
class LinkNode {
    friend class LinkIterator<T>;
    friend class LinkList<T>;

    private:
        LinkNode() : m_next(0), m_previous(0) {}

        T m_data;
        LinkNode* m_next;
        LinkNode* m_previous;
};

template<typename T>
class LinkIterator {
    public:
        LinkIterator() {
            m_node = NULL;
        }

        ~LinkIterator() {}

        void operator = (LinkNode<T>* node) {
            m_node = node;
        }

        T& operator * () {
            assert(m_node != NULL);

            return m_node -> m_data;
        }

        void operator ++ () {
            assert(m_node != NULL);

            m_node = m_node -> m_next;
        }

        void operator ++ (int) {
            assert(m_node != NULL);

            m_node = m_node -> m_next;
        }

        bool operator != (LinkNode<T>* node) {
            return(m_node != node);
        }

        bool operator == (LinkNode<T>* node) {
            return(m_node == node);
        }

        void operator -- () {
            assert(m_node != NULL);

            m_node = m_node -> m_previous;
        }

        void operator -- (int) {
            assert(m_node != NULL);

            m_node = m_node -> m_previous;
        }

    private:
        LinkNode<T>* m_node;
};

template<typename T>
class LinkList {
    public:
        LinkList() : m_size(0), m_root(0), m_lastNode(0) {}

        ~LinkList() {
            while(m_root != NULL) {
                Pop();
            }
        }

        LinkNode<T>* Begin() {
            assert(m_root != NULL);

            return m_root;
        }

        LinkNode<T>* Last() {
            return m_lastNode;
        }

        LinkNode<T>* End() {
            return NULL;
        }

        void Push(T newData) {
            LinkNode<T>* node = new LinkNode<T>;

            assert(node != NULL);

            node -> m_data = newData;
            node -> m_next = NULL;
            node -> m_previous = NULL;

            if(m_lastNode != NULL) {
                m_lastNode -> m_next = node;
                node -> m_previous = m_lastNode;
            }
            else {
                m_root = node;
            }

            m_lastNode = node;
            m_size++;
        }

        void Pop() {
            assert(m_root != NULL);

            if(m_root -> m_next == NULL) {
                delete m_root;

                m_root = NULL;
            }
            else {
                LinkNode<T>* prevNode = m_lastNode -> m_previous;

                prevNode -> m_next = NULL;

                delete m_lastNode;

                m_lastNode = prevNode;
            }

            m_size = (m_size == 0 ? m_size : m_size - 1);
        }

        int GetSize() {
            return m_size;
        }

        void Push_Front(T newData) {
            LinkNode<T>* node = new LinkNode<T>;

            assert(node != NULL);

            node -> m_data = newData;
            node -> m_next = NULL;
            node -> m_previous = NULL;

            if(m_root != NULL) {
                node -> m_next = m_root;
                m_root -> m_previous = node;
                m_root = node;
            }
            else {
                m_root = node;
                m_lastNode = node;
            }

            m_size++;
        }

        void Pop_Front() {
            assert(m_root != NULL);

            LinkNode<T>* temp = m_root;

            m_root = m_root -> m_next;

            if(m_root != NULL) {
                m_root -> m_previous = NULL;
            }

            delete temp;

            m_size = (m_size == 0 ? m_size : m_size - 1);
        }

    private:
        int m_size;
        LinkNode<T>* m_root;
        LinkNode<T>* m_lastNode;
};

template<typename T>
class Queue {
    public:
        Queue(int size) {
            assert(size > 0);

            m_size = size;
        }

        ~Queue() {}

        void push(T val) {
            if(m_elements.GetSize() < m_size) {
                m_elements.Push(val);
            }
        }

        void pop() {
            m_elements.Pop_Front();
        }

        const T& front() {
            LinkIterator<T> it;

            it = m_elements.Begin();

            return *it;
        }

        int GetSize() {return m_elements.GetSize();}
        int GetMaxSize() {return m_size;}
        bool isEmpty() {return (m_elements.GetSize() == 0);}

        void Resize(int size) {
            assert(size > 0);

            m_size = size;
        }

    private:
        LinkList<T> m_elements;
        int m_size;
};

int main(int argc, char* argv[]) {
    std::cout << "Queue examples" << std::endl;
    std::cout << "Chapter 6: Stacks and Queues" << std::endl;
    std::cout << std::endl;

    const int size = 5;

    Queue<int> intQueue(size);

    for(int i = 0; i < size; ++i) {
        intQueue.push(10 + i);
    }

    std::cout << "Queue contents (size - " << intQueue.GetSize() << "): ";

    while(intQueue.isEmpty() == false) {
        std::cout << " " << intQueue.front();

        intQueue.pop();
    }

    std::cout << "." << std::endl << std::endl;

    if(intQueue.isEmpty() == true) {
        std::cout << "The int queue is empty" << std::endl << std::endl;
    }
    else {
        std::cout << "The int queue is NOT empty" << std::endl << std::endl;
    }

    return 1;
}