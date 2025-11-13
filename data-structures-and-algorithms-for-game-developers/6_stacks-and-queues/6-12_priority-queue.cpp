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

    friend class LinkList<T>;

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

        bool isValid() {return (m_node != NULL);}

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

        void Insert_Before(LinkIterator<T>& it, T newData) {
            assert(it.m_node != NULL);

            LinkNode<T>* node = new LinkNode<T>;

            assert(node != NULL);

            node -> m_data = newData;
            node -> m_next = it.m_node;
            node -> m_previous = it.m_node -> m_previous;

            if(node -> m_previous != NULL) {
                node -> m_previous -> m_next = node;
            }

            it.m_node -> m_previous = node;

            if(it.m_node == m_root) {
                m_root = node;
            }

            m_size++;
        }

        void Insert_After(LinkIterator<T>& it, T newData) {
            assert(it.m_node != NULL);

            LinkNode<T>* node = new LinkNode<T>;

            assert(node != NULL);

            node -> m_data = newData;
            node -> m_next = it.m_node -> m_next;
            node -> m_previous = it.m_node;

            if(node -> m_next != NULL) {
                node -> m_next -> m_previous = node;
            }

            it.m_node -> m_next = node;

            if(it.m_node == m_lastNode) {
                m_lastNode = node;
            }

            m_size++;
        }

    private:
        int m_size;
        LinkNode<T>* m_root;
        LinkNode<T>* m_lastNode;
};

template<typename T, typename CMP>
class PriorityQueue {
    public:
        PriorityQueue(int size) {
            assert(size > 0);

            m_size = size;
        }

        ~PriorityQueue() {}

        void push(T val) {
            assert(m_elements.GetSize() < m_size);

            if(m_elements.GetSize() == 0) {
                m_elements.Push(val);
            }
            else {
                LinkIterator<T> it;

                it = m_elements.Begin();

                CMP cmp;

                while(it.isValid()) {
                    if(cmp(val, *it)) {
                        break;
                    }

                    it++;
                }

                if(it.isValid()) {
                    m_elements.Insert_Before(it, val);
                }
                else {
                    m_elements.Push(val);
                }
            }
        }

        void pop() {m_elements.Pop_Front();}

        T& front() {
            LinkIterator<T> it;

            it = m_elements.Begin();

            return *it;
        }

        T& back() {
            LinkIterator<T> it;

            it = m_elements.Last();

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

template<typename T>
class less_cmp {
    public:
        inline bool operator()(T lVal, T rVal) {
            return(lVal < rVal);
        }
};

template<typename T>
class less_cmp_ptr {
    public:
        inline bool operator()(T lVal, T rVal) {
            return ((*lVal) < (*rVal));
        }
};

template<typename T>
class greater_cmp {
    public:
        inline bool operator()(T lVal, T rVal) {
            return !(lVal < rVal);
        }
};

template<typename T>
class greater_cmp_ptr {
    public:
        inline bool operator()(T lVal, T rVal) {
            return !((*lVal) < (*rVal));
        }
};

class NetworkMessage {
    public:
        NetworkMessage() : m_priority(0), m_id(0) {}
        NetworkMessage(int p, int id) : m_priority(p), m_id(id) {}
        ~NetworkMessage() {}

        int GetPriority() {return m_priority;}
        int GetID() {return m_id;}

        bool operator<(NetworkMessage& m) {
            if(m_priority < m.GetPriority()) {
                return true;
            }
            else if(m_id < m.GetID()) {
                return true;
            }

            return false;
        }

        bool operator>(NetworkMessage& m) {
            return !(*this < m);
        }

    private:
        int m_priority;
        int m_id;
};

int main(int argc, char** argv) {
    std::cout << "Priority Queue Data Structures Example" << std::endl;
    std::cout << "Chapter 6: Stacks and Queues" << std::endl;
    std::cout << std::endl;

    const int size = 4;

    PriorityQueue<NetworkMessage, less_cmp<NetworkMessage>> que(size);

    que.push(NetworkMessage(3, 100));
    que.push(NetworkMessage(2, 286));
    que.push(NetworkMessage(1, 362));
    que.push(NetworkMessage(3, 435));

    std::cout << "Priority queue contents (size - " << que.GetSize() << "): " << std::endl;

    while(que.isEmpty() == false) {
        std::cout << "Priority: " << que.front().GetPriority();
        std::cout << "    - ID: " << que.front().GetID();
        std::cout << std::endl;

        que.pop();
    }

    std::cout << std::endl;

    if(que.isEmpty() == true) {
        std::cout << "The container is empty" << std::endl << std::endl;
    }
    else {
        std::cout << "The container is NOT empty" << std::endl << std::endl;
    }

    return 1;
}