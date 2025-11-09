#include <iostream>
#include <cassert>

template<typename T> class LinkIterator;
template<typename T> class LinkList;

template<typename T>
class LinkNode {
    friend class LinkIterator<T>;
    friend class LinkList<T>;

    private:
        T m_data;
        LinkNode* m_next;
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

        LinkNode<T>* End() {
            return NULL;
        }

        void Push(T newData) {
            LinkNode<T>* node = new LinkNode<T>;

            assert(node != NULL);
            node -> m_data = newData;
            node -> m_next = NULL;

            if(m_lastNode != NULL) {
                m_lastNode -> m_next = node;
                m_lastNode = node;
            }
            else {
                m_root = node;
                m_lastNode = node;
            }

            m_size++;
        }

        void Push_Front(T newData) {
            LinkNode<T>* node = new LinkNode<T>;

            assert(node != NULL);

            node -> m_data = newData;
            node -> m_next = NULL;

            if(m_root != NULL) {
                node -> m_next = m_root;
                m_root = node;
            }
            else {
                m_root = node;
                m_lastNode = node;
            }

            m_size++;
        }

        void Pop() {
            assert(m_root != NULL);

            if(m_root -> m_next == NULL) {
                delete m_root;

                m_root = NULL;
            }
            else {
                LinkNode<T>* prevNode = m_root;

                while(prevNode -> m_next != NULL && prevNode -> m_next != m_lastNode) {
                    prevNode = prevNode -> m_next;
                }

                delete m_lastNode;

                prevNode -> m_next = NULL;
                m_lastNode = prevNode;
            }

            m_size = (m_size == 0 ? m_size : m_size - 1);
        }

        void Pop_Front() {
            assert(m_root != NULL);

            LinkNode<T>* temp = m_root;

            m_root = m_root -> m_next;

            delete temp;

            m_size = (m_size == 0 ? m_size : m_size - 1);
        }

        int GetSize() {
            return m_size;
        }

    private:
        int m_size;
        LinkNode<T>* m_root;
        LinkNode<T>* m_lastNode;
};

int main(int argc, char* argv[]) {
    std::cout << "Double-Ended link list example" << std::endl;
    std::cout << "Chapter 4: Link Lists" << std::endl;
    std::cout << std::endl;

    LinkList<int> lList;

    lList.Push(101);
    lList.Push_Front(201);
    lList.Push(301);
    lList.Push_Front(401);
    lList.Push(501);
    lList.Pop();
    lList.Push(601);
    lList.Pop_Front();

    LinkIterator<int> it;

    std::cout << "Contents of the link list:";

    for(it = lList.Begin(); it != lList.End(); ++it) {
        std::cout << " " << *it;
    }

    std::cout << "." << std::endl << std::endl;

    return 1;
}