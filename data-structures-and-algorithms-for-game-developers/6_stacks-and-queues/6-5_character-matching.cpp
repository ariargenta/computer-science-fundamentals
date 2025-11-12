#include <iostream>
#include <cstring>
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

        LinkIterator<T>& operator = (LinkNode<T>* node) {
            m_node = node;

            return *this;
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

        int GetSize() {
            return m_size;
        }

        LinkNode<T>* Last() {
            return m_lastNode;
        }

    private:
        int m_size;
        LinkNode<T>* m_root;
        LinkNode<T>* m_lastNode;
};

template<typename T>
class Stack {
    public:
        Stack(int size) {}
        ~Stack() {}

        void push(T val) {
            m_container.Push(val);
        }

        T pop() {
            T val = T();

            if(m_container.Last() != NULL) {
                LinkIterator<T> it;

                it = this -> m_container.Last();

                val = *it;
            }

            m_container.Pop();

            return val;
        }

        const T& top() {
            LinkIterator<T> it;

            it = m_container.Last();

            return *it;
        }

        int GetSize() {return m_container.GetSize();}
        bool isEmpty() {return (m_container.GetSize() == 0);}

    private:
        LinkList<T> m_container;
};

void PrintError(char ch, int index) {
    std::cout << "Error " << ch << " at " << index << "." << std::endl;
}

void ParseString(char* str, int size) {
    if(str == NULL || size <= 0) {
        std::cout << "Error with parameters!" << std::endl << std::endl;

        return;
    }

    Stack<char> sList(size);
    char ch = 0;
    int errors = 0;

    for(int i = 0; i < size; ++i) {
        switch(str[i]) {
            case '{':
            case '(':
            case '[':
                sList.push(str[i]);

                break;
            case '}':
            case ')':
            case ']':
                if(sList.isEmpty() == false) {
                    ch = sList.pop();

                    if((ch != '{' && str[i] == '}') || (ch != '(' && str[i] == ')') || (ch != '[' && str[i] == ']')) {
                        PrintError(ch, i + 1);

                        errors++;
                    }
                }

                break;
        }
    }

    if(sList.isEmpty() && errors == 0) {
        std::cout << "No parsing errors" << std::endl << std::endl;
    }
    else if(sList.isEmpty() == false) {
        std::cout << "Unclosed characters: " << sList.GetSize() << "." << std::endl << std::endl;
    }
}

int main(int argc, char** argv) {
    std::cout << "Character matching with stacks example" << std::endl;
    std::cout << "Chapter 6: Stacks and Queues" << std::endl;
    std::cout << std::endl;

    char str[] = {'{', '(', 'a', '[', '5', ']', ')', '}'};
    int size = 8;

    std::cout << "Parsing str." << std::endl;

    ParseString(str, size);

    char str2[] = {'{', ')', 'b', '[', '1', ']', ')', '}'};
    size = 8;

    std::cout << "Parsing str2." << std::endl;

    ParseString(str2, size);

    std::cout << std::endl;

    return 1;
}