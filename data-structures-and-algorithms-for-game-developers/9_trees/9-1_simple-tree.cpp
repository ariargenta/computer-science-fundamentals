#include <iostream>

class Node {
    public:
        Node(int obj) : m_object(obj), m_next(NULL), m_prev(NULL), m_child(NULL) {
            std::cout << "Node created" << std::endl;
        }

        ~Node() {
            m_prev = NULL;

            if(m_child != NULL) {
                delete m_child;
            }

            if(m_next != NULL) {
                delete m_next;
            }

            m_child = NULL;
            m_next = NULL;

            std::cout << "Node deleted" << std::endl;
        }

        void AddChild(Node* node) {
            if(m_child == NULL) {
                m_child = node;
            }
            else {
                m_child -> AddSibling(node);
            }
        }

        void AddSibling(Node* node) {
            Node* ptr = m_next;

            if(m_next == NULL) {
                m_next = node;
                node -> m_prev = this;
            }
            else {
                while(ptr -> m_next != NULL) {
                    ptr = ptr -> m_next;
                }

                ptr -> m_next = node;
                node -> m_prev = ptr;
            }
        }

        void DisplayTree() {
            std::cout << m_object;

            if(m_next != NULL) {
                std::cout << " ";

                m_next -> DisplayTree();
            }

            if(m_child != NULL) {
                std::cout << std::endl;

                m_child -> DisplayTree();
            }
        }

        bool Search(int value) {
            if(m_object == value) {
                return true;
            }

            if(m_child != NULL) {
                if(m_child -> Search(value) == true) {
                    return true;
                }
            }

            if(m_next != NULL) {
                if(m_next -> Search(value) == true) {
                    return true;
                }
            }

            return false;
        }

    private:
        int m_object;
        Node* m_next;
        Node* m_prev;
        Node* m_child;
};

int main(int argc, char* argv[]) {
    std::cout << "Simple Tree Data Structure" << std::endl;
    std::cout << "Chapter 9: Trees" << std::endl << std::endl;

    Node* root = new Node(1);
    Node* subTree1 = new Node(3);

    root -> AddChild(new Node(2));

    subTree1 -> AddChild(new Node(5));
    subTree1 -> AddChild(new Node(6));

    root -> AddChild(subTree1);
    root -> AddChild(new Node(4));

    std::cout << std::endl;

    std::cout << "Tree contents by level:" << std::endl;

    root -> DisplayTree();

    std::cout << std::endl << std::endl;

    std::cout << "Searching for node 5: ";

    if(root -> Search(5) == true) {
        std::cout << "Node found" << std::endl;
    }
    else {
        std::cout << "Node NOT found" << std::endl;
    }

    std::cout << "Searching for node 9: ";

    if(root -> Search(9) == true) {
        std::cout << "Node found" << std::endl;
    }
    else {
        std::cout << "Node NOT found" << std::endl;
    }

    std::cout << std::endl;

    delete root;

    std::cout << std::endl << std::endl;

    return 1;
}