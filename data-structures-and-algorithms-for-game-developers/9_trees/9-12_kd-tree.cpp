#include <iostream>
#include <vector>
#include <cassert>

template<class TYPE>
class KdTree;

template<class TYPE>
struct KdNode {
    friend KdTree<TYPE>;

    public:
        KdNode(std::vector<TYPE>& key) : m_key(key), m_left(NULL), m_right(NULL) {}

        ~KdNode() {
            if(m_left != NULL) {
                delete m_left;

                m_left = NULL;
            }

            if(m_right != NULL) {
                delete m_right;

                m_right = NULL;
            }
        }

    private:
        std::vector<TYPE> m_key;
        KdNode* m_left;
        KdNode* m_right;
};

template<typename TYPE>
class KdTree {
    public:
        KdTree(int depth) : m_root(0), m_depth(depth) {
            assert(depth > 0);
        }

        ~KdTree() {
            if(m_root != NULL) {
                delete m_root;

                m_root = NULL;
            }
        }

        void push(std::vector<TYPE>& key) {
            KdNode<TYPE>* newNode = new KdNode<TYPE>(key);

            if(m_root == NULL) {
                m_root = newNode;

                return;
            }

            KdNode<TYPE>* currentNode = m_root;
            KdNode<TYPE>* parentNode = m_root;
            int level = 0;

            while(1) {
                parentNode = currentNode;

                if(key[level] < currentNode -> m_key[level]) {
                    currentNode = currentNode -> m_left;

                    if(currentNode == NULL) {
                        parentNode -> m_left = newNode;

                        return;
                    }
                }
                else {
                    currentNode = currentNode -> m_right;

                    if(currentNode == NULL) {
                        parentNode -> m_right = newNode;

                        return;
                    }
                }

                level++;

                if(level >= m_depth) {
                    level = 0;
                }
            }
        }

        void displayRange(const std::vector<TYPE>& low, const std::vector<TYPE>& high) {
            displayRange(0, low, high, m_root);
        }

    private:
        void displayRange(
            int level
            , const std::vector<TYPE>& low
            , const std::vector<TYPE>& high
            , KdNode<TYPE>* node
        ) {
            if(node == NULL) {
                return;
            }

            bool inRange = true;

            for(int i = 0; i < m_depth; ++i) {
                if((low[i] > node -> m_key[i]) || (high[i] < node -> m_key[i])) {
                    inRange = false;

                    break;
                }
            }

            if(inRange) {
                std::cout << "(";

                for(int j = 0; j < m_depth; ++j) {
                    std::cout << node -> m_key[j];

                    if(j != m_depth - 1) {
                        std::cout << ", ";
                    }
                }

                std::cout << ")" << std::endl;
            }

            int nextLevel = (level + 1) % m_depth;

            if(low[level] <= node -> m_key[level]) {
                displayRange(nextLevel, low, high, node -> m_left);
            }

            if(high[level] >= node -> m_key[level]) {
                displayRange(nextLevel, low, high, node -> m_right);
            }
        }

        KdNode<TYPE>* m_root;
        int m_depth;
};

int main(int argc, char* argv[]) {
    std::cout << "KD Trees" << std::endl;
    std::cout << "Chapter 9: Trees" << std::endl;
    std::cout << std::endl;

    KdTree<int> kdTree(3);

    for(int i = 0; i < 100; ++i) {
        std::vector<int> key(3);

        key[0] = rand() % 100;
        key[1] = rand() % 100;
        key[2] = rand() % 100;

        kdTree.push(key);
    }

    std::vector<int> low(3);
    std::vector<int> high(3);

    low[0] = 20;
    low[1] = 30;
    low[2] = 25;
    high[0] = 90;
    high[1] = 70;
    high[2] = 80;

    std::cout << "Range (20, 30, 25) (90, 70, 80) match:" << std::endl;

    kdTree.displayRange(low, high);

    std::cout << std::endl << std::endl;

    return 1;
}