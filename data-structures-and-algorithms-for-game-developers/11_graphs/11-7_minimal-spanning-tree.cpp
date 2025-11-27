#include <iostream>
#include <cassert>
#include <cstring>
#include <vector>
#include <stack>
#include <queue>

template<typename T>
class GraphVertex {
    public:
        GraphVertex(T node) : m_node(node) {}

        T GetNode() {
            return m_node;
        }

    private:
        T m_node;
};

template<typename T>
class Graph {
    public:
        Graph(int numVerts) : m_maxVerts(numVerts), m_adjMatrix(NULL) {
            assert(numVerts > 0);

            m_vertices.reserve(m_maxVerts);

            m_adjMatrix = new char*[m_maxVerts];

            assert(m_adjMatrix != NULL);

            m_vertVisits = new char[m_maxVerts];

            assert(m_vertVisits != NULL);

            memset(m_vertVisits, 0, m_maxVerts);

            for(int i = 0; i < m_maxVerts; ++i) {
                m_adjMatrix[i] = new char[m_maxVerts];
                
                assert(m_adjMatrix[i] != NULL);

                memset(m_adjMatrix[i], 0, m_maxVerts);
            }
        }

        ~Graph() {
            if(m_adjMatrix != NULL) {
                for(int i = 0; i < m_maxVerts; ++i) {
                    if(m_adjMatrix[i] != NULL) {
                        delete[] m_adjMatrix[i];

                        m_adjMatrix[i] = NULL;
                    }
                }

                delete[] m_adjMatrix;

                m_adjMatrix = NULL;
            }

            if(m_vertVisits != NULL) {
                delete[] m_vertVisits;

                m_vertVisits = NULL;
            }
        }

        bool push(T node) {
            if((int)m_vertices.size() >= m_maxVerts) {
                return false;
            }

            m_vertices.push_back(GraphVertex<T>(node));

            return true;
        }

        void attachEdge(int index1, int index2) {
            assert(m_adjMatrix != NULL);

            m_adjMatrix[index1][index2] = 1;
            m_adjMatrix[index2][index1] = 1;
        }

        void attachDirectedEdge(int index1, int index2) {
            assert(m_adjMatrix != NULL);

            m_adjMatrix[index1][index2] = 1;
        }

        int getNextUnvisitedVertex(int index) {
            assert(m_adjMatrix != NULL);
            assert(m_vertVisits != NULL);

            for(int i = 0; i < (int)m_vertices.size(); ++i) {
                if(m_adjMatrix[index][i] == 1 && m_vertVisits[i] == 0) {
                    return i;
                }
            }

            return -1;
        }

        bool BreadthFirstSearch(int startIndex, int endIndex) {
            assert(m_adjMatrix != NULL);
            assert(m_vertVisits != NULL);

            m_vertVisits[startIndex] = 1;

            std::cout << m_vertices[startIndex].GetNode();

            std::queue<int> searchQueue;

            int vert1 = 0;
            int vert2 = 0;

            searchQueue.push(startIndex);

            while(searchQueue.empty() != true) {
                vert1 = searchQueue.front();

                searchQueue.pop();

                if(vert1 == endIndex) {
                    memset(m_vertVisits, 0, m_maxVerts);

                    return true;
                }

                while((vert2 = getNextUnvisitedVertex(vert1)) != -1) {
                    m_vertVisits[vert2] = 1;

                    std::cout << m_vertices[vert2].GetNode();

                    searchQueue.push(vert2);
                }
            }

            memset(m_vertVisits, 0, m_maxVerts);

            return false;
        }

        void DisplayMST() {
            assert(m_adjMatrix != NULL);
            assert(m_vertVisits != NULL);

            int startIndex = 0;

            m_vertVisits[startIndex] = 1;

            std::stack<int> searchStack;

            int vert = 0;
            int currentVert = 0;

            searchStack.push(startIndex);

            while(searchStack.empty() != true) {
                currentVert = searchStack.top();
                vert = getNextUnvisitedVertex(currentVert);

                if(vert == -1) {
                    searchStack.pop();
                }
                else {
                    m_vertVisits[vert] = 1;

                    searchStack.push(vert);

                    std::cout
                        << m_vertices[currentVert].GetNode()
                        << ":"
                        << m_vertices[vert].GetNode()
                        << " ";
                }
            }

            memset(m_vertVisits, 0, m_maxVerts);
        }

    private:
        std::vector<GraphVertex<T>> m_vertices;
        int m_maxVerts;
        char** m_adjMatrix;
        char* m_vertVisits;
};

int main(int argc, char* argv[]) {
    std::cout << "Graphs - Minimum Spanning Trees" << std::endl;
    std::cout << "Chapter 11: Graphs" << std::endl;
    std::cout << std::endl;

    Graph<char> demoGraph(6);

    demoGraph.push('A');
    demoGraph.push('B');
    demoGraph.push('C');
    demoGraph.push('D');
    demoGraph.push('E');
    demoGraph.push('F');
    demoGraph.attachEdge(0, 2); //A2C & C2A
    demoGraph.attachEdge(0, 3); //A2D & D2A
    demoGraph.attachEdge(1, 4); //B2E & E2B
    demoGraph.attachEdge(2, 5); //C2F & F2C

    std::cout << "Breadth-First Search nodes visited: ";

    int result = demoGraph.BreadthFirstSearch(0, 3);

    std::cout << std::endl << std::endl;

    if(result == 1) {
        std::cout << "Path from A to D found";
    }
    else {
        std::cout << "Path from A to D NOT found!";
    }

    std::cout << std::endl << std::endl;
    
    std::cout << "Minimum Spanning Tree: ";

    demoGraph.DisplayMST();

    std::cout << std::endl << std::endl;

    return 1;
}