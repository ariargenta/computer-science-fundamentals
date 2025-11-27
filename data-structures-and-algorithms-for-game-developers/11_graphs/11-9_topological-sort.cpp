#include <iostream>
#include <cassert>
#include <cstring>
#include <vector>
#include <stack>
#include <queue>
#include <list>
#include <iterator>

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

        int GetVertNoSuccessor(char** adjMat, int size) {
            bool edgeFound = false;

            for(int row = 0; row < size; ++row) {
                edgeFound = false;

                for(int col = 0; col < size; ++col) {
                    if(adjMat[row][col] != 0) {
                        edgeFound = true;

                        break;
                    }
                }

                if(edgeFound == false) {
                    return row;
                }
            }

            return -1;
        }

        bool topologicalSort(std::list<T>& output) {
            bool hasCycles = false;

            std::vector<GraphVertex<T>> tempVerts(m_vertices);

            int tempSize = (int)tempVerts.size();

            char** tempAdjMat = new char*[m_maxVerts];

            assert(tempAdjMat != NULL);

            for(int i = 0; i < m_maxVerts; ++i) {
                tempAdjMat[i] = new char[m_maxVerts];

                assert(tempAdjMat[i] != NULL);

                memcpy(tempAdjMat[i], m_adjMatrix[i], m_maxVerts);
            }

            while(tempSize > 0) {
                int v = GetVertNoSuccessor(tempAdjMat, tempSize);

                if(v == -1) {
                    hasCycles = true;

                    break;
                }

                output.push_front(tempVerts[v].GetNode());

                if(v != (tempSize - 1)) {
                    typename std::vector<GraphVertex<T>>::iterator it;

                    it = tempVerts.begin() + v;

                    tempVerts.erase(it);

                    for(int row = v; row < tempSize - 1; ++row) {
                        for(int c = 0; c < tempSize; ++c) {
                            tempAdjMat[row][c] = tempAdjMat[row + 1][c];
                        }
                    }

                    for(int col = v; col < tempSize - 1; ++col) {
                        for(int r = 0; r < tempSize; ++r) {
                            tempAdjMat[r][col] = tempAdjMat[r][col + 1];
                        }
                    }
                }

                tempSize--;
            }

            if(tempAdjMat != NULL) {
                for(int i = 0; i < m_maxVerts; ++i) {
                    if(tempAdjMat[i] != NULL) {
                        delete[] tempAdjMat[i];

                        tempAdjMat[i] = NULL;
                    }
                }

                delete[] tempAdjMat;

                tempAdjMat = NULL;
            }

            return !hasCycles;
        }

    private:
        std::vector<GraphVertex<T>> m_vertices;
        int m_maxVerts;
        char** m_adjMatrix;
        char* m_vertVisits;
};

int main(int argc, char* argv[]) {
    std::cout << "Graphs - Topological Sorting" << std::endl;
    std::cout << "Chapter 11: Graphs" << std::endl;
    std::cout << std::endl;

    Graph<char> demoGraph(6);

    demoGraph.push('A');    //0
    demoGraph.push('B');    //1
    demoGraph.push('C');    //2
    demoGraph.push('D');    //3
    demoGraph.push('E');    //4
    demoGraph.push('F');    //5

    // Attach A:B A:C B:D C:E D:E E:F
    demoGraph.attachDirectedEdge(0, 1);
    demoGraph.attachDirectedEdge(0, 2);
    demoGraph.attachDirectedEdge(1, 3);
    demoGraph.attachDirectedEdge(2, 4);
    demoGraph.attachDirectedEdge(3, 4);
    demoGraph.attachDirectedEdge(4, 5);

    std::cout << "Breadth-First Search nodes visited: ";

    bool result = demoGraph.BreadthFirstSearch(0, 3);

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

    std::list<char> tsResult;

    if(demoGraph.topologicalSort(tsResult) == true) {
        std::cout << "Topological Sort: ";

        std::ostream_iterator<char> output(std::cout, " ");

        copy(tsResult.begin(), tsResult.end(), output);
    }
    else {
        std::cout << "There are cycles in the graph!";
    }

    std::cout << std::endl << std::endl;

    return 1;
}