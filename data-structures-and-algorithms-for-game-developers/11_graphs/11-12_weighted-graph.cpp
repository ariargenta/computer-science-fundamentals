#include <iostream>
#include <cassert>
#include <cstring>
#include <vector>
#include <algorithm>
#include <queue>

class EdgeInfo {
    public:
        EdgeInfo() : m_v1Index(0), m_v2Index(0), m_weight(0) {}

        bool operator<(const EdgeInfo& e2) {
            return (m_weight < e2.m_weight);
        }

        bool operator==(const EdgeInfo& e2) {
            return (m_v2Index == e2.m_v2Index);
        }

        int m_v1Index;
        int m_v2Index;
        int m_weight;
};

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

        void attachEdge(int index1, int index2, int weight = 1) {
            assert(m_adjMatrix != NULL);

            m_adjMatrix[index1][index2] = weight;
            m_adjMatrix[index2][index1] = weight;
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

        void DisplayMST(std::string& output) {
            assert(m_adjMatrix != NULL);
            assert(m_vertVisits != NULL);

            int currentVert = 0;
            int totalChecked = 0;
            int size = (int)m_vertices.size();

            std::vector<EdgeInfo> vList;

            while(totalChecked < size - 1) {
                m_vertVisits[currentVert] = 1;

                totalChecked++;

                for(int i = 0; i < size; ++i) {
                    if(i == currentVert || m_vertVisits[i] == 1 || m_adjMatrix[currentVert][i] == 0) {
                        continue;
                    }

                    EdgeInfo edge;

                    edge.m_v1Index = currentVert;
                    edge.m_v2Index = i;
                    edge.m_weight = m_adjMatrix[currentVert][i];

                    std::vector<EdgeInfo>::iterator it = find(vList.begin(), vList.end(), edge);

                    if(it == vList.end()) {
                        vList.push_back(edge);
                    }
                    else {
                        if(edge.m_weight <= (*it).m_weight) {
                            (*it).m_v1Index = edge.m_v1Index;
                            (*it).m_v2Index = edge.m_v2Index;
                            (*it).m_weight = edge.m_weight;
                        }
                    }
                }

                if(vList.empty() == true) {
                    output = "Error: Graph is not connected";

                    return;
                }

                sort(vList.rbegin(), vList.rend());

                int endIndex = (int)vList.size() - 1;
                int v1 = vList[endIndex].m_v1Index;

                currentVert = vList[endIndex].m_v2Index;

                output += m_vertices[v1].GetNode();
                output += ":";
                output += m_vertices[currentVert].GetNode();
                output += " ";

                vList.pop_back();
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
    std::cout << "Graphs - Weighted MST" << std::endl;
    std::cout << "Chapter 11: Graphs" << std::endl;
    std::cout << std::endl;

    Graph<char> demoGraph(5);

    std::string output;

    demoGraph.push('A');
    demoGraph.push('B');
    demoGraph.push('C');
    demoGraph.push('D');
    demoGraph.push('E');

    //Attach A to each other vertices
    demoGraph.attachEdge(0, 1, 1);
    demoGraph.attachEdge(0, 2, 2);
    demoGraph.attachEdge(0, 3, 4);
    demoGraph.attachEdge(0, 4, 4);

    //Attach B to each other vertices
    demoGraph.attachEdge(1, 2, 1);
    demoGraph.attachEdge(1, 3, 3);
    demoGraph.attachEdge(1, 4, 4);

    //Attach C to each other vertices
    demoGraph.attachEdge(2, 3, 1);
    demoGraph.attachEdge(2, 4, 4);

    //Attach D to each other vertices
    demoGraph.attachEdge(3, 4, 1);

    demoGraph.DisplayMST(output);

    std::cout << "Minimum Spanning Tree: " << output;

    std::cout << std::endl << std::endl;

    return 1;
}