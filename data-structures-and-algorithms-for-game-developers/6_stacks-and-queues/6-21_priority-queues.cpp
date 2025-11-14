#include <iostream>
#include <queue>
#include <vector>

template<typename T>
class less_cmp {
    public:
        inline bool operator()(T lVal, T rVal) {
            return (lVal < rVal);
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
        NetworkMessage(int data) : m_data(data) {}
        ~NetworkMessage() {}

        bool operator<(const NetworkMessage& obj) const {
            return (m_data < obj.GetData());
        }

        bool operator>(const NetworkMessage& obj) const {
            return !(m_data < obj.GetData());
        }

        int GetData() const {
            return m_data;
        }

    private:
        int m_data;
};

int main(int argc, char** argv) {
    std::cout << "STL Priority Queue Example" << std::endl;
    std::cout << "Chapter 6: Stacks and Queues" << std::endl;
    std::cout << std::endl;

    std::priority_queue<
        NetworkMessage
        , std::vector<NetworkMessage>
        , less_cmp<NetworkMessage>
    > priQueue;

    std::priority_queue<
        NetworkMessage*
        , std::vector<NetworkMessage*>
        , greater_cmp_ptr<NetworkMessage*>
    > priQueuePtr;

    priQueue.push(NetworkMessage(5));
    priQueue.push(NetworkMessage(35));
    priQueue.push(NetworkMessage(2));
    priQueue.push(NetworkMessage(53));
    priQueuePtr.push(new NetworkMessage(14));
    priQueuePtr.push(new NetworkMessage(67));
    priQueuePtr.push(new NetworkMessage(13));
    priQueuePtr.push(new NetworkMessage(12));

    std::cout << "Priority queue contents:" << std::endl;

    int size = (int)priQueue.size();

    for(int i = 0; i < size; ++i) {
        std::cout << " " << priQueue.top().GetData() << std::endl;

        priQueue.pop();
    }

    std::cout << std::endl;
    std::cout << "Priority queue ptr contents:" << std::endl;

    size = (int)priQueuePtr.size();

    for(int i = 0; i < size; ++i) {
        NetworkMessage* ptr = priQueuePtr.top();

        if(ptr != NULL) {
            std::cout << " " << ptr -> GetData();

            delete ptr;

            std::cout << " (deleted)" << std::endl;
        }

        priQueuePtr.pop();
    }

    std::cout << std::endl << std::endl;

    return 1;
}