#include <iostream>
#include <queue>
#include <list>

template<typename T>
void DisplayQueue(T& que) {
    std::cout << "(Size - " << que.size() << "): ";

    while(que.empty() == false) {
        std::cout << " " << que.front();

        que.pop();
    }

    std::cout << "." << std::endl;
};

int main(int argc, char* argv[]) {
    std::cout << "STL Queue Example" << std::endl;
    std::cout << "Chapter 6: Stacks and Queues" << std::endl;
    std::cout << std::endl;

    std::queue<int> intQueue;
    std::queue<int, std::list<int>> listQueue;

    for(int i = 0; i < 5; ++i) {
        intQueue.push(44 + i);
        listQueue.push(55 + i);
    }

    std::cout << "Contents of the int queue ";

    DisplayQueue(intQueue);

    std::cout << "Contents of the int list queue ";

    DisplayQueue(listQueue);

    std::cout << std::endl;

    if(intQueue.empty() == true) {
        std::cout << "The int queue is empty" << std::endl;
    }
    else {
        std::cout << "The int queue is NOT empy" << std::endl;
    }

    if(listQueue.empty() == true) {
        std::cout << "The list int queue is empty" << std::endl;
    }
    else {
        std::cout << "The list int queue is NOT empty" << std::endl;
    }

    std::cout << std::endl;

    return 1;
}