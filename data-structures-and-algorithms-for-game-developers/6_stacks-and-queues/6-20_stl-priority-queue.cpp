#include <iostream>
#include <queue>

int main(int argc, char* argv[]) {
    std::cout << "STL Priority Queue Example" << std::endl;
    std::cout << "Chapter 6: Stacks and Queues" << std::endl;
    std::cout << std::endl;

    std::priority_queue<int> priQueue;

    for(int i = 0; i < 5; ++i) {
        priQueue.push(88 + i);
    }

    std::cout
        << "Priority queue (int) contents ("
        << "Size: "
        << (int)priQueue.size()
        << ") - ";

    int size = (int)priQueue.size();

    for(int i = 0; i < size; ++i) {
        std::cout << " " << priQueue.top();
        priQueue.pop();
    }

    std::cout << "." << std::endl;

    if(priQueue.empty() == true) {
        std::cout << "Priority queue (int) is empty";
    }
    else {
        std::cout << "Priority queue (int) is NOT empty";
    }

    std::cout << std::endl << std::endl;

    return 1;
}