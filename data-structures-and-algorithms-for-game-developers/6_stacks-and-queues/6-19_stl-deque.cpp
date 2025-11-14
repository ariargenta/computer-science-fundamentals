#include <iostream>
#include <deque>
#include <algorithm>
#include <numeric>
#include <iterator>

void PrintDeque(std::deque<int>& deq) {
    std::cout << "Contents (" << "Size: " << (int)deq.size() << ") - ";

    std::ostream_iterator<int> output(std::cout, " ");

    copy(deq.begin(), deq.end(), output);

    std::cout << std::endl;
}

void PrintDequeReverse(std::deque<int>& deq) {
    std::cout << "Contents (" << "Size: " << (int)deq.size() << ") - ";

    std::ostream_iterator<int> output(std::cout, " ");

    copy(deq.rbegin(), deq.rend(), output);

    std::cout << std::endl;
}

int main(int agrc, char* argv[]) {
    std::cout << "STL Deque Example" << std::endl;
    std::cout << "Chapter 6: Stacks and Queues" << std::endl;
    std::cout << std::endl;

    std::deque<int> intDeque;

    for(int i = 0; i < 5; ++i) {
        intDeque.push_back(66 + i);
    }

    std::cout << "Inserted into deque: ";

    PrintDeque(intDeque);

    std::cout << "Reversed deque: ";

    PrintDequeReverse(intDeque);

    std::cout << "Deque front(): " << intDeque.front() << "." << std::endl;
    std::cout << "Deque back(): " << intDeque.back() << "." << std::endl;

    intDeque.pop_back();
    intDeque.pop_back();

    std::cout << "Popped two from deque: ";

    PrintDeque(intDeque);

    intDeque.clear();

    std::cout << "Cleared deque: ";

    PrintDeque(intDeque);

    std::cout << std::endl;

    if(intDeque.empty() == true) {
        std::cout << "Deque is empty";
    }
    else {
        std::cout << "Deque is NOT empty";
    }

    std::cout << std::endl << std::endl;

    return 1;
}