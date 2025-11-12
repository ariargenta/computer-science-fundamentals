#include <iostream>
#include <stack>
#include <vector>
#include <list>

template<typename T>
void DisplayStack(T& stack) {
    std::cout << "(Size - " << stack.size() << "): ";

    while(stack.empty() == false) {
        std::cout << " " << stack.top();

        stack.pop();
    }

    std::cout << "." << std::endl;
};

int main(int argc, char* argv[]) {
    std::cout << "STL Stacks Example" << std::endl;
    std::cout << "Chapter 6: Stacks and Queues" << std::endl;
    std::cout << std::endl;

    std::stack<int> intStack;
    std::stack<int, std::vector<int>> vecStack;
    std::stack<int, std::list<int>> listStack;

    for(int i = 0; i < 5; ++i) {
        intStack.push(11 + i);
        vecStack.push(22 + i);
        listStack.push(33 + i);
    }

    std::cout << "Contents of the int stack: ";

    DisplayStack(intStack);

    std::cout << "Contents of the int vector stack: ";

    DisplayStack(vecStack);

    std::cout << "Contents of the int list stack: ";
    
    DisplayStack(listStack);

    std::cout << std::endl;

    if(intStack.empty() == true) {
        std::cout << "The int stack is empty" << std::endl;
    }
    else {
        std::cout << "The int stack is NOT empty" << std::endl;
    }

    if(vecStack.empty() == true) {
        std::cout << "The vec int stack is empty" << std::endl;
    }
    else {
        std::cout << "The vec int stack is NOT empty" << std::endl;
    }

    if(listStack.empty() == true) {
        std::cout << "The list int stack is empty" << std::endl;
    }
    else {
        std::cout << "The list int stack is NOT empty" << std::endl;
    }

    std::cout << std::endl;

    return 1;
}