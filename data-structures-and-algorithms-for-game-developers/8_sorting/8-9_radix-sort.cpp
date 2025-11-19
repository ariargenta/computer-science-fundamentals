#include <iostream>
#include <deque>

#define BASE 10
#define MAX_POSITIONS 2

void RadixSort(int* array, int size) {
    int b = 0;
    int r = 0;
    int i = 0;
    int index = 0;
    int factor = 0;

    std::deque<int> qList[BASE];

    for(b = 1, factor = 1; b <= MAX_POSITIONS; factor *= BASE, ++b) {
        for(r = 0; r < size; ++r) {
            index = (array[r] / factor) % BASE;

            qList[index].push_back(array[r]);
        }

        for(r = 0, i = 0; r < BASE; ++r) {
            while(qList[r].empty() != true) {
                array[i++] = qList[r].front();

                qList[r].pop_front();
            }
        }
    }
}

int main(int argc, char* argv[]) {
    std::cout << "Radix sort example" << std::endl;
    std::cout << "Chapter 8: Advanced Sorting" << std::endl;
    std::cout << std::endl;

    const int size = 10;
    int array[size];
    int i = 0;

    for(i = 0; i < size; ++i) {
        array[i] = 10 + rand() % 89;
    }

    std::cout << "Array contents before sort: ";

    for(i = 0; i < size; ++i) {
        std::cout << " " << array[i];
    }

    std::cout << std::endl;

    RadixSort(array, size);

    std::cout << "Array contents after sort: ";

    for(i = 0; i < size; ++i) {
        std::cout << " " << array[i];
    }

    std::cout << std::endl << std::endl;

    return 1;
}