#include <bits/stdc++.h>

void bubbleSort(int* array, int size) {
    if(!array || !size) {
        return;
    }

    for(int j = size - 1; j > 0; --j) {
        for(int i = 0; i < j; ++i) {
            if(array[i] > array[i + 1]) {
                int temp = array[i];

                array[i] = array[i + 1];
                array[i + 1] = temp;
            }
        }
    }
}

int main(int argc, char* argv[]) {
    int* array = new int[5];

    array[0] = 80;
    array[1] = 64;
    array[2] = 99;
    array[3] = 76;
    array[4] = 5;

    std::cout << "Bubble sort algorithm" << std::endl << std::endl;
    std::cout << "Unordered array: ";

    for(int i = 0; i < 5; ++i) {
        std::cout << array[i] << " ";
    }

    std::cout << std::endl;

    bubbleSort(array, 5);

    std::cout << "Ordered array: ";

    for(int i = 0; i < 5; ++i) {
        std::cout << array[i] << " ";
    }

    delete[] array;

    array = NULL;

    std::cout << std::endl << std::endl;

    return 1;
}