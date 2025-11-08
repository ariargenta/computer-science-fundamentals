#include <iostream>
#include <vector>
#include <algorithm>
#include <functional>
#include <iterator>

inline bool CompareNoCase(char lVal, char rVal) {
    return tolower(lVal) < tolower(rVal);
}

int main(int argc, char** argv) {
    std::cout << "STL sorting algorithm" << std::endl;
    std::cout << "Chapter 4: Sorting" << std::endl << std::endl;

    char str1[] = "lekiamhjdqn";
    char str2[] = "peuyxknasbd";

    std::vector<int> int1;

    int1.push_back(58);
    int1.push_back(23);
    int1.push_back(1);
    int1.push_back(53);
    int1.push_back(33);
    int1.push_back(84);
    int1.push_back(12);

    std::cout << "Original str1 data: " << str1 << "." << std::endl;

    std::sort(str1, str1 + (sizeof(str1) - 1), CompareNoCase);

    std::cout << "Sorted str1 data: " << str1 << "." << std::endl;
    std::cout << std::endl;
    std::cout << "Original str2 data: " << str2 << "." << std::endl;

    std::stable_sort(str2, str2 + (sizeof(str2) - 1), CompareNoCase);

    std::cout << "Sorted str2 data: " << str2 << "." << std::endl;
    std::cout << std::endl;

    std::ostream_iterator<int> output(std::cout, " ");

    std::cout << "Original int1 data: ";

    copy(int1.begin(), int1.end(), output);

    std::cout << std::endl;

    partial_sort(int1.begin(), int1.begin() + int1.size(), int1.end(), std::less<int>());

    std::cout << "Sorted int1 data: ";

    copy(int1.begin(), int1.end(), output);

    std::cout << std::endl << std::endl;

    return 1;
}