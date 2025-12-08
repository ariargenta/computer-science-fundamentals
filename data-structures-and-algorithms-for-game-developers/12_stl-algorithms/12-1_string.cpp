#include <iostream>
#include <string>
#include <algorithm>
#include <iterator>

int main(int argc, char* argv[]) {
    std::cout << "Strings example" << std::endl;
    std::cout << "Chapter 12: STL Algorithms" << std::endl;
    std::cout << std::endl;

    std::string str("Hello World");

    std::cout << "String contents: " << str << std::endl;

    str.clear();

    std::cout << "String contents after clear: " << str << std::endl;

    str = "Goodbye World";

    std::cout << "Assigning string contents: " << str << std::endl;

    str.push_back('!');

    std::cout << "String contents with iterators: ";

    std::ostream_iterator<char> output(std::cout, "");

    copy(str.begin(), str.end(), output);

    std::cout << std::endl;

    std::cout << "Reverse contents: ";

    copy(str.rbegin(), str.rend(), output);

    std::cout << std::endl << std::endl;

    std::cout << "Enter in a string and press enter: ";

    getline(std::cin, str);

    std::cout << std::endl;

    std::cout << "You've entered: " << str << std::endl << std::endl;

    return 1;
}