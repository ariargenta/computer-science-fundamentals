#include <iostream>
#include <list>
#include <algorithm>
#include <numeric>
#include <iterator>

void PrintList(std::list<int>& lList) {
    std::cout << "Contents (" << "Size: " << (int)lList.size() << ") - ";

    std::ostream_iterator<int> output(std::cout, " ");

    copy(lList.begin(), lList.end(), output);

    std::cout << std::endl;
}

void PrintListReverse(std::list<int>& lList) {
    std::cout << "Contents (" << "Size: " << (int)lList.size() << ") - ";

    std::ostream_iterator<int> output(std::cout, " ");

    copy(lList.rbegin(), lList.rend(), output);

    std::cout << std::endl;
}

int main(int argc, char* argv[]) {
    std::cout << "STL link list example" << std::endl;
    std::cout << "Data Structures for Game Developers" << std::endl;
    std::cout << "Aria Argenta" << std::endl << std::endl;

    std::list<int> lList;

    lList.push_back(10);
    lList.push_back(20);
    lList.push_back(30);
    lList.push_back(40);
    lList.push_back(50);

    std::list<int> lList2;

    for(int i = 0; i < 5; ++i) {
        lList2.push_back(0);
    }

    copy(lList.begin(), lList.end(), lList2.begin());

    std::cout << "Inserted into list: ";

    PrintList(lList);

    std::cout << "Reverse contents: ";

    PrintListReverse(lList);

    lList.sort();

    std::cout << "Sorting the list: ";

    PrintList(lList);

    lList.reverse();

    std::cout << "Reverse the list: ";

    PrintList(lList);

    lList.push_front(60);
    lList.push_front(70);
    lList.pop_front();
    lList.push_front(80);

    std::cout << "Push/Pop front: ";

    PrintList(lList);

    std::cout << "Accumulate: " << accumulate(lList.begin(), lList.end(), 0) << std::endl;

    lList.pop_back();
    lList.pop_back();

    std::cout << "Popped two from list: ";

    PrintList(lList);

    lList.clear();

    std::cout << "Cleared list: ";

    PrintList(lList);

    std::cout << std::endl;

    if(lList.empty() == true) {
        std::cout << "List is empty";
    }
    else {
        std::cout << "List is NOT empty";
    }

    std::cout << std::endl << std::endl;

    return 1;
}