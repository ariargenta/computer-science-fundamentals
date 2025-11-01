#include <iostream>

template<typename T>
T min(T lVal, T rVal) {
    if(lVal > rVal) {
        return rVal;
    }

    return lVal;
}

template<typename T>
T max(T lVal, T rVal) {
    if(lVal < rVal) {
        return rVal;
    }

    return lVal;
}

template<typename P>
class TemplateClass {
    public:
        TemplateClass(P val) {
            m_val = val;
        }

        bool operator<(TemplateClass &rVal) {
            return m_val < rVal.GetVal();
        }

        bool operator>(TemplateClass& rVal) {
            return m_val > rVal.GetVal();
        }

        P GetVal() {
            return m_val;
        }

    private:
        P m_val;
};

int main(int argc, char** argv) {
    std::cout << "C++ Templates" << std::endl;
    std::cout << "Chapter 1: Templates" << std::endl;
    std::cout << std::endl;
    std::cout << "Min = " << min(32, 54) << std::endl;
    std::cout << "Max = " << max(49.3, 38.98) << std::endl;

    std::cout
        << "Max (objects) = "
        << max(
            TemplateClass<int>(7), TemplateClass<int>(4)
            )
            .GetVal()
        << std::endl;

    TemplateClass<int> obj(10);

    std::cout << "obj = " << obj.GetVal() << std::endl;
    std::cout << std::endl << std::endl;

    return 1;
}