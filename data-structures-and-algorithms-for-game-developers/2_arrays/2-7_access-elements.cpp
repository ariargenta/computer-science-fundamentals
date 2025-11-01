template<class T>
class UnorderedArray {
    public:
        virtual T* operator[](int index) {
            assert(m_array != NULL && index <= m_numElements);

            return m_array[index];
        }
};