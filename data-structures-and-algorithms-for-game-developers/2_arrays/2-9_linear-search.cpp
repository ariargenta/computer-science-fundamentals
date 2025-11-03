template<class T>
class UnorderedArray {
    public:
        virtual int search(T val) {
            assert(m_array != NULL);

            for(int i = 0; i < m_numElements; ++i) {
                if(m_array[i] == val) {
                    return i;
                }

                return -1;
            }
        }
};