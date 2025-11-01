template<class T>
class UnorderedArray {
    public:
        UnorderedArray(int size, int growBy = 1) :
            m_array(NULL)
            , m_maxSize(0)
            , m_growSize(0)
            , m_numElements(0)
        {
                if(size) {
                    m_maxSize = size;
                    m_array = new T[m_maxSize];
                    m_growSize = ((growBy > 0) ? growBy : 0);
                }
            }

        ~UnorderedArray() {
            if(m_array != NULL) {
                delete[] m_array;

                m_array = NULL;
            }
        }

    private:
        T *m_array;
        int m_maxSize;
        int m_growSize;
        int m_numElements;
};