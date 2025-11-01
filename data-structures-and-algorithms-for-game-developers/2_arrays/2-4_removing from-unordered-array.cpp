template<class T>
class UnorderedArray {
    public:
        void pop() {
            if(m_numElements > 0) {
                m_numElements--;
            }

            void remove(int index) {
                assert(m_array != NULL);

                if(index >= m_maxSize) {
                    return;
                }

                for(int k = index; k < m_maxSize - 1; ++k) {
                    m_array[k] = m_array[k + 1];
                }

                m_maxSize--;

                if(m_numElements >= m_maxSize) {
                    m_numElements = m_maxSize - 1;
                }
            }
        }
};