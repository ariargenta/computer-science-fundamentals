template <class T>
class OrderedArray {
    public:
        int push(T val) {
            assert(m_array != NULL);

            if(m_numElements >= m_maxSize) {
                Expand();
            }

            for(int i = 0; i < m_numElements; ++i) {
                if(m_array[i] > val) {
                    break;
                }
            }

            for(int k = m_numElements; k > i; --k) {
                m_array[k] = m_array[k - 1];
            }

            m_array[i] = val;
            m_numElements++;

            return i;
        }
};