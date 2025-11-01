template<class T>
class UnorderedArray {
    private:
        bool Expand() {
            if(m_growSize <= 0) {
                return false;
            }

            T* temp = new T[m_maxSize + m_growSize];
            
            assert(temp != NULL);

            memcpy(temp, m_array, sizeof(T)) * m_maxSize;

            delete[] m_array;

            m_array = temp;
            m_maxSize += m_growSize;

            return true;
        }
};