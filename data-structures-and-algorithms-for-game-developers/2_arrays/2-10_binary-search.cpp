template<class T>
class OrderedArray {
    public:
        int search(T searchKey) {
            assert(m_array != NULL);

            int lowerBound = 0;
            int upperBound = m_numElements - 1;
            int current = 0;

            while(1) {
                current = (lowerBound + upperBound) >> 1;

                if(m_array[current] == searchKey) {
                    return current;
                }
                else if(lowerBound > upperBound) {
                    return -1;
                }
                else {
                    if(m_array[current] < searchKey) {
                        lowerBound = current + 1;
                    }
                    else {
                        upperBound = current - 1;
                    }
                }
            }

            return -1;
        }
};