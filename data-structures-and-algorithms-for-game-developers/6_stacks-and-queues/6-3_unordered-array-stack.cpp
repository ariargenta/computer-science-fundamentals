template<class T>
class UnorderedArray {
    public:
        UnorderedArray(int size, int growBy = 1):
            m_array(NULL)
            , m_maxSize(0)
            , m_growSize(0)
            , m_numElements(0)
        {
            if(size) {
                m_maxSize = size;
                m_array = new T[m_maxSize];

                std::memset(m_array, 0, sizeof(T) * m_maxSize);

                m_growSize = ((growBy > 0) ? growBy : 0);
            }
        }

        virtual ~UnorderedArray() {
            if(m_array != NULL) {
                delete[] m_array;

                m_array = NULL;
            }
        }

        virtual void push(T val) {
            assert(m_array != NULL);

            if(m_numElements >= m_maxSize) {
                Expand();
            }

            m_array[m_numElements] = val;
            m_numElements++;
        }

        void pop() {
            if(m_numElements > 0) {
                m_numElements--;
            }
        }

        void remove(int index) {
            assert(m_array != NULL);

            if(index >= m_numElements) {
                return;
            }

            for(int k = index; k < m_numElements - 1; ++k) {
                m_array[k] = m_array[k + 1];
            }

            m_numElements--;
        }

        virtual T& operator[](int index) {
            assert(m_array != NULL && index <= m_numElements);

            return m_array[index];
        }

        virtual int search(T val) {
            assert(m_array != NULL);

            for(int i = 0; i < m_numElements; ++i) {
                if(m_array[i] == val) {
                    return i;
                }
            }

            return -1;
        }

        void clear() {m_numElements = 0;}
        int GetSize() {return m_numElements;}
        int GetMaxSize() {return m_maxSize;}
        int GetGrowSize() {return m_growSize;}

        void SetGrowSize(int val) {
            assert(val >= 0);

            m_growSize = val;
        }

    private:
        bool Expand() {
            if(m_growSize <= 0) {
                return false;
            }

            T* temp = new T[m_maxSize + m_growSize];

            assert(temp != NULL);

            memcpy(temp, m_array, sizeof(T) * m_maxSize);

            delete[] m_array;

            m_array = temp;
            m_maxSize += m_growSize;

            return true;
        }

        T* m_array;
        int m_maxSize;
        int m_growSize;
        int m_numElements;
};

template<typename T>
class Stack {
    public:
        Stack(int size, int growBy = 1) {
            assert(size > 0 && growBy >= 0);

            m_container = new UnorderedArray<T>(size, growBy);

            assert(m_container != NULL);
        }

        ~Stack() {
            if(m_container != NULL) {
                delete m_container;

                m_container = NULL;
            }
        }

        void push(T val) {
            assert(m_container != NULL);

            m_container -> push(val);
        }

        void pop() {
            assert(m_container != NULL);

            m_container -> pop();
        }

        const T& top() {
            assert(m_container != NULL);

            return (*m_container)[m_container -> GetSize() - 1];
        }

        int GetSize() {
            assert(m_container != NULL);

            return m_container -> GetSize();
        }

        int GetMaxSize() {
            assert(m_container != NULL);

            return m_container -> GetMaxSize();
        }

        bool isEmpty() {
            assert(m_container != NULL);

            return (m_container -> GetSize() == 0);
        }

        bool isFull() {
            assert(m_container != NULL);

            return (m_top == m_container -> GetMaxSize() - 1);
        }

    private:
        UnorderedArray<T>* m_container;
};