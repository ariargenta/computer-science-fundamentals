template<typename T> class LinkIterator;
template<typename T> class LinkList;

template<typename T>
class LinkNode {
    friend class LinkIterator<T>;
    friend class LinkList<T>;

    private:
        T m_data;
        LinkNode* m_next;
};