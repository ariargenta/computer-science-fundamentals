int triangularNumber(int term) {
    int value = 0;

    for(; term > 0; --term) {
        value += term;
    }

    return value;
}