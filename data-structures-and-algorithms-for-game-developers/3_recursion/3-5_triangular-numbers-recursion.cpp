int TriangularNumber(int term) {
    assert(term >= 1);

    if(term == 1) {
        return 1;
    }

    return(TriangularNumber(term - 1) + term);
}