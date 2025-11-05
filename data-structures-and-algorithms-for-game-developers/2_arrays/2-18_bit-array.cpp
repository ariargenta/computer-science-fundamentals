#include <iostream>
#include <vector>
#include <cassert>

#define BYTE_BITS 8
#define BIT_TO_CHAR(bit) ((bit) / BYTE_BITS)
#define BIT_IN_CHAR(bit) (1 << (BYTE_BITS - 1 - ((bit) % BYTE_BITS)))
#define MIN_CHARS(bits) ((((bits) - 1) / CHAR_BIT) + 1)
#define MAX_UCHAR 0xff

class BitArray {
    public:
        BitArray(unsigned int size) {
            assert(size > 0);

            m_totalBits = size;
            m_totalBytes = MIN_CHARS(m_totalBits);
            m_bits.reserve(m_totalBytes);

            for(unsigned int i = 0; i < m_totalBytes; ++i) {
                m_bits.push_back(0);
            }
        }

        ~BitArray() {}
        void ClearAllBits() {m_bits.assign(m_bits.size(), 0);}

        void SetAllBits() {
            m_bits.assign(m_bits.size(), MAX_UCHAR);

            int bits = m_totalBits % BYTE_BITS;

            if(bits != 0) {
                unsigned char mask = MAX_UCHAR << (BYTE_BITS - bits);

                m_bits[BIT_TO_CHAR(m_totalBits - 1)] = mask;
            }
        }

        void SetByte(unsigned int byte, unsigned char val) {
            assert(m_totalBits > byte);

            m_bits[byte] = val;
        }

        void SetBit(unsigned int bit) {
            assert(m_totalBits > bit);

            m_bits[BIT_TO_CHAR(bit)] |= BIT_IN_CHAR(bit);
        }

        void ClearBit(unsigned int bit) {
            assert(m_totalBits > bit);

            unsigned char mask = BIT_IN_CHAR(bit);

            mask = ~mask;

            m_bits[BIT_TO_CHAR(bit)] &= mask;
        }

        bool operator[](unsigned int bit) const {
            assert(m_totalBits > bit);

            return((m_bits[BIT_TO_CHAR(bit)] & BIT_IN_CHAR(bit)) != 0);
        }

    private:
        std::vector<unsigned char> m_bits;
        unsigned int m_totalBits;
        unsigned int m_totalBytes;
};

int main(int argc, char* argv[]) {
    BitArray bitArray(16);

    std::cout << "Bit Array Example" << std::endl;
    std::cout << "Data Structures for Game Developers" << std::endl;
    std::cout << "Aria Argenta" << std::endl << std::endl;
    std::cout << "Initial bit values for 2, 3 and 14" << std::endl << std::endl;
    std::cout << "Bit 2 = " << bitArray[2] << "." << std::endl;
    std::cout << "Bit 3 = " << bitArray[3] << "." << std::endl;
    std::cout << "Bit 14 = " << bitArray[14] << "." << std::endl << std::endl;
    std::cout << "Set bits 2 and 14" << std::endl << std::endl;

    bitArray.SetBit(2);
    bitArray.SetBit(14);

    std::cout << "Bit 2 = " << bitArray[2] << "." << std::endl;
    std::cout << "Bit 3 = " << bitArray[3] << "." << std::endl;
    std::cout << "Bit 14 = " << bitArray[14] << "." << std::endl << std::endl;
    std::cout << "Set all bits" << std::endl << std::endl;

    bitArray.SetAllBits();

    std::cout << "Bit 2 = " << bitArray[2] << "." << std::endl;
    std::cout << "Bit 3 = " << bitArray[3] << "." << std::endl;
    std::cout << "Bit 14 = " << bitArray[14] << "." << std::endl << std::endl;
    std::cout << "Clear all bits" << std::endl << std::endl;

    bitArray.ClearAllBits();

    std::cout << "Bit 2 = " << bitArray[2] << "." << std::endl;
    std::cout << "Bit 3 = " << bitArray[3] << "." << std::endl;
    std::cout << "Bit 14 = " << bitArray[14] << "." << std::endl << std::endl;

    return 1;
}