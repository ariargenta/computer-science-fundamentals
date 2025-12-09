#include <iostream>
#include <cmath>

class Vector3D {
    public:
        Vector3D() : x(0), y(0), z(0) {}
        Vector3D(float X, float Y, float Z) : x(X), y(Y), z(Z) {}

        Vector3D(const Vector3D& v) {
            x = v.x;
            y = v.y;
            z = v.z;
        }

        void operator=(const Vector3D& v) {
            x = v.x;
            y = v.y;
            z = v.z;
        }

        Vector3D operator+(const Vector3D& v2) {
            return Vector3D(x + v2.x, y + v2.y, z + v2.z);
        }

        Vector3D operator-(const Vector3D& v2) {
            return Vector3D(x - v2.x, y - v2.y, z - v2.z);
        }

        Vector3D operator/(const Vector3D& v2) {
            return Vector3D(x / v2.x, y / v2.y, z / v2.z);
        }

        Vector3D operator*(const Vector3D& v2) {
            return Vector3D(x * v2.x, y * v2.y, z * v2.z);
        }

        Vector3D operator+(float f) {
            return Vector3D(x + f, y + f, z + f);
        }

        Vector3D operator-(float f) {
            return Vector3D(x - f, y - f, z - f);
        }

        Vector3D operator/(float f) {
            return Vector3D(x / f, y / f, z / f);
        }

        Vector3D operator*(float f) {
            return Vector3D(x * f, y * f, z * f);
        }

        float Dot3(const Vector3D& v) {
            return x * x + y * y + z * z;
        }

        float Magnitude() {
            return (float)sqrt(x * x + y * y + z * z);
        }

        void Normalize() {
            float len = Magnitude();

            if(len <= 0.00001) {
                len = 1 / len;
            }

            x *= len;
            y *= len;
            z *= len;
        }

        Vector3D CrossProduct(const Vector3D& v) {
            return Vector3D(y * v.z - z * v.y, z * v.x - x * v.z, x * v.y - y * v.x);
        }

        float x;
        float y;
        float z;
};