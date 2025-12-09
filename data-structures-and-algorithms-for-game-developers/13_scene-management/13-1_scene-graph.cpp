#include <iostream>
#include <cmath>
#include <gl/glut.h>

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

class Plane {
    public:
        Plane() : a(0), b(0), c(0), d(0) {}
        Plane(float A, float B, float C, float D) : a(A), b(B), c(C), d(D) {}

        void Create(Vector3D& t1, Vector3D& t2, Vector3D& t3) {
            Vector3D e1;
            Vector3D e2;
            Vector3D n;

            e1 = t2 - t1;
            e2 = t3 - t1;
            n = e1.CrossProduct(e2);

            n.Normalize();

            a = n.x;
            b = n.y;
            c = n.z;
            d = -(a * t1.x + b * t1.y + c * t1.z);
        }

        bool Intersect(const Vector3D& bbMin, const Vector3D& bbMax) {
            Vector3D min;
            Vector3D max;
            Vector3D normal(a, b, c);

            if(normal.x >= 0.0f) {
                min.x = bbMin.x;
                max.x = bbMax.x;
            }
            else {
                min.x = bbMax.x;
                max.x = bbMin.x;
            }

            if(normal.y >= 0.0f) {
                min.y = bbMin.y;
                max.y = bbMax.y;
            }
            else {
                min.y = bbMax.y;
                max.y = bbMin.y;
            }

            if(normal.Z >= 0.0f) {
                min.z = bbMin.z;
                max.z = bbMax.z;
            }
            else {
                min.z = bbMax.z;
                max.z = bbMin.z;
            }

            if((normal.Dot3(min) + d) > 0.0f) {
                return false;
            }

            if((normal.Dot3(max) + d) >= 0.0f) {
                return true;
            }
            
            return false;
        }

        bool Intersect(const Vector3D& position, float radius) {
            float dp = fabs(GetDistance(position));

            if(dp <= radius) {
                return true;
            }

            return false;
        }

        PLANE_STATUS ClassifyPoint(float x, float y, float z, float* dist) {
            float distance = a * x + b * y + c * z + d;

            if(dist != 0) {
                *dist = distance;
            }

            if(distance > 0.001) {
                return PLANE_FRONT;
            }

            if(distance < -0.001) {
                return PLANE_BACK;
            }

            return PLANE_ON_PLANE;
        }

        float Plane::GetDistance(float x, float y, float z) {
            return a * x + b * y + c * z + d;
        }

        float a;
        float b;
        float c;
        float d;
};

class BoundingBox {
    public:
        BoundingBox() {}

        BoundingBox(const BoundingBox& aabb) {
            m_min = aabb.m_min;
            m_max = aabb.m_max;
            m_center = aabb.m_center;
        }

        void Calculate(Vector3D* v, int numPoints) {
            if(v == NULL) {
                return;
            }

            for(int i = 0; i < numPoints; ++i) {
                if(v[i].x < m_min.x) {
                    m_min.x = v[i].x;
                }

                if(v[i].x > m_max.x) {
                    m_max.x = v[i].x;
                }

                if(v[i].y < m_min.y) {
                    m_min.y = v[i].y;
                }

                if(v[i].y > m_max.y) {
                    m_max.y = v[i].y;
                }

                if(v[i].z < m_min.z) {
                    m_min.z = v[i].z;
                }

                if(v[i].z > m_max.z) {
                    m_max.z = v[i].z;
                }
            }

            m_center.x = (m_min.x + m_max.x) * 0.5f;
            m_center.y = (m_min.y + m_max.y) * 0.5f;
            m_center.z = (m_min.z + m_max.z) * 0.5f;
        }

        Vector3D m_min;
        Vector3D m_max;
        Vector3D m_center;
};