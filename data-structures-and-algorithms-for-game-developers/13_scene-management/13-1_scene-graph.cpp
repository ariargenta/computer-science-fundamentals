#include <iostream>
#include <cmath>
#include <vector>

enum PLANE_STATUS {
    PLANE_FRONT = 0
    , PLANE_BACK = 1
    , PLANE_ON_PLANE = 2
};

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
            return Vector3D(
                x + v2.x
                , y + v2.y
                , z + v2.z
            );
        }

        Vector3D operator-(const Vector3D& v2) {
            return Vector3D(
                x - v2.x
                , y - v2.y
                , z - v2.z
            );
        }

        Vector3D operator/(const Vector3D& v2) {
            return Vector3D(
                x / v2.x
                , y / v2.y
                , z / v2.z
            );
        }

        Vector3D operator*(const Vector3D& v2) {
            return Vector3D(
                x * v2.x
                , y * v2.y
                , z * v2.z
            );
        }

        Vector3D operator+(float f) {
            return Vector3D(
                x + f
                , y + f
                , z + f
            );
        }

        Vector3D operator-(float f) {
            return Vector3D(
                x - f
                , y - f
                , z - f
            );
        }

        Vector3D operator/(float f) {
            return Vector3D(
                x / f
                , y / f
                , z / f
            );
        }

        Vector3D operator*(float f) {
            return Vector3D(
                x * f
                , y * f
                , z * f
            );
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
            return Vector3D(
                y * v.z - z * v.y
                , z * v.x - x * v.z
                , x * v.y - y * v.x
            );
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

            if(normal.z >= 0.0f) {
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

class Frustrum {
    public:
        Frustrum() {}

        void CalculateFrustrum(
            float angle
            , float ratio
            , float near
            , float far
            , Vector3D& camPos
            , Vector3D& lookAt
            , Vector3D& up
        ) {
            Vector3D xVec;
            Vector3D yVec;
            Vector3D zVec;
            Vector3D vecN;
            Vector3D vecF;
            Vector3D nearTopLeft;
            Vector3D nearTopRight;
            Vector3D nearBottomLeft;
            Vector3D nearBottomRight;
            Vector3D farTopLeft;
            Vector3D farTopRight;
            Vector3D farBottomLeft;
            Vector3D farBottomRight;

            float radians = (float)tan((DEG_TO_RAD(angle)) * 0.5);
            float nearH = near * radians;
            float nearW = nearH * ratio;
            float farH = far * radians;
            float farW = farH * ratio;

            zVec = camPos - lookAt;

            zVec.Normalize();

            xVec = up.CrossProduct(zVec);

            xVec.Normalize();

            yVec = zVec.CrossProduct(xVec);

            vecN = camPos - zVec * near;
            vecF = camPos - zVec * far;

            nearTopLeft = vecN + yVec * nearH - xVec * nearW;
            nearTopRight = vecN + yVec * nearH + xVec * nearW;
            nearBottomLeft = vecN - yVec * nearH - xVec * nearW;
            nearBottomRight = vecN - yVec * nearH + xVec * nearW;
            farTopLeft = vecF + yVec * farH - xVec * farW;
            farTopRight = vecF + yVec * farH + xVec * farW;
            farBottomLeft = vecF - yVec * farH - xVec * farW;
            farBottomRight = vecF - yVec * farH + xVec * farW;

            m_frustrum.clear();

            Plane plane;

            plane.CreatePlaneFromTri(nearTopRight, nearTopLeft, farTopLeft);
            AddPlane(plane);
            plane.CreatePlaneFromTri(nearBottomLeft, nearBottomRight, farBottomRight);
            AddPlane(plane);
            plane.CreatePlaneFromTri(nearTopLeft, nearBottomLeft, farBottomLeft);
            AddPlane(plane);
            plane.CreatePlaneFromTri(nearBottomRight, nearTopRight, farBottomRight);
            AddPlane(plane);
            plane.CreatePlaneFromTri(nearTopLeft, nearTopRight, nearBottomRight);
            AddPlane(plane);
            plane.CreatePlaneFromTri(farTopRight, farTopLeft, farBottomLeft);
            AddPlane(plane);
        }

        void AddPlane(Plane& pl) {
            m_frustrum.push_back(pl);
        }

        bool isPointVisible(float x, float y, float z) {
            for(int i = 0; i < (int)m_frustrum.size(); ++i) {
                if(m_frustrum[i].GetDistance(x, y, z) < 0) {
                    return false;
                }
            }

            return true;
        }

        bool isSphereVisible(float x, float y, float z, float radius) {
            float distance = 0;

            for(int i = 0; i < (int)m_frustrum.size(); ++i) {
                distance = m_frustrum[i].GetDistance(x, y, z);

                if(distance < -radius) {
                    return false;
                }
            }

            return true;
        }

        bool isBoxVisible(Vector3D min, Vector3D max) {
            if(isPointVisible(min.x, min.y, min.z)) {
                return true;
            }
            
            if(isPointVisible(max.x, min.y, min.z)) {
                return true;
            }

            if(isPointVisible(min.x, max.y, min.z)) {
                return true;
            }

            if(isPointVisible(max.x, max.y, min.z)) {
                return true;
            }

            if(isPointVisible(min.x, min.y, max.z)) {
                return true;
            }

            if(isPointVisible(max.x, min.y, max.z)) {
                return true;
            }

            if(isPointVisible(min.x, max.y, max.z)) {
                return true;
            }

            if(isPointVisible(max.x, max.y, max.z)) {
                return true;
            }

            return false;
        }

        int GetTotalPlanes() {
            return (int)m_frustrum.size();
        }

    private:
        std::vector<Plane> m_frustrum;
};

class Camera {
    public:
        Camera(Vector3D& pos, Vector3D& lookAt) {
            m_pos = pos;
            m_lookAt = lookAt;
        }

        void MoveCamera(Vector3D& direction, float speed) {
            m_pos += direction * speed;
            m_lookAt += direction * speed;
        }

        void RotateView(float angle);

        void SetPosition(Vector3D& pos) {
            m_pos = pos;
        }

        void SetLookDirection(Vector3D& at) {
            m_lookAt = at;
        }

        Vector3D GetPosition() {
            return m_pos;
        }

        Vector3D GetLookDirection() {
            return m_lookAt;
        }

    private:
        Vector3D m_pos;
        Vector3D m_lookAt;
};

class Node {
    public:
        Node() {
            m_next = NULL;
            m_prev = NULL;
            m_child = NULL;
        }

        virtual ~Node() {
            m_prev = NULL;

            if(m_child != NULL) {
                delete m_child;

                m_child = NULL;
            }

            if(m_next != NULL) {
                delete m_next;

                m_next = NULL;
            }
        }

        void AddChild(Node* node) {
            if(m_child == NULL) {
                m_child = node;
            }
            else {
                m_child -> AddSibling(node);
            }
        }

        void AddSibling(Node* node) {
            Node* ptr = m_next;

            if(m_next == NULL) {
                m_next = node;
                node -> m_prev = this;
            }
            else {
                while(ptr -> m_next != NULL) {
                    ptr = ptr -> m_next;
                }

                ptr -> m_next = node;
                node -> m_prev = ptr;
            }
        }

        virtual void Process() {
            if(m_child != NULL) {
                m_child -> Process();
            }

            if(m_next != NULL) {
                m_next -> Process();
            }
        }

    protected:
        Node* m_next;
        Node* m_prev;
        Node* m_child;
};

class TransformationNode : public Node {
    public:
        TransformationNode(Vector3D& pos) : m_pos(pos) {}
        ~TransformationNode() {}

        void Process() {
            glPushMatrix();
            glTranslatef(m_pos.x, m_pos.y, m_pos.z);

            if(m_child != NULL) {
                m_child -> Process();
            }

            glPopMatrix();

            if(m_next != NULL) {
                m_next -> Process();
            }
        }

    protected:
        Vector3D m_pos;
};

class SphereNode : public Node {
    public:
        SphereNode(double rd, int slices, int stacks, float r, float g, float b) : m_radius(rd), m_slices(slices), m_stacks(stacks), m_red(r), m_green(g), m_blue(b) {}

        ~SphereNode() {}

        void Process() {
            glColor3f(m_red, m_green, m_blue);
            glutSolidSphere(m_radius, m_slices, m_stacks);

            if(m_child != NULL) {
                m_child -> Process();
            }

            if(m_next != NULL) {
                m_next -> Process();
            }
        }

    protected:
        double m_radius;
        int m_slices;
        int m_stacks;
        float m_red;
        float m_green;
        float m_blue;
};

class SceneGraph {
    public:
        SceneGraph() {
            m_root = NULL;
        }

        ~SceneGraph() {
            Release();
        }

        void Release() {
            if(m_root != NULL) {
                delete m_root;

                m_root = NULL;
            }
        }

        void AddNode(Node* node) {
            if(m_root == NULL) {
                m_root = new Node;
            }

            m_root -> AddChild(node);
        }

        void Process() {
            if(m_root != NULL) {
                m_root -> Process();
            }
        }

    private:
        Node* m_root;
};