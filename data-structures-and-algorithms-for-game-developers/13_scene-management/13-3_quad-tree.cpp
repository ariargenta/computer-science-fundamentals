listOfPolygons = new PolygonList(...);
quadTree = CreateQuadNode(listOfPolygons);

CreateQuadNode(listOfPolygons) {
    node = new QuadNode();

    aabb = CalculateBoundingBox(listOfPolygons);

    node.aabb = aabb;

    if(listOfPolygons.count <= MIN_NODE_POLY_COUNT) {
        node.polygons = listOfPolygons;

        return node;
    }

    frontLeftBox = CalculateSubBox(aabb, FRONT_LEFT);
    backLeftBox = CalculateSubBox(aabb, BACK_LEFT);
    frontRightBox = CalculateSubBox(aabb, FRONT_RIGHT);
    backRightBox = CalculateSubBox(aabb, BACK_RIGHT);

    frontLeftList = new PolygonList();
    backLeftList = new PolygonList();
    frontRightList = new PolygonList();
    backRightList = new PolygonList();

    for each(element i in listOfPolygons) {
        if(frontLeftBox.IsPolyIn(listOfPolygons[i]) == true) {
            frontLeftList.push(listOfPolygons[i]);
        }
        else if(backLeftBox.IsPolyIn(listOfPolygons[i]) == true) {
            backLeftList.push(listOfPolygons[i]);
        }
        else if(frontRightBox.IsPolyIn(listOfPolygons[i]) == true) {
            frontRightList.push(listOfPolygons[i]);
        }
        else if(backRightBox.IsPolyIn(listOfPolygons[i]) == true) {
            backRightList.push(listOfPolygons[i]);
        }
    }

    node.frontLeftNode = CreateQuadNode(frontLeftList);
    node.backLeftNode = CreateQuadNode(backLeftList);
    node.frontRightNode = CreateQuadNode(frontRightList);
    node.backRightNode = CreateQuadNode(backRightList);

    return node;
}

listOfPolygons = new PolygonList(...);
quadTree = CreateQuadNode(listOfPolygons);
frustrum = Frustrum(camera);

RenderQuadTree(frustrum, quadTree);

RenderQuadTree(frustrum, node) {
    if(node == NULL) {
        return;
    }

    if(frustrum.isVisible(node.aabb) == FALSE) {
        return;
    }

    if(node.polygons != NULL) {
        for each(element i in node.polygons) {

        }
    }
    else {
        RenderQuadTree(frustrum, node.frontLeftNode);
        RenderQuadTree(frustrum, node.backLeftNode);
        RenderQuadTree(frustrum, node.frontRightNode);
        RenderQuadTree(frustrum, node.backRightNode);
    }
}