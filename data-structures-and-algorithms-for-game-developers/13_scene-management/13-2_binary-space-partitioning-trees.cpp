listOfPolygons = new PolygonList(...);
bspTree = CreateBSPNode(listOfPolygons);

CreateBSPNode(listOfPolygons) {
    node = new BSPNode();

    if(listOfPolygon.count <= MIN_NODE_POLY_COUNT) {
        node.polygonList = listOfPolygons;

        return node;
    }

    splitPlane = GetBestSplitter(listOfPolygons);

    for each(element i in listOfPolygons) {
        if(splitPlane.Classify(listOfPolygons[i]) == FRONT) {
            subPolyList1.push(listOfPolygons[i]);
        }
        else if(splitPlane.CLassify(listOfPolygons[i]) == BACK) {
            subPolyList2.push(listOfPolygons[i]);
        }
        else {
            splitPlane.ClipPolygon(listOfPolygons[i], &p1, &p2);

            subPolyList1.push(p1);
            subPolyList2.push(p2);
        }
    }

    node.frontNode = CreateBSPNode(subPolyList1);
    node.backNode = CreateBSPNode(subPolyList2);

    return node;
}

GetBestSplitter(listOfPolygons) {
    plane = Plane();
    minPlane = Plane();
    minCount = 9999999999;
    currentCount = 0;

    for each(element i in listOfPolygons) {
        plane.Create(listOfPolygons[i]);

        for each(element j in listOfPolygons) {
            frontCount = 0;
            backCount = 0;

            if(i != j) {
                if(plane.Classify(listOfPolygons[j]) == FRONT) {
                    frontCount++;
                }
                else if(plane.Classify(listOfPolygons[j]) == BACK) {
                    backCount++;
                }
                else {
                    frontCount++;
                    backCount++;
                }
            }
        }

        currentCount = abs(frontCount - backCount);

        if(currentCount < minCount) {
            minPlane = plane;
            minCount = currentCount;
        }
    }

    return minPlane;
}