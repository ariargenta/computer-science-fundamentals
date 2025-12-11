for each(element i in array of data) {
    if(element i does not have frequency) {
        frequency = GetFrequency(i, data);
        node = CreateNode(frequencyList);

        AddNode(node, priorityQueue);
    }
}

while(priorityQueue.count > 1) {
    node1 = RemoveTopNode(priorityQueue);
    node2 = RemoveTopNode(priorityQueue);

    newSubNode = CreateNode(node1, node2);

    AddNode(newSubNode, priorityQueue);
}

huffmanTree = RemoveTopNode(priorityQueue);
huffmanTable = CreateHuffmanTable(huffmanTree);

for each(element i in array of data) {
    compressedData[i] = huffmanTable[data[i]];
}

index = 0;

for each(bit i in array of compressedData) {
    huffmanTree.MoveToNextTreeBranch(compressedData[i]);

    if(huffmanTree.AtLeafNode()) {
        data[index++] = huffmanTree.GetCurrentNodeValue();
    }
}