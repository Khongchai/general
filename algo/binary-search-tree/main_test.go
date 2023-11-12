package main

import (
	"binarysearchtree/base"
	"binarysearchtree/basic"
	"log"
	"testing"
)

func TestAll(t *testing.T) {

	bstRecursive := &basic.BinarySearchTreeRecursive{}

	testInorderTraversal(TestSet, bstRecursive, t)
	testBinarySearch(TestSet, bstRecursive, t)
}

func testInorderTraversal(testSet []float64, bst base.BinarySearchTree, t *testing.T) {
	for i := range testSet {
		bst.Insert(testSet[i])
	}

	var previousFloat float64 = 0

	bst.InOrderTraverse(bst.GetRoot(), func(node *base.TreeNode) {
		log.Printf("Traversing, current node: %f", node.Key)
		key := node.Key
		if previousFloat > key {
			t.Errorf("Invalid in order traversal")
		}
		previousFloat = key
	})
}

func testBinarySearch(testSet []float64, bst base.BinarySearchTree, t *testing.T) {
	length := len(testSet)
	target := testSet[length/2]
	foundNode := bst.Search(bst.GetRoot(), target, func(node *base.TreeNode) {
		log.Printf("Searching, current node: %f", node.Key)
	})

	if foundNode == nil || foundNode.Key != target {
		t.Errorf("Target node not found")
	}
}
