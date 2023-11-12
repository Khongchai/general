package main

import (
	"binarysearchtree/base"
	"binarysearchtree/basic"
	"log"
	"math/rand"
	"testing"
)

func TestAll(t *testing.T) {
	rand.NewSource(4)
	testSet := make([]float64, 1000)

	bstRecursive := &basic.BinarySearchTreeRecursive{}

	testInorderTraversal(testSet, bstRecursive, t)
}

func testInorderTraversal(testSet []float64, bst base.BinarySearchTree, t *testing.T) {
	for i := range testSet {
		testSet[i] = rand.Float64() * 100
		bst.Insert(testSet[i])
	}

	var previousFloat float64 = 0

	bst.InOrderTraverse(bst.GetRoot(), func(node *base.TreeNode) {
		log.Printf("Node: %f", node.Key)
		key := node.Key
		if previousFloat > key {
			t.Errorf("Invalid in order traversal")
		}
		previousFloat = key
	})
}
