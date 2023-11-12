package main

import (
	"binarysearchtree/base"
	"binarysearchtree/basic"
	"log"
	"testing"
)

func TestAll(t *testing.T) {

	testInorderTraversal(TestSet, &basic.BinarySearchTreeRecursive{}, t)

	testBinarySearch(TestSet, &basic.BinarySearchTreeRecursive{}, t)

	bstInterval := &basic.BinarySearchTreeRecursive{}
	bstInterval.Insert(1)
	bstInterval.Insert(2)
	bstInterval.Insert(3)
	bstInterval.Insert(5)
	bstInterval.Insert(7)
	bstInterval.Insert(9)
	testFindInterval(
		2.5,
		&base.TreeNode{Key: 2},
		&base.TreeNode{Key: 3},
		bstInterval,
		t,
	)
	testFindInterval(
		6,
		&base.TreeNode{Key: 5},
		&base.TreeNode{Key: 7},
		bstInterval,
		t,
	)
	testFindInterval(
		999999999,
		&base.TreeNode{Key: 9},
		nil,
		bstInterval,
		t,
	)
	testFindInterval(
		-1,
		nil,
		&base.TreeNode{Key: 1},
		bstInterval,
		t,
	)
	testFindInterval(
		5,
		&base.TreeNode{Key: 5},
		&base.TreeNode{Key: 5},
		bstInterval,
		t,
	)
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
	for i := range testSet {
		bst.Insert(testSet[i])
	}
	length := len(testSet)
	target := testSet[length/2]
	foundNode := bst.Search(bst.GetRoot(), target, func(node *base.TreeNode) {
		log.Printf("Searching, current node: %f", node.Key)
	})

	if foundNode == nil || foundNode.Key != target {
		t.Errorf("Target node not found")
		t.FailNow()
	}
}

func testFindInterval(
	input float64,
	outputLower *base.TreeNode,
	outputUpper *base.TreeNode,
	bst base.BinarySearchTree, t *testing.T) {
	result := bst.FindInterval(bst.GetRoot(), input, func(node *base.TreeNode) {
		log.Printf("Finding interval, current node; %f", node.Key)
	})

	if result == nil {
		if outputLower == nil && outputUpper == nil {
			return
		}
		t.Errorf("Error, result is nil but the expected output is not")
		t.FailNow()
	}

	if result.Left != nil && result.Right != nil {
		if result.Left.Key != outputLower.Key || result.Right.Key != outputUpper.Key {
			t.Errorf("Error: result and output node do not share the same value(s)")
			t.FailNow()
		}
		return
	}

	if result.Left == nil && outputLower != nil {
		t.Errorf("Error: result is nil but the expected output is not")
		t.FailNow()
		return
	}

	if result.Right == nil && outputUpper != nil {
		t.Errorf("Error: result is nil but the expected output is not")
		t.FailNow()
		return
	}

}
