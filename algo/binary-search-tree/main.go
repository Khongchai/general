package main

import (
	"binarysearchtree/base"
	"binarysearchtree/basic"
	"log"
)

func main() {
	log.Printf("Beginning")
	var bst base.BinarySearchTree = &basic.BinarySearchTreeRecursive{}
	bst.Insert(2)
	bst.Insert(1)
	bst.Insert(3)
	bst.Insert(4)
	bst.Insert(1.5)
	bst.InOrderTraverse(bst.GetRoot(), func(node *base.TreeNode) {
		log.Printf("Node: %f", node.Key)
	})
}
