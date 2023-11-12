package main

import (
	"binarysearchtree/basic"
	"log"
)

func main() {
	log.Printf("Beginning")
	bst := &basic.BinarySearchTreeRecursive{}
	bst.Insert(2)
	bst.Insert(1)
	bst.Insert(3)
	bst.Insert(4)
	bst.Insert(1.5)
	bst.InOrderTraverse(bst.Root, func(node *basic.TreeNode) {
		log.Printf("Node: %f", node.Key)
	})
}
