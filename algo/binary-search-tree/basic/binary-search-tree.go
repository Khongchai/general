package basic

import "binarysearchtree/base"

type BinarySearchTreeRecursive struct {
	root *base.TreeNode
	base.BinarySearchTree
}

func (b *BinarySearchTreeRecursive) Insert(key float64) {
	newNode := &base.TreeNode{Key: key}
	if b.root == nil {
		b.root = newNode
		return
	}

	b.InsertNode(b.root, newNode)
}

func (b *BinarySearchTreeRecursive) InsertNode(node *base.TreeNode, newNode *base.TreeNode) {
	if node.Key > newNode.Key {
		if node.Left == nil {
			node.Left = newNode
		} else {
			b.InsertNode(node.Left, newNode)
		}
		return
	}

	if node.Right == nil {
		node.Right = newNode
	} else {
		b.InsertNode(node.Right, newNode)
	}
}

func (b *BinarySearchTreeRecursive) InOrderTraverse(node *base.TreeNode, callback func(node *base.TreeNode)) {
	if node != nil {
		b.InOrderTraverse(node.Left, callback)
		callback(node)
		b.InOrderTraverse(node.Right, callback)
	}
}

func (b *BinarySearchTreeRecursive) GetRoot() *base.TreeNode {
	return b.root
}
