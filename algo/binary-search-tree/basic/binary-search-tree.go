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
			newNode.Top = node.Left
		} else {
			b.InsertNode(node.Left, newNode)
		}
		return
	}

	if node.Right == nil {
		node.Right = newNode
		newNode.Top = node.Right
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

func (b *BinarySearchTreeRecursive) Search(node *base.TreeNode, key float64, callback func(node *base.TreeNode)) *base.TreeNode {
	callback(node)
	if node == nil || key == node.Key {
		return node
	}
	if key < node.Key {
		return b.Search(node.Left, key, callback)
	}

	return b.Search(node.Right, key, callback)
}

func (b *BinarySearchTreeRecursive) GetRoot() *base.TreeNode {
	return b.root
}

func (b *BinarySearchTreeRecursive) FindInterval(
	node *base.TreeNode,
	key float64,
	callback func(node *base.TreeNode)) *base.IntervalSearchResult {
	callback(node)
	return nil

	// if key == current.Key {
	// 	return &base.IntervalSearchResult{Left: previous}
	// }

	// if key > current.Key {
	// 	if current.Right == nil {
	// 		return &base.IntervalSearchResult{Left: current, Right: previous}
	// 	}
	// 	return b.FindInterval(current, current.Right, key, callback)
	// }

}
