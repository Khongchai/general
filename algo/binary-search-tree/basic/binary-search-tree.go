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

func (b *BinarySearchTreeRecursive) Delete(key float64) {
	b.DeleteNode(b.root, key)
}

func (b *BinarySearchTreeRecursive) DeleteNode(node *base.TreeNode, key float64) {

}

func (b *BinarySearchTreeRecursive) InsertNode(node *base.TreeNode, newNode *base.TreeNode) {
	if node.Key > newNode.Key {
		if node.Left == nil {
			node.Left = newNode
			newNode.Top = node
		} else {
			b.InsertNode(node.Left, newNode)
		}
		return
	}

	if node.Right == nil {
		node.Right = newNode
		newNode.Top = node
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

// Check if the key is within the interval of the current and its parent
func (b *BinarySearchTreeRecursive) processEnd(node *base.TreeNode, key float64) *base.IntervalSearchResult {
	// edge case: root node
	if node.Top == nil {
		return &base.IntervalSearchResult{
			Left:  nil,
			Right: node,
		}
	}

	var lowerbound *base.TreeNode
	var upperbound *base.TreeNode

	if node.Top.Key < node.Key {
		lowerbound = node.Top
		upperbound = node
	} else {
		lowerbound = node
		upperbound = node.Top
	}

	if lowerbound.Key <= key && key <= upperbound.Key {
		return &base.IntervalSearchResult{
			Left:  lowerbound,
			Right: upperbound,
		}
	}

	return nil
}

// this is wrong, btw. A better and simpler way is to just use the iterative version and keep track of lower and upper bound variables.
func (b *BinarySearchTreeRecursive) FindInterval(
	node *base.TreeNode,
	key float64,
	callback func(node *base.TreeNode)) *base.IntervalSearchResult {
	if node == nil {
		return nil
	}

	callback(node)

	if key == node.Key {
		return &base.IntervalSearchResult{
			Left:  node,
			Right: node,
		}
	}

	if key < node.Key {
		result := b.FindInterval(node.Left, key, callback)
		if result != nil {
			return result
		}

		endResult := b.processEnd(node, key)

		if endResult != nil {
			return endResult
		}

		return &base.IntervalSearchResult{
			Left:  nil,
			Right: node,
		}
	}

	result := b.FindInterval(node.Right, key, callback)
	if result != nil {
		return result
	}

	endResult := b.processEnd(node, key)

	if endResult != nil {
		return endResult
	}

	return &base.IntervalSearchResult{
		Left:  node,
		Right: nil,
	}
}
