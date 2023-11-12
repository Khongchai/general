package basic

type TreeNode struct {
	Key   float64
	Left  *TreeNode
	Right *TreeNode
}

type BinarySearchTreeRecursive struct {
	Root *TreeNode
}

func (b *BinarySearchTreeRecursive) Insert(key float64) {
	newNode := &TreeNode{Key: key}
	if b.Root == nil {
		b.Root = newNode
		return
	}

	b.InsertNode(b.Root, newNode)
}

func (b *BinarySearchTreeRecursive) InsertNode(node *TreeNode, newNode *TreeNode) {
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

func (b *BinarySearchTreeRecursive) InOrderTraverse(node *TreeNode, callback func(node *TreeNode)) {
	if node != nil {
		b.InOrderTraverse(node.Left, callback)
		callback(node)
		b.InOrderTraverse(node.Right, callback)
	}
}
