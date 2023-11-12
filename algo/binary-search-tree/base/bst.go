package base

type TreeNode struct {
	Key   float64
	Left  *TreeNode
	Right *TreeNode
}

type BinarySearchTree interface {
	GetRoot() *TreeNode
	Insert(key float64)
	InsertNode(node *TreeNode, newNode *TreeNode)
	InOrderTraverse(node *TreeNode, callback func(node *TreeNode))
}
