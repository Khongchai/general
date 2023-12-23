package base

type TreeNode struct {
	Key   float64
	Left  *TreeNode
	Right *TreeNode
	Top   *TreeNode
}

// Represents the search result of an interval
type IntervalSearchResult struct {
	// if `Left` is null, the value given is less than the valid range
	// this may be equal to the target key, but never more than
	Left *TreeNode
	// if `Right` is null, the value given is greater than the valid range
	// this may be equal to the target key, but never less than
	Right *TreeNode
}

type BinarySearchTree interface {
	GetRoot() *TreeNode
	Insert(key float64)
	Delete(key float64)
	DeleteNode(node *TreeNode, key float64)
	InsertNode(node *TreeNode, newNode *TreeNode)
	InOrderTraverse(node *TreeNode, callback func(node *TreeNode))
	Search(node *TreeNode, key float64, callback func(node *TreeNode)) *TreeNode
	// For intervalic search, we can modify search such that instead of returning the found node or null,
	// we return a result, which contains the closest two nodes within the specified interval
	//
	// If the key is outside of [min, max] range, nil is returned
	FindInterval(node *TreeNode, key float64, callback func(node *TreeNode)) *IntervalSearchResult
}
