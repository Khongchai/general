package main

type Tree struct {
	Left  *Tree
	Value int
	Right *Tree
}

// Walk walks the tree t sending all values
// from the tree to the channel ch.
func Walk(t *Tree, ch chan int) {
	if t.Left != nil {
		Walk(t.Left, ch)
	}
	if t.Right != nil {
		Walk(t.Right, ch)
	}
	ch <- t.Value
}

func Same(t1, t2 *tree.Tree) bool {
	ch1, ch2 := make(chan int), make(chan int)

	go func() {
		Walk(t1, ch1)
		close(ch1)
	}()
	go func() {
		Walk(t2, ch2)
		close(ch2)
	}()

	for {
		v1, ok1 := <-ch1
		v2, ok2 := <-ch2

		if ok1 != ok2 { // one channel closed before the other
			return false
		}
		if !ok1 { // both closed
			return true
		}
		if v1 != v2 { // mismatch
			return false
		}
	}
}
func main() {
	ch := make(chan int, 10)
	go Walk(New(1), ch)
	for i := 0; i < 10; i++ {
		val := <-ch
		print(val)
	}

}
