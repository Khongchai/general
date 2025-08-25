package main

import (
	"fmt"
	"sync"
)

// fork add to array and join later
func main() {
	array := []int{}

	var mu sync.Mutex
	var wg sync.WaitGroup

	for i := 0; i < 10; i++ {
		wg.Add(1)
		go func(val int) {
			defer wg.Done()
			defer mu.Unlock()
			mu.Lock()
			array = append(array, val)
		}(i)
	}

	wg.Wait()

	fmt.Print(array)
}
