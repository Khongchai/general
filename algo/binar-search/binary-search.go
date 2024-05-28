package main

import (
	"fmt"
)

func binarySearch(arr *[]int, target int) bool {
	low := 0
	high := len(*arr) - 1

	for low <= high {
		mid := (low + high) / 2
		if (*arr)[mid] == target {
			return true
		} else if (*arr)[mid] > target {
			high = mid - 1
			continue
		} else {
			low = mid + 1
			continue
		}
	}

	return false
}

func main() {
	arr := []int{
		1, 5, 6, 8, 10, 200, 500,
	}

	result := binarySearch(&arr, 201)

	fmt.Print(result)

}
