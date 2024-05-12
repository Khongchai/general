package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func bubbleSort(arr *[]int) {
	for i := 0; i < len(*arr)-1; i++ {
		for j := 0; j < len(*arr)-1; j++ {
			if (*arr)[j] > (*arr)[j+1] {
				temp := (*arr)[j]
				(*arr)[j] = (*arr)[j+1]
				(*arr)[j+1] = temp
			}
		}
	}
}

func main() {
	fmt.Printf("Need comman-separated numbers")
	scanner := bufio.NewScanner(os.Stdin)

	for {
		fmt.Printf(">>")
		scanned := scanner.Scan()
		if !scanned {
			return
		}

		line := scanner.Text()

		var parsedArray []int

		for _, s := range strings.Split(line, ",") {
			num, err := strconv.Atoi(s)
			if err != nil {
				panic("Bruh, invalid num")
			}
			parsedArray = append(parsedArray, num)
		}

		bubbleSort(&parsedArray)

		fmt.Println(parsedArray)
	}

}
