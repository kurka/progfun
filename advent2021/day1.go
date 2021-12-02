package main

import (
	"fmt"
	"io"
)

func readinput() []int {
	var n int
	var arr []int
	// read input and store it in array
	for {
		_, err := fmt.Scan(&n)
		arr = append(arr, n)
		if err == io.EOF {
			break
		}
	}
	return arr
}


func solve(arr []int, window_size int) int {

	if len(arr) < window_size+1 {
		panic("Array is too short to be processed!")
	}

	increases := 0
	for i := range arr[:len(arr)-window_size-1] {
		// we just need to compare the edges of the window, not the intersection
		if arr[i+window_size] - arr[i] > 0 {
			increases++
		}
	}
	return increases
}

func main() {
	arr := readinput()
	fmt.Println(solve(arr, 1))
	fmt.Println(solve(arr, 3))
}
