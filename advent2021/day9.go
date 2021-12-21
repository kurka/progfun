package main

import (
	"fmt"
	"io"
	"sort"
)

type CoordValue struct {
	row, column, value int
}

func readInput9() (heightMap [][]int) {
	var l string
	for {
		_, err := fmt.Scan(&l)
		if err == io.EOF {
			break
		}
		li := []int{}
		for _, c := range l {
			li = append(li, int(c)-48)
		}
		heightMap = append(heightMap, li)

	}
	return
}

// O(n) solution, as each cell is visited only once (and O(4n) comparisons are made)
func countBasin(heightMap *[][]int, valley CoordValue) (size int) {

	i, j := valley.row, valley.column
	// replace current position with 9 so it doesn't get counted twice
	(*heightMap)[i][j] = 9
	size = 1

	// explore until it reaches the border or a 9
	if i > 0 && (*heightMap)[i-1][j] != 9 {
		size += countBasin(heightMap, CoordValue{i - 1, j, (*heightMap)[i-1][j]})
	}
	if j > 0 && (*heightMap)[i][j-1] != 9 {
		size += countBasin(heightMap, CoordValue{i, j - 1, (*heightMap)[i][j-1]})
	}
	if i < len(*heightMap)-1 && (*heightMap)[i+1][j] != 9 {
		size += countBasin(heightMap, CoordValue{i + 1, j, (*heightMap)[i+1][j]})
	}
	if j < len((*heightMap)[i])-1 && (*heightMap)[i][j+1] != 9 {
		size += countBasin(heightMap, CoordValue{i, j + 1, (*heightMap)[i][j+1]})
	}
	return
}

func solve9(heightMap [][]int) (valleysSum int, basinsProduct int) {
	valleys := []CoordValue{}
	// O(n) solution
	for i := range heightMap {
		for j := range heightMap[i] {
			c := heightMap[i][j]
			if (i == 0 || heightMap[i-1][j] > c) &&
				(j == 0 || heightMap[i][j-1] > c) &&
				(i == len(heightMap)-1 || heightMap[i+1][j] > c) &&
				(j == len(heightMap[i])-1 || heightMap[i][j+1] > c) {

				valleysSum += c + 1
				valleys = append(valleys, CoordValue{i, j, c})
			}

		}
	}

	// compute basin sizes for part 2
	// O(n) recursive solution
	basinSizes := []int{}
	for _, valley := range valleys {
		basinSize := countBasin(&heightMap, valley)
		basinSizes = append(basinSizes, basinSize)
	}
	// sort all the sizes so we can retrieve the top3
	sort.Sort(sort.Reverse(sort.IntSlice(basinSizes)))
	basinsProduct = basinSizes[0] * basinSizes[1] * basinSizes[2]

	return
}

func main() {
	heightMap := readInput9()
	fmt.Println(solve9(heightMap))

}
