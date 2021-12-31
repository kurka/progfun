package main

import (
	"container/heap"
	"fmt"
)

type Coord struct {
	x, y int
}

// An Item is something we manage in a priority queue.
type Item struct {
	coord    Coord // The value of the item; arbitrary.
	priority int   // The priority of the item in the queue.
	distance int   // the total distance travelled
	parent   Coord // Coord of parent
}

// A PriorityQueue implements heap.Interface and holds Items.
type PriorityQueue []*Item

func (pq PriorityQueue) Len() int { return len(pq) }

func (pq PriorityQueue) Less(i, j int) bool {
	// We want Pop to give us the lowest, so we use lower than here.
	return pq[i].priority < pq[j].priority
}

func (pq PriorityQueue) Swap(i, j int) {
	pq[i], pq[j] = pq[j], pq[i]
}

func (pq *PriorityQueue) Push(x interface{}) {
	item := x.(*Item)
	*pq = append(*pq, item)
}

func (pq *PriorityQueue) Pop() interface{} {
	old := *pq
	n := len(old)
	item := old[n-1]
	old[n-1] = nil // avoid memory leak
	*pq = old[0 : n-1]
	return item
}

func readInput15() (grid [][]int) {
	var l string
	scanLine := func() (err error) { _, err = fmt.Scan(&l); return }
	for err := scanLine(); err == nil; err = scanLine() {
		li := []int{}
		for _, c := range l {
			li = append(li, int(c)-48)
		}
		grid = append(grid, li)

	}
	return
}

func min(a, b int) int {
	if a < b {
		return a
	} else {
		return b
	}
}

func max(a, b int) int {
	if a > b {
		return a
	} else {
		return b
	}
}

func solve15(grid [][]int, mult int) {
	nrows := len(grid)
	ncols := len(grid[nrows-1])
	pq := make(PriorityQueue, 0, nrows*mult*ncols*mult)
	computed := make(map[Coord]*Item, nrows*mult*ncols*mult)

	item := &Item{coord: Coord{0, 0}, priority: 0, distance: 0, parent: Coord{-1, -1}}
	heap.Push(&pq, item)
	computed[item.coord] = item

	// A* algorithm
	for pq.Len() > 0 {
		if _, ok := computed[Coord{nrows*mult - 1, ncols*mult - 1}]; ok {
			break
		}

		item := heap.Pop(&pq).(*Item)

		for x := max(0, item.coord.x-1); x < min(nrows*mult, item.coord.x+2); x++ {
			for y := max(0, item.coord.y-1); y < min(ncols*mult, item.coord.y+2); y++ {
				if _, comp := computed[Coord{x, y}]; !comp && (x == item.coord.x || y == item.coord.y) {
					grid_value := ((grid[x%nrows][y%ncols] + x/nrows + y/ncols - 1) % 9) + 1
					distance := item.distance + grid_value
					priority := distance + (nrows*mult - 1 - x) + (ncols*mult - 1 - y)
					neighbor := &Item{coord: Coord{x, y}, priority: priority, distance: distance, parent: item.coord}
					heap.Push(&pq, neighbor)
					computed[item.coord] = item
				}
			}

		}
	}

	fmt.Println(computed[Coord{nrows*mult - 1, ncols*mult - 1}])
}

func main() {
	grid := readInput15()
	solve15(grid, 1)
	solve15(grid, 5)
}
