package main

import (
	"fmt"
)

type Node struct {
	data                int
	left, right, parent *Node
}

func readInput18() (trees []*Node) {
	var l string
	scanLine := func() (err error) { _, err = fmt.Scan(&l); return }
	for err := scanLine(); err == nil; err = scanLine() {
		tree, _ := parseTree([]rune(l), nil)
		trees = append(trees, tree)
	}

	return
}

func parseTree(expression []rune, parent *Node) (*Node, int) {

	// expression is always of format [llll,rrrrr]
	// where lllll can be either a number or a nested expression
	var left, right *Node
	var leftSize, rightSize int
	var self Node
	// [xxxxx,yyyyy]
	// parse left subtree
	switch expression[1] {
	case '[':
		left, leftSize = parseTree(expression[1:], &self)
	default: // number
		left = &Node{int(expression[1]) - 48, nil, nil, &self}
		leftSize = 1
	}

	switch expression[leftSize+2] {
	case '[':
		right, rightSize = parseTree(expression[leftSize+2:], &self)
	default:
		right = &Node{int(expression[leftSize+2]) - 48, nil, nil, &self}
		rightSize = 1
	}

	self = Node{-1, left, right, parent}
	size := leftSize + rightSize + 3 // add 3 for [,]
	return &self, size
}

func copyTree(root, parent *Node) *Node {
	if root == nil {
		return nil
	}

	var copiedRoot Node
	copiedLeft := copyTree(root.left, &copiedRoot)
	copiedRight := copyTree(root.right, &copiedRoot)
	copiedRoot = Node{root.data, copiedLeft, copiedRight, parent}
	return &copiedRoot
}

func printTree(root *Node) {
	if root.data != -1 {
		fmt.Printf("%d (%p->%p)", root.data, root, root.parent)
	} else {
		fmt.Print("[")
		printTree(root.left)
		fmt.Printf(" +(%p->%p) ", root, root.parent)
		printTree(root.right)
		fmt.Print("]")
	}
}
func printTreeS(root *Node) {
	if root.data != -1 {
		fmt.Printf("%d", root.data)
	} else {
		fmt.Print("[")
		printTreeS(root.left)
		fmt.Printf(" + ")
		printTreeS(root.right)
		fmt.Print("]")
	}
}

func findExplosion(tree *Node, height int) *Node {
	if tree == nil {
		return nil
	}
	// fmt.Printf("%p %v %d\n", tree, *tree, height)
	if height == 5 && tree.data == -1 {
		if tree.left.data == -1 || tree.right.data == -1 {
			panic("invalid structure")
		}
		return tree
	}

	result := findExplosion(tree.left, height+1)
	if result == nil {
		result = findExplosion(tree.right, height+1)
	}
	return result
}

func findSplit(tree *Node) *Node {
	if tree == nil {
		return nil
	}

	if tree.data >= 10 {
		return tree
	}

	result := findSplit(tree.left)
	if result == nil {
		result = findSplit(tree.right)
	}
	return result
}

func explode(explodedNode *Node) {

	// fmt.Printf("explode %p %v\n", explodedNode, *explodedNode)
	var left, right *Node = nil, nil
	// go up the tree looking for right and left
	curNode := explodedNode
	for curNode != nil {
		// fmt.Printf("%p %v %p %p\n", curNode, *curNode, left, right)
		parentNode := curNode.parent
		if left == nil && parentNode != nil && parentNode.left != curNode {
			left = parentNode.left
		}
		if right == nil && parentNode != nil && parentNode.right != curNode {
			right = parentNode.right
		}
		curNode = parentNode
	}

	if left != nil {
		// find first leaf at right of left
		for left.data == -1 {
			left = left.right
		}
		left.data += explodedNode.left.data
	}
	if right != nil {
		// find first leaf at left of right
		for right.data == -1 {
			right = right.left
		}
		right.data += explodedNode.right.data
	}

	// replace explodedNode by 0
	explodedNode.data = 0
	explodedNode.left = nil
	explodedNode.right = nil
}

func split(splitNode *Node) {
	splitValue := splitNode.data
	splitNode.data = -1
	splitNode.left = &Node{splitValue / 2, nil, nil, splitNode}
	splitNode.right = &Node{(splitValue + 1) / 2, nil, nil, splitNode}
}

func magnitude(tree *Node) int {
	if tree == nil {
		panic("shouldn't get this far!")
	}
	if tree.data != -1 {
		return tree.data
	}

	return 3*magnitude(tree.left) + 2*magnitude(tree.right)
}

func addTrees(treeA, treeB *Node) *Node {
	var newTree Node

	// add treeA and treeB
	treeA.parent = &newTree
	treeB.parent = &newTree

	newTree = Node{-1, treeA, treeB, nil}
	// printTreeS(&newTree)
	// fmt.Println()
	// printTree(&newTree)
	// fmt.Println()
	for {
		explosion_candidate := findExplosion(&newTree, 1)
		if explosion_candidate != nil {
			explode(explosion_candidate)
			continue
		}
		split_candidate := findSplit(&newTree)
		if split_candidate != nil {
			split(split_candidate)
		}
		if explosion_candidate == nil && split_candidate == nil {
			// exit loop if nothing happened
			break
		}
	}
	//
	// fmt.Println(">>>")
	// printTree(&newTree)
	// fmt.Println()
	// fmt.Println("<<<<<<")
	// printTreeS(&newTree)
	// fmt.Println()

	return &newTree
}

func solve18_part1(trees []*Node) {
	curTree := copyTree(trees[0], nil)
	for i := 1; i < len(trees); i++ {
		curTree = addTrees(curTree, copyTree(trees[i], nil))
	}
	// printTreeS(curTree)
	// fmt.Println()
	fmt.Println(magnitude(curTree))
}


func solve18_part2(trees []*Node) {
	maxMagnitude := 0
	for i := 0; i < len(trees); i++ {
		for j := 0; j < len(trees); j++ {
			if i == j {
				continue
			}
			treei := copyTree(trees[i], nil)
			treej := copyTree(trees[j], nil)

			mag := magnitude(addTrees(treei, treej))
			if mag > maxMagnitude {
				maxMagnitude = mag
			}
		}
	}
	fmt.Println(maxMagnitude)
}

func main() {
	trees := readInput18()
	solve18_part1(trees)
	solve18_part2(trees)
}
