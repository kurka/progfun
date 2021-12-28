package main

import (
	"fmt"
	"strings"
)

func readInput12() (caveMap map[string][]string) {
	caveMap = make(map[string][]string)

	var sourceTarget string //, source, target string
	fscan := func() (err error) { _, err = fmt.Scan(&sourceTarget); return }
	for err := fscan(); err == nil; err = fscan() {
		sourceTargetList := strings.Split(sourceTarget, "-")
		source, target := sourceTargetList[0], sourceTargetList[1]

		caveMap[source] = append(caveMap[source], target)
		caveMap[target] = append(caveMap[target], source)
	}

	return

}

func contains(ls []string, s string) (found bool) {
	for _, c := range ls {
		if c == s {
			found = true
			break
		}
	}
	return
}

func validCave(path []string, cave string, simple bool) (valid bool) {
	if cave == "start" {
		valid = false
		return
	}

	if cave == strings.ToUpper(cave) { // uppercase caves are always valid
		valid = true
		return
	}

	if simple { // case 1
		valid = true
		for _, c := range path {
			if c == cave {
				valid = false
				break
			}
		}
	} else { // case 2
		caveCounter := make(map[string]int)
		for _, c := range path {
			if c == strings.ToLower(c) {
				caveCounter[c] += 1
			}
		}

		// decide what to do based on how many times cave appeared in path
		switch caveCounter[cave] {
		case 0: // cave wasn't visited yet, so allow it
			valid = true
		case 1:
			anyTwo := false
			for _, count := range caveCounter {
				if count == 2 {
					anyTwo = true
					break
				}
			}
			if !anyTwo { // another cave was already visited twice
				valid = true
			} else {
				valid = false
			}
		case 2: // already visited twice, don't allow a third one
			valid = false

		}
	}
	return
}

func countPaths(caveMap *map[string][]string, pathTraveled []string, simple bool) (nPaths int) {

	lastCave := pathTraveled[len(pathTraveled)-1]
	// base case
	if lastCave == "end" {
		return 1
	}

	// for each cave connected to the last cave in path traveled
	for _, newCave := range (*caveMap)[lastCave] {
		if newCave == strings.ToUpper(newCave) || validCave(pathTraveled, newCave, simple) {
			nPaths += countPaths(caveMap, append(pathTraveled, newCave), simple)
		}
	}

	return
}

func solve12(caveMap map[string][]string) (nPathsA, nPathsB int) {
	nPathsA = countPaths(&caveMap, []string{"start"}, true)
	nPathsB = countPaths(&caveMap, []string{"start"}, false)
	return
}

func main() {
	caveMap := readInput12()
	fmt.Println(solve12(caveMap))

}
