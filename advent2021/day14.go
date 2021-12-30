package main

import (
	"fmt"
	"math"
)

type Pair struct {
	fst, snd rune
}

func readInput14() (start map[Pair]int, rules map[Pair]rune, firstRune rune, lastRune rune) {
	var startString string
	var pair Pair
	var consequent rune
	start = make(map[Pair]int)
	rules = make(map[Pair]rune)

	fmt.Scanf("%s\n\n", &startString)
	// transform string into count of pairs of runes
	startRunesList := []rune(startString)
	for i := 0; i < len(startRunesList)-1; i++ {
		start[Pair{startRunesList[i], startRunesList[i+1]}] += 1
	}
	firstRune = startRunesList[0]
	lastRune = startRunesList[len(startRunesList)-1]

	scanRules := func() (err error) {
		_, err = fmt.Scanf("%c%c -> %c\n", &pair.fst, &pair.snd, &consequent)
		return
	}
	for err := scanRules(); err == nil; err = scanRules() {
		rules[pair] = consequent
	}

	return
}

func countDiffOfExtremes(sequence *map[Pair]int, firstRune rune, lastRune rune) int {
	letterCounter := make(map[rune]int)
	for pair, count := range *sequence {
		letterCounter[pair.fst] += count
		letterCounter[pair.snd] += count
	}
	// add extra count to first and last runes
	letterCounter[firstRune]++
	letterCounter[lastRune]++

	maxCount, minCount := 0, math.MaxInt
	for _, v := range letterCounter {
		if v > maxCount {
			maxCount = v
		}
		if v < minCount {
			minCount = v
		}
	}

	return (maxCount/2) - (minCount/2)

}

func solve14(
	sequence map[Pair]int,
	rules map[Pair]rune,
	firstRune rune,
	lastRune rune) {

	for step := 1; step <= 40; step++ {
		next := make(map[Pair]int)
		for pair, count := range sequence {
			// transform a pair in sequence into two pairs in next
			// ex: if pair == AC and rule AC -> B, add AB and BC to next
			newRune := rules[pair]
			next[Pair{pair.fst, newRune}] += count
			next[Pair{newRune, pair.snd}] += count
		}
		sequence = next

		if step == 10 || step == 40 {
			fmt.Println(countDiffOfExtremes(&sequence, firstRune, lastRune))
		}
	}

}

func main() {
	start, rules, firstRune, lastRune := readInput14()
	solve14(start, rules, firstRune, lastRune)
}
