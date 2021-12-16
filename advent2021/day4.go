package main

import (
	"fmt"
	"io"
	"strconv"
	"strings"
)

const CardSize = 5

type Coordinate struct {
	row, column int
}

// we just need to store unmarked numbers and the count of marked ones per row and column
type CardData struct {
	unmarked           map[int]Coordinate // store numbers not drawn and their coordinates
	rowsColumnsCounter [CardSize * 2]int // counts how many hits each row and column had
}

func readinput4() (draws []int, cards []CardData) {
	var draws_string string
	// first read the list of draws
	fmt.Scan(&draws_string)

	for _, d := range strings.Split(draws_string, ",") {
		draw, _ := strconv.Atoi(d)
		draws = append(draws, draw)
	}

	// then read the cards
ReadCardLoop:
	for {
		unmarked := make(map[int]Coordinate, CardSize*CardSize)
		var card_value int
		for i := 0; i < CardSize; i++ {
			for j := 0; j < CardSize; j++ {
				_, err := fmt.Scan(&card_value)
				if err == io.EOF {
					break ReadCardLoop
				}
				unmarked[card_value] = Coordinate{i, j}
			}
		}
		cardData := CardData{unmarked, [CardSize * 2]int{}}
		cards = append(cards, cardData)
	}

	return
}


// O(mn) solution, where m is the number of numbers drawn, and n the number of cards
// returns both solutions in a same function run
func solve4(draws []int, cards []CardData, partB bool) (firstWinner, lastWinner int) {

	firstWinnerIsKnown := false

	for _, draw := range draws {
		var next_round_cards = []CardData{} // store the cards that will be used in next round
		for card_i := range cards {
			card := &cards[card_i]
			bingo := false
			// fill cards and check for bingo
			if coords, isUnmarked := card.unmarked[draw]; isUnmarked {
				delete(card.unmarked, draw)
				// increment row and column counters
				card.rowsColumnsCounter[coords.row] += 1
				card.rowsColumnsCounter[CardSize+coords.column] += 1

				if card.rowsColumnsCounter[coords.row] == CardSize || card.rowsColumnsCounter[CardSize+coords.column] == CardSize {
					// bingo!
					bingo = true
				}
			}

			// deal with winning (and losing) cards
			if bingo {
				if !firstWinnerIsKnown || len(cards) == 1 {
					// compute final sum and return
					unmarked_sum := 0
					for unmarked := range card.unmarked {
						unmarked_sum += unmarked
					}
					result := unmarked_sum * draw
					// check if this result is for the first or last winner
					if !firstWinnerIsKnown {
						firstWinnerIsKnown = true
						firstWinner = result
					} else {
						lastWinner = result
						return
					}
				}
			} else {
				// as a bing didn't happen, keep card to the next round
				// TODO: not sure if creating a new list in this way is
				// efficient. Are we creating copies of the struct every time?
				// Is there a solution with slices, or copying by reference?
				next_round_cards = append(next_round_cards, cards[card_i])
			}
		}
		cards = next_round_cards
	}
	return
}

func main() {
	draws, cards := readinput4()
	solA, solB := solve4(draws, cards, true)
	fmt.Println(solA)
	fmt.Println(solB)
}
