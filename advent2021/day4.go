package main

import (
	"fmt"
	"io"
	"strconv"
	"strings"
)

const CardSize = 5

type Card = [CardSize][CardSize]int

type Coordinate struct {
	row, column int
}

type CardData struct {
	card           Card
	marked         map[int]Coordinate
	unmarked       map[int]Coordinate
	rowsCounter    [CardSize]int
	columnsCounter [CardSize]int
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
CardLoop:
	for {
		var card Card
		unmarked := make(map[int]Coordinate, CardSize*CardSize)
		var card_value int
		for i := 0; i < CardSize; i++ {
			for j := 0; j < CardSize; j++ {
				_, err := fmt.Scan(&card_value)
				if err == io.EOF {
					break CardLoop
				}
				card[i][j] = card_value
				unmarked[card_value] = Coordinate{i, j}
			}
		}
		cardData := CardData{card, map[int]Coordinate{}, unmarked, [CardSize]int{}, [CardSize]int{}}
		cards = append(cards, cardData)
	}

	return
}

func solve4a(draws []int, cards *[]CardData) (result int) {

	for _, draw := range draws {
		for card_i := range *cards {
			card := &(*cards)[card_i]
			if coords, ok := card.unmarked[draw]; ok {
				delete(card.unmarked, draw)
				card.marked[draw] = coords
				card.rowsCounter[coords.row] += 1
				card.columnsCounter[coords.column] += 1
			}
			for i := range card.rowsCounter {
				if card.rowsCounter[i] == CardSize || card.columnsCounter[i] == CardSize {
					unmarked_sum := 0
					for unmarked := range card.unmarked {
						unmarked_sum += unmarked
					}
					result = unmarked_sum * draw
					return result
				}
			}
		}
	}
	return result

}

func main() {
	draws, cards := readinput4()
	fmt.Println(solve4a(draws, &cards))
}
