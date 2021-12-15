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
	card Card
	marked map[int]Coordinate
	unmarked map[int]Coordinate
	rowsCounter [CardSize]int
	columnsCounter [CardSize]int

}

func readinput() (draws []int, cards []CardData) {
	var draws_string string
	// first read the list of draws
	fmt.Scan(&draws_string)

	fmt.Println("a", draws_string)
	for _, d := range strings.Split(draws_string, ",") {
		draw, _ := strconv.Atoi(d)
		draws = append(draws, draw)
	}
	fmt.Println("draws", draws)

	// then read the cards
	// var space string // couldn't make it work without reading this variable
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

func solve1(draws []int, cards []CardData) int {

	var result int
	for _, draw := range draws {
		fmt.Println("Drew", draw)
		for _, card := range cards {
			fmt.Println("checking:", card)
			if coords, ok := card.unmarked[draw]; ok {
				delete(card.unmarked, draw)
				card.marked[draw] = coords
				card.rowsCounter[coords.row] += 1
				card.columnsCounter[coords.column] += 1
				fmt.Println("deleted", draw, card)
			}
			for i := range card.rowsCounter {
				if card.rowsCounter[i] == CardSize || card.columnsCounter[i] == CardSize{
					unmarked_sum := 0;
					for unmarked := range card.unmarked {
						unmarked_sum += unmarked
					}
					result = unmarked_sum * draw
				}
			}
		}
	}
	return result


}

func main() {
	draws, cards := readinput()
	fmt.Println(draws)
	fmt.Println(cards)
	fmt.Println(solve1(draws, cards))
}
