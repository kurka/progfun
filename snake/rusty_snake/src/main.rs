use console_engine::pixel;
use console_engine::Color;
use console_engine::KeyCode;
use rand::{thread_rng, Rng};

#[derive(Clone, Copy)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

#[derive(Clone, Copy)]
enum Cell {
    SnakePart(Direction),
    Fruit,
    EmptySpace,
}

enum SnakeState {
    Hungry,
    Digesting(usize),
}

fn raffle_fruit(
    grid: &Vec<Vec<Cell>>,
    lim_x: usize,
    lim_y: usize,
    snake_size: usize,
) -> (usize, usize) {
    // draw a fruit in the grid by picking a number within the empty cells count
    let empty_cells_count = (lim_x * lim_y) - snake_size;
    let mut rng = thread_rng();
    let n: usize = rng.gen_range(0..empty_cells_count);

    let mut empty_cells_visited = 0;
    for x in 0..lim_x {
        for y in 0..lim_y {
            if matches!(grid[x][y], Cell::EmptySpace) {
                empty_cells_visited += 1;
            }

            if empty_cells_visited == n {
                return (x, y);
            }
        }
    }

    unreachable!("An empty cell should always be found during loop")
}
fn main() {
    // initializes a screen of 20x10 characters with a target of 3 frames per second
    // coordinates will range from [0,0] to [19,9]
    let fps = 10;
    let lim_x = 40;
    let lim_y = 20;
    let snake_pixel = pixel::pxl_fg('#', Color::Green);
    let fruit_pixel = pixel::pxl_fg('o', Color::Red);
    let empty_pixel = pixel::pxl_fg('.', Color::DarkGrey);

    let mut grid: Vec<Vec<Cell>> = vec![vec![Cell::EmptySpace; lim_y]; lim_x];
    let mut engine = console_engine::ConsoleEngine::init(lim_x as u32, lim_y as u32, fps).unwrap();

    // initialize the snake with size 2
    let mut head_direction: Direction = Direction::Right;
    let mut head = (2, 1);
    let mut tail = (1, 1);
    let mut snake_size = 2;
    grid[head.0][head.1] = Cell::SnakePart(head_direction);
    grid[tail.0][tail.1] = Cell::SnakePart(head_direction);

    let mut fruit = raffle_fruit(&grid, lim_x, lim_y, snake_size);
    grid[fruit.0][fruit.1] = Cell::Fruit;
    let mut snake_state = SnakeState::Hungry;

    // Draw the initial screen
    for x in 0..lim_x {
        for y in 0..lim_y {
            let pxl = match &grid[x][y] {
                Cell::SnakePart(_) => snake_pixel,
                Cell::Fruit => fruit_pixel,
                Cell::EmptySpace => empty_pixel,
            };
            engine.set_pxl(x as i32, y as i32, pxl);
        }
    }

    // main loop, be aware that you'll have to break it because ctrl+C is captured
    loop {
        engine.wait_frame(); // wait for next frame + capture inputs
                             // engine.clear_screen(); // reset the screen

        engine.draw(); // draw the screen

        if engine.is_key_pressed(KeyCode::Up) && !matches!(head_direction, Direction::Down) {
            head_direction = Direction::Up;
        }
        if engine.is_key_pressed(KeyCode::Right) && !matches!(head_direction, Direction::Left) {
            head_direction = Direction::Right;
        }
        if engine.is_key_pressed(KeyCode::Down) && !matches!(head_direction, Direction::Up) {
            head_direction = Direction::Down;
        }
        if engine.is_key_pressed(KeyCode::Left) && !matches!(head_direction, Direction::Right) {
            head_direction = Direction::Left;
        }

        if engine.is_key_pressed(KeyCode::Char('q')) {
            // if the user presses 'q' :
            break; // exits app
        }

        // update head
        grid[head.0][head.1] = Cell::SnakePart(head_direction);
        head = match head_direction {
            Direction::Up => (head.0, if head.1 > 0 { head.1 - 1 } else { lim_y - 1 }),
            Direction::Down => (head.0, if head.1 < lim_y - 1 { head.1 + 1 } else { 0 }),
            Direction::Left => (if head.0 > 0 { head.0 - 1 } else { lim_x - 1 }, head.1),
            Direction::Right => (if head.0 < lim_x - 1 { head.0 + 1 } else { 0 }, head.1),
        };
        if matches!(grid[head.0][head.1], Cell::SnakePart(_)) {
            // TODO: crash the game
            engine.print(0, 0, "Game Over!");
        }
        if matches!(grid[head.0][head.1], Cell::Fruit) {
            snake_state = match snake_state {
                SnakeState::Digesting(x) => SnakeState::Digesting(x + 3),
                SnakeState::Hungry => SnakeState::Digesting(3),
            };
            fruit = raffle_fruit(&grid, lim_x, lim_y, snake_size);
            grid[fruit.0][fruit.1] = Cell::Fruit;
            engine.set_pxl(fruit.0 as i32, fruit.1 as i32, fruit_pixel);
        }

        grid[head.0][head.1] = Cell::SnakePart(head_direction);
        engine.set_pxl(head.0 as i32, head.1 as i32, snake_pixel);

        if matches!(snake_state, SnakeState::Hungry) {
            // update tail
            let tail_direction = match grid[tail.0][tail.1] {
                Cell::SnakePart(dir) => dir,
                _ => unreachable!("Tail should always point to a SnakePart!!!"),
            };

            grid[tail.0][tail.1] = Cell::EmptySpace;
            engine.set_pxl(tail.0 as i32, tail.1 as i32, empty_pixel);

            tail = match tail_direction {
                Direction::Up => (tail.0, if tail.1 > 0 { tail.1 - 1 } else { lim_y - 1 }),
                Direction::Down => (tail.0, if tail.1 < lim_y - 1 { tail.1 + 1 } else { 0 }),
                Direction::Left => (if tail.0 > 0 { tail.0 - 1 } else { lim_x - 1 }, tail.1),
                Direction::Right => (if tail.0 < lim_x - 1 { tail.0 + 1 } else { 0 }, tail.1),
            };
        } else {
            snake_size += 1;
        }

        if let SnakeState::Digesting(r) = snake_state {
            snake_state = if r == 0 {
                SnakeState::Hungry
            } else {
                SnakeState::Digesting(r - 1)
            };
        }
    }
}
