# Sudoku Game

A classic Sudoku game built with Scala.js, allowing users to solve Sudoku puzzles in a web browser. 

## Features

Interactive Sudoku Board: A 9x9 Sudoku board where users can input numbers to solve the puzzle.
Number Highlighting: Clickable buttons to highlight placed numbers on the board, helping puzzle solving.
Annotation Mode: Allows users to annotate cells with possible numbers, facilitating the puzzle-solving process.
Difficulty Levels: Users can choose from multiple difficulty levels (Easy, Medium, Hard) to start a new game.
Ending: The game automatically detects when the game is finished because all cells are filled and there is no duplicates in the rows, columns and 3 by 3 subgrids.




## Getting Started

#### Prerequisites:
- Scala and SBT installed on your machine
- A modern web browser


#### Installation
Clone the repository:
bash
git clone https://github.com/yourusername/sudoku-game.git


#### Navigate to the project directory:
cd sudoku-game


#### Compile the Scala.js project and generate JavaScript:
sbt fastLinkJS

Type localhost:8080 in a web browser to start playing.




## Usage

1. Select a difficulty level to start a new game, 1 being the easiest and 3 the hardest.
2. Fill in the grid according to the sudoku rule by double-clicking on a cell to select it, then inputting a number from the keyboard
3. You can use the number buttons to highlight all instances of that number on the board.
4. To annotate, you can press the "Annotate" button to enter or exit annotation mode for suggesting possible numbers for each cell. You can annoate several numbers in a cell of the grid but always double.click on the cell before inputting a new number.




## Contributing

Contributions to the Sudoku game project are welcome! Please follow these steps to contribute:

Fork the repository.
Create a new branch: git checkout -b feature/your_feature_name.
Make your changes and commit them: git commit -am 'Add some feature'.
Push to the branch: git push origin feature/your_feature_name.
Submit a pull request.




## License
This project is licensed under the MIT License - see the LICENSE.md file for details.



## Acknowledgments
Special thanks to EPFL CS214 course for providing useful and important information on how to implement a webapp.
