# PLOG1819_T3G4

Project for the Logic Programming (PLOG) class of the Master in Informatics and Computer Engineering (MIEIC) at the Faculty of Engineering of the University of Porto (FEUP). 

## Team Members 

Rui Jorge Leão Guedes
* Student Number: 201603854
* E-Mail: up201603854@fe.up.pt

João Fernando da Costa Meireles Barbosa
* Student Number: 201604156
* E-Mail: up201604156@fe.up.pt

## First Assignment - Prolog Application for a Board Game

### Description of Board Game - Pente

#### Origin
Pente is a strategy board game created by Gary Gabrel in 1977. Gary was a dishwasher at Hideaway Pizza, a restaurant in Stillwater, Oklahoma. Customers played Pente at this restaurant while waiting for their order. The game is based off a Japanese game called gomoku. Both are played in a 19x19 Go board with white and black stones.

![Pente Sample Game](https://upload.wikimedia.org/wikipedia/en/3/3f/Pente.jpg)

*A Sample Game of Pente*

#### Ruleset
Pente is played with two players and, like with most board games, players alternate their moves, with white being the first player. Each move corresponds to the placement of a single stone of the player's color on an intersection of the board. The placement is arbitrary, as long as the intersection is free (The board is initially empty).

**TODO Insert picture representing the evolution of the board with each move**

During the game there's a single event that changes the state of the board, other than the regular placement of stones, called a capture. Captures occur when, along any direction (Horizontal, Vertical or Diagonal), two contiguous stones of a color are surrounded by two stones of another color (For Example: X-O-O-X). When a capture occurs, the surrounded stones are removed from the board. It is only considered a capture if the capturing player takes the initiative, otherwise it is not considered a capture. Multiple captures can occur in a single move.

**TODO Insert picture illustrating when it's considered a capture or not and a multiple capture**

There are two winning conditions in Pente. A player wins when they either perform five captures (capture 10 opponent stones) or when they place at least five stones of their color in a row along any direction (Horizontal, Vertical, Diagonal).

**TODO Insert picture representing an example of a final state where a player wins.

#### Game State Representation
The game state can be represented by the displacement of stones in the board. Throughout the game

#### References
[Wikipedia - Pente](https://en.wikipedia.org/wiki/Pente)

[pente.org - Game Rules](https://pente.org/help/helpWindow.jsp?file=playGameRules)
