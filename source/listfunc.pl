% ----BOARD MANIPULATION----
% API: getPiece() and setPiece()
% Auxiliary Predicates: getLine(), getColumn(), setLine() and setColumn()
%
% Initial State of Board:
board([ ['0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0'],
		['0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0'],
		['0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0'],
		['0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0'],
		['0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0'],
		['0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0'],
		['0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0'],
		['0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0'],
		['0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0'],
		['0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0'],
		['0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0'],
		['0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0'],
		['0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0'],
		['0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0'],
		['0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0'],
		['0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0'],
		['0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0'],
		['0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0'],
		['0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0']
	  ]).

% Column identifiers
columnSymb(['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z']).

% Symbol converter
converteSymb('0','+').
converteSymb('1','O').
converteSymb('2','X').

% Retrieves a Piece of a given board.

%	+LinNo: Row number for the piece to be retrieved.
%  	+ColNo: Row number for the piece to be retrieved.
%	+Board: Internal Representation of the board.
%	-Piece: Piece retrieved from the board
getPiece(LinNo, ColNo, Board, Piece):-	getLine(LinNo, Board, Line), 
									   	getColumn(Line, ColNo, Piece).

getLine(19, [Line|_], Line).
getLine(LinNo, [_|RestBoard], Line):- 	LinNo < 19, 
									  	PrevLineNo is LinNo + 1, 
									  	getLine(PrevLineNo, RestBoard, Line).

getColumn([Piece|_], 1, Piece).
getColumn([_|RestLine], ColNo, Piece):-	ColNo > 1, 
										PrevColNo is ColNo - 1, 
										getColumn(RestLine, PrevColNo, Piece). 
											

setPiece(LinNo, ColNo, OldBoard, NewBoard, Piece):-	getLine(LinNo, OldBoard, OldLine), 
													setColumn(ColNo, Piece, OldLine, NewLine),
													setLine(LinNo, OldBoard, NewBoard, NewLine).

setColumn(1, Piece, [_|T], [Piece|T]).
setColumn(ColNo, Piece, [H|T], [H|R]) :-	ColNo > 1, 
											PrevColNo is ColNo - 1,
											setColumn(PrevColNo, Piece, T, R).

							
setLine(19, [_|T], [NewLine|T], NewLine).					
setLine(LinNo, [H|T], [H|R], NewLine) :-	LinNo < 19,
											PrevLinNo is LinNo + 1,
											setLine(PrevLinNo, T, R, NewLine).
											
											
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Begin - DISPLAY %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
											
displayGame(Board):- length(Board, Size), displayColSymb(Size), displayGame(Board, Size), displayColSymb(Size), displayPlayerInfo.
displayGame([H], LineNumber):-	displayLine(H, LineNumber).												
displayGame([H|T], LineNumber):-	displayLine(H, LineNumber),
									displaySepLine,
									NextLineNumber is LineNumber - 1,
									displayGame(T, NextLineNumber).	

									
displayColSymb(Size):-	columnSymb(List), write('\n   '),  displayColSymb(List, Size), write('\n\n').									
displayColSymb([H|_], 1):- put_char(H).
displayColSymb([H|T], Size):-	put_char(H), 
								write('  '), 
								NextSize is Size - 1,
								displayColSymb(T, NextSize).

								
displaySepLine:- board(Board), length(Board, Size), write('   '), displaySepLine(Size).
displaySepLine(1):- write('|\n').
displaySepLine(Size):- 	write('|  '),
						NextSize is Size - 1,
						displaySepLine(NextSize).


displayRow([H|[]]):- 	converteSymb(H,Symb), 
						put_char(Symb).
displayRow([H|T]):- 	converteSymb(H,Symb), 
						put_char(Symb),
						write('--'), 
						displayRow(T).						

displayLine(BoardLine, LineNumber):- 	LineNumber < 10,
										format(" ~p ", [LineNumber]),
										displayRow(BoardLine),
										format(" ~p~n", [LineNumber]).
displayLine(BoardLine, LineNumber):- 	LineNumber > 9, 
										format("~p ", [LineNumber]),
										displayRow(BoardLine),
										format(" ~p~n", [LineNumber]). 
										
displayPlayerInfo:- format('   Player One -> ~p Captures    ', [5]), %change captures
					format('   Player Two -> ~p Captures~n~n', [4]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% End - DISPLAY %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%										


startGame:- board(Board), gameStep(Board, 'playerOne').

gameStep(Board, Player):- 	displayGame(Board), 
							handleInput(Board, Player, Line, Column),
							updateBoard(Player, Line, Column, Board, NewBoard), 
							switchPlayer(Player, NewPlayer),
							% Predicate responsible to manage game state (capture or winning condition)
							gameStep(NewBoard, NewPlayer).

switchPlayer('playerOne', 'playerTwo').
switchPlayer('playerTwo', 'playerOne').

% More info to be added

% Struct info: player - player symbol
player('playerOne', '1').  
player('playerTwo', '2').

% Handle input 
handleInput(Board, Player, Line, Column):- 	userInput(Player, Line, Column),
											getPiece(Line, Column, Board, Piece),
											validateUserInput(Board, Player, Line, Column, Piece).

% Retrieves player input
userInput(Player, X, Y):-	player(Player, Num), converteSymb(Num, Symb),
							format('-> Player ~p [~p] turn:~n~n', [Num, Symb]), 	
							write('   Line: '), read(X),
							write('   Column: '), read(Y). 

% Validates user input
validateUserInput(_, _, _, _, '0').
validateUserInput(Board, Player, X, Y, Piece):-	converteSymb(Piece, Symb),
												format('~n   Invalid move. Chosen cell is occupied with [~p]~n~n', [Symb]), !,
												handleInput(Board, Player, X, Y).
						
% Updates board Status
updateBoard(Player, Line, Column, Board, NewBoard):-	player(Player, Piece),
														setPiece(Line, Column, Board, NewBoard, Piece).
