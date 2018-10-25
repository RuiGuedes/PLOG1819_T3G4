% ---- BOARD MANIPULATION ----
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

% Symbol converter
modelToView('0','+').
modelToView('1','O').
modelToView('2','X').

% Retrieves a Cell from a given board.

%	+LinNo: Row number for the cell to be retrieved.
%  	+ColNo: Row number for the cell to be retrieved.
%	+Board: Internal Representation of the board.
%	-Piece: Cell value retrieved from the board
getPiece(LinNo, ColNo, Board, Piece):-	
	getLine(LinNo, Board, Line), 
	getColumn(ColNo, Line, Piece).

% Retrieves a Line from a given board.

%	+LinNo: Number of the line to be retrieved.
%	+Board: Internal Representation of the board.
%	-Line: 	Line retrieved from the board
getLine(LinNo, Board, Line):-
	length(Board, MaxLines),
	getLine(LinNo, MaxLines, Board, Line).
	
getLine(MaxLines, MaxLines, [Line|_], Line).
getLine(LinNo, MaxLines, [_|RestBoard], Line):-
	NextLineNo is LinNo + 1, 
	getLine(NextLineNo, MaxLines, RestBoard, Line).

% Retrieves a cell from a given line.

%	+ColNo: Number of the cell to be retrieved.
%	+Line: 	List containing the elements of the line.
%	-Piece: Cell value to be retrieved from the line
getColumn(1, [Piece|_], Piece).
getColumn(ColNo, [_|RestLine], Piece):-	
	ColNo > 1, 
	PrevColNo is ColNo - 1, 
	getColumn(PrevColNo, RestLine, Piece). 

	
% Sets a cell on a board.

%	+LinNo: 	Row number for the cell to be edited.
%  	+ColNo: 	Row number for the cell to be edited.
%	+OldBoard: 	Internal Representation of the board before being edited.
%	-NewBoard: 	Internal Representation of the board after being edited.
%	+Piece: 	Cell value to be attributed.
setPiece(LinNo, ColNo, OldBoard, NewBoard, Piece):-	
	getLine(LinNo, OldBoard, OldLine), 
	setColumn(ColNo, Piece, OldLine, NewLine),
	setLine(LinNo, NewLine, OldBoard, NewBoard).
	
% Edits a cell from a given line.

%	+ColNo: 	Number of the cell to be edited.
%	+Piece: 	Cell value to be attributed.
%	+OldLine: 	List containing the elements of the line before being edited.
%	-NewLine: 	List containing the elements of the line after being edited.
setColumn(1, Piece, [_|T], [Piece|T]).
setColumn(ColNo, Piece, [H|T], [H|R]) :-	
	ColNo > 1, 
	PrevColNo is ColNo - 1,
	setColumn(PrevColNo, Piece, T, R).
							
% Edits a line from a given board.

%	+LinNo: 	Number of the line to be replaced.
%	+Line: 		Line to be attributed to the board.
%	+OldBoard: 	Internal Representation of the board before being edited.
%	-NewBoard: 	Internal Representation of the board after being edited.

setLine(LinNo, Line, OldBoard, NewBoard) :-
	length(OldBoard, MaxLines),
	setLine(LinNo, MaxLines, Line, OldBoard, NewBoard).
	
setLine(MaxLines, MaxLines, NewLine, [_|T], [NewLine|T]).					
setLine(LinNo, MaxLines, NewLine, [H|Told], [H|Tnew]) :-	
	LinNo < MaxLines,
	PrevLinNo is LinNo + 1,
	setLine(PrevLinNo, MaxLines, NewLine, Told, Tnew).										
											
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


displayRow([H|[]]):- 	modelToView(H,Symb), 
						put_char(Symb).
displayRow([H|T]):- 	modelToView(H,Symb), 
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
										
displayPlayerInfo:- format('   Player One -> [~p] Captures    ', 5), %change captures
					format('   Player Two -> [~p] Captures~n~n', 4).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% End - DISPLAY %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%										

% Column identifiers
columnSymb(['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z']).

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
											validateUserInput(Board, Line, Column).
handleInput(Board, Player, Line, Column):-	format('~n   Invalid move. Chosen cell is either invalid or occupied~n~n', []),
											handleInput(Board, Player, Line, Column).

% Retrieves player input
userInput(Player, X, Y):-	player(Player, Num), modelToView(Num, Symb),
							format('-> Player ~p [~p] turn:~n~n', [Num, Symb]), 	
							write('   Line: '), read(X),
							write('   Column: '), read(Y).

% Validates user input
validateUserInput(Board, X, Y):-	getPiece(X, Y, Board, Piece),
									Piece = '0'.
						
% Updates board Status
updateBoard(Player, Line, Column, Board, NewBoard):-	player(Player, Piece),
														setPiece(Line, Column, Board, NewBoard, Piece).
