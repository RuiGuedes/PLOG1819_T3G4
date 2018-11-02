%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Board Manipulation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API: getPiece/4 and setPiece/5
% Auxiliary Predicates: getLine/3, getColumn/3, setLine/4 and setColumn/4


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
modelToView('3','#').

% Struct info: player - player symbol - number of captures
player('playerOne', '1', _).  
player('playerTwo', '2', _).

% Retrieves a Cell from a given board.

%	+LinNo: Row number for the cell to be retrieved.
%  	+ColNo: Row number for the cell to be retrieved.
%	+Board: Internal Representation of the board.
%	-Piece: Cell value retrieved from the board
getPiece(LinNo, ColNo, Board, Piece):-	LinNo >= 1, LinNo =< 19,
										ColNo >= 1, ColNo =< 19,
										getLine(LinNo, Board, Line), 
										getColumn(ColNo, Line, Piece).
	
getPiece(_, _, _, Piece):- 				modelToView(Piece, '#').

% Retrieves a Line from a given board.

%	+LinNo: Number of the line to be retrieved.
%	+Board: Internal Representation of the board.
%	-Line: 	Line retrieved from the board
getLine(LinNo, Board, Line):-	length(Board, MaxLines),
								getLine(LinNo, MaxLines, Board, Line).
	
getLine(MaxLines, MaxLines, [Line|_], Line).
getLine(LinNo, MaxLines, [_|RestBoard], Line):-	NextLineNo is LinNo + 1, 
												getLine(NextLineNo, MaxLines, RestBoard, Line).

% Retrieves a cell from a given line.

%	+ColNo: Number of the cell to be retrieved.
%	+Line: 	List containing the elements of the line.
%	-Piece: Cell value to be retrieved from the line
getColumn(1, [Piece|_], Piece).
getColumn(ColNo, [_|RestLine], Piece):-	ColNo > 1, 
										PrevColNo is ColNo - 1, 
										getColumn(PrevColNo, RestLine, Piece). 

% Sets a cell on a board.

%	+LinNo: 	Row number for the cell to be edited.
%  	+ColNo: 	Row number for the cell to be edited.
%	+OldBoard: 	Internal Representation of the board before being edited.
%	-NewBoard: 	Internal Representation of the board after being edited.
%	+Piece: 	Cell value to be attributed.
setPiece(LinNo, ColNo, OldBoard, NewBoard, Piece):-	getLine(LinNo, OldBoard, OldLine), 
													setColumn(ColNo, Piece, OldLine, NewLine),
													setLine(LinNo, NewLine, OldBoard, NewBoard).
	
% Edits a cell from a given line.

%	+ColNo: 	Number of the cell to be edited.
%	+Piece: 	Cell value to be attributed.
%	+OldLine: 	List containing the elements of the line before being edited.
%	-NewLine: 	List containing the elements of the line after being edited.
setColumn(1, Piece, [_|T], [Piece|T]).
setColumn(ColNo, Piece, [H|T], [H|R]) :-	ColNo > 1, 
											PrevColNo is ColNo - 1,
											setColumn(PrevColNo, Piece, T, R).
							
% Edits a line from a given board.

%	+LinNo: 	Number of the line to be replaced.
%	+Line: 		Line to be attributed to the board.
%	+OldBoard: 	Internal Representation of the board before being edited.
%	-NewBoard: 	Internal Representation of the board after being edited.
setLine(LinNo, Line, OldBoard, NewBoard) :-	length(OldBoard, MaxLines),
											setLine(LinNo, MaxLines, Line, OldBoard, NewBoard).
	
setLine(MaxLines, MaxLines, NewLine, [_|T], [NewLine|T]).					
setLine(LinNo, MaxLines, NewLine, [H|Told], [H|Tnew]) :-	LinNo < MaxLines,
															PrevLinNo is LinNo + 1,
															setLine(PrevLinNo, MaxLines, NewLine, Told, Tnew).										

															
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Game Display %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API: displayGame/1, displayPlayerInfo/0
% Auxiliary Predicates: displayColSymb/1, displayLine/2, displayRow/1, displaySepLine/1


% Displays the game board in a user friendly format.
% The board must be rectangular.

% +Board: Internal Representation of the board to be displayed.								
displayGame(Board):-	Board = [FirstLine | _],
						length(Board, LineSize),
						length(FirstLine, ColSize),
						displayColSymb(ColSize),
						displayGame(Board, LineSize, ColSize),
						displayColSymb(ColSize).
	
displayGame([H], LineNumber, _):-			displayLine(H, LineNumber).											
displayGame([H|T], LineNumber, ColSize):-	displayLine(H, LineNumber),
											displaySepLine(ColSize),
											NextLineNumber is LineNumber - 1,
											displayGame(T, NextLineNumber).


% Displays a Row containing the identifiers of the collumns, allowing the player to easily identify cells

% +Size: Number of columns in the board to be displayed.
displayColSymb(Size):-	char_code('A', CharCode),
						write('\n   '), 
						displayColSymb(Size, 1, CharCode), 
						write('\n\n').
						
displayColSymb(Size, Size, CharCode):-	put_code(CharCode).
displayColSymb(Size, ColNo, CharCode):-	put_code(CharCode),
										write('  '),
										IncColNo is ColNo + 1,
										IncCharCode is CharCode + 1,
										displayColSymb(Size, IncColNo, IncCharCode).

										
% Display a Row of the board with its identifier, allowing the player to easily identify cells

% +BoardLine:	List representation of the Line to be displayed
% +LineNumber:	Number of the Line to be displayed
displayLine(BoardLine, LineNumber):- 	LineNumber < 10,
										format(" ~p ", [LineNumber]),
										displayRow(BoardLine),
										format(" ~p~n", [LineNumber]).
displayLine(BoardLine, LineNumber):- 	format("~p ", [LineNumber]),
										displayRow(BoardLine),
										format(" ~p~n", [LineNumber]).

% Display a Row of the board

% +BoardLine: List representation of the Line to be displayed
displayRow([H|[]]):- 	modelToView(H,Symb), 
						put_char(Symb).
displayRow([H|T]):- 	modelToView(H,Symb), 
						put_char(Symb),
						write('--'), 
						displayRow(T).										

						
% Displays a Line of characters between two different rows, providing a better visual experience

% +Size - Number of columns in the board to be displayed
displaySepLine(Size):- 	write('   '), 
						dispSepLine(Size).

dispSepLine(1):- 		write('|\n').
dispSepLine(Size):- 	write('|  '),
						DecSize is Size - 1,
						displaySepLine(DecSize).	
						

% Displays information about each players' number of captures 						
displayPlayerCaptInfo:- player('playerOne', _, P1Captures),
						player('playerTwo', _, P2Captures),
						format('   Player One -> [~p] Captures    ', P1Captures), 
						format('   Player Two -> [~p] Captures~n~n', P2Captures).
											
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Game State Transitions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Game Initial State:		startGame/0
% Game End
% Game State Transtion:		gameStep/3
% Board State Transition: 	boardStep/5

% Initializes the game in its initial state (Empty Board)
startGame:- board(Board),
			gameStep(Board, player('playerOne', P1_Num, 0), player('playerTwo', P2_Num, 0)).		

			
% Ends the game depending on its final state (Win by Pieces in a row, Win by captures, Draw)			
endGame(Board, Num):- 	displayGame(Board),
						modelToView(Num, Symb),
						format('-> Victory: Player ~p was won the game by having five <~p> pieces in a row!~n~n', [Num, Symb]).
						
endGame(Board, Num, Captures):- displayGame(Board),
								format('-> Victory: Player ~p was won the game by making 10 or more captures [~p] !~n~n', [Num, Captures]).																													
								
endGame(Board):- 	displayGame(Board),
					format('-> Draw: No more cells available !~n~n', []).


% Handles the transition between two game states.

% +Board: 		Internal Representation of the Board in its current state.
% +CurrPlayer:	Internal Representation of the Player going to play this turn
% +NextPlayer:	Internal Representation of this turn's player opponent.
gameStep(Board, CurrPlayer, NextPlayer):- 	displayGame(Board),	
											displayPlayerCaptInfo(),
											boardEmptyCells(Board, 0, EmptyCells), 
											handleInput(Board, CurrPlayer, Line, Column, EmptyCells),
											boardStep(Board, Line, Column, _Score).
											% updateBoard(CurrPlayer, Line, Column, Board, NewBoard), !,											
											% gameTransitionState(player(CurrPlayer, Curr_Num, Curr_Capt), player(NextPlayer, Next_Num, Next_Capt), Line, Column, NewBoard).


% Handles the transition between two board states. 
% This including updating the board and checking its current status in respect to the game

% +Board:		Internal Representation of the Board in its current state.
% +CurrPlayer:	Internal Representation of the Player going to play this turn.
% +SetLine:		Line of the cell where the player is playing his piece.
% +SetColumn:	Column of the cell where the player is playing his piece.
% -Score:		Score of the resulting board state (Used to evaluate AI movements).
boardStep(Board, player(_, Piece, CaptureNo), SetLine, SetColumn, Score) :- setPiece(SetLine, SetColumn, Board, NewBoard, Piece).
																			% Do victory and capture analysis here.

														
% Check possible game state transations: Victory or Captures																										
gameTransitionState(player(CurrPlayer, Curr_Num, _), _, Line, Column, Board):- 	updateGameState(CurrPlayer, Line, Column, Board), endGame(Board, Curr_Num).
gameTransitionState(player(CurrPlayer, Curr_Num, Curr_Capt), player(NextPlayer, Next_Num, Next_Capt), Line, Column, Board):- 	verifyCaptures(CurrPlayer, Line, Column, Board, NewBoard, Num_Capt), 
																																updatePlayerCaptures(player(CurrPlayer, Curr_Num, Curr_Capt), player(NewPlayer, New_Num, New_Num_Capt), Num_Capt),
																																checkPlayerCaptures(NewBoard, player(NewPlayer, New_Num, New_Num_Capt), player(NextPlayer, Next_Num, Next_Capt)).
% After 10 captures the current player is considered the winner
checkPlayerCaptures(Board, player(_, Curr_Num, Curr_Capt), _):- Curr_Capt >= 10, !,  endGame(Board, Curr_Num, Curr_Capt).
checkPlayerCaptures(Board, player(CurrPlayer, Curr_Num, Curr_Capt), player(NextPlayer, Next_Num, Next_Capt)):- 	Curr_Capt < 10, 
																												boardEmptyCells(Board, 0, EmptyCells), 
																												checkDraw(Board, player(CurrPlayer, Curr_Num, Curr_Capt), player(NextPlayer, Next_Num, Next_Capt), EmptyCells).
																												
																		
checkDraw(Board, player(_, _, _), player(_, _, _), 0):- endGame(Board).								
checkDraw(Board, player(CurrPlayer, Curr_Num, Curr_Capt), player(NextPlayer, Next_Num, Next_Capt), _):- gameStep(Board, player(NextPlayer, Next_Num, Next_Capt), player(CurrPlayer, Curr_Num, Curr_Capt)).																

%%%%%%%%%%%%%%%%%%
%% Handle input %%
%%%%%%%%%%%%%%%%%%

handleInput(Board, Player, Line, Column, EmptyCells):- 	userInput(Player, Line, Column),
														validateUserInput(Board, Line, Column, EmptyCells).
handleInput(Board, Player, Line, Column, EmptyCells):-	EmptyCells = 361, 
														format('~n   Invalid move. First move must be at the center of the board.~n~n', []),
														handleInput(Board, Player, Line, Column, EmptyCells).
handleInput(Board, Player, Line, Column, EmptyCells):-	format('~n   Invalid move. Chosen cell is either invalid or occupied.~n~n', []),
														handleInput(Board, Player, Line, Column, EmptyCells).

% Retrieves player input
userInput(Player, X, Y):-	player(Player, Num, _), modelToView(Num, Symb),
							format('-> Player ~p [~p] turn:~n~n', [Num, Symb]), 	
							write('   Line: '), read(X),
							write('   Column: '), read(Y), !.

% Validates user input
validateUserInput(Board, X, Y, 361):-	X = 10, Y = 10,
										getPiece(X, Y, Board, Piece),
										Piece = '0'.
validateUserInput(Board, X, Y, EmptyCells):-	EmptyCells \= 361,
												getPiece(X, Y, Board, Piece),
												Piece = '0'.

% Determines how many cells still empty
boardEmptyCells([], Iterator, EmptyCells):-  EmptyCells is Iterator.
boardEmptyCells([H|T], Iterator, EmptyCells):-  lineEmptyCells(H, 0, LineEmptyCells), NewIterator is Iterator + LineEmptyCells, boardEmptyCells(T, NewIterator, EmptyCells).

lineEmptyCells([], Iterator, EmptyCells):- EmptyCells is Iterator.
lineEmptyCells([H|T], Iterator, EmptyCells):- H = '0', NewIterator is Iterator + 1, lineEmptyCells(T, NewIterator, EmptyCells).
lineEmptyCells([_|T], Iterator, EmptyCells):- lineEmptyCells(T, Iterator, EmptyCells). 
				
				
%%%%%%%%%%%%%%%%%%%%%%%%														
%% Updates game state %%
%%%%%%%%%%%%%%%%%%%%%%%%
													
updateGameState(Player, Line, Column, Board):- victoryBySequence(Player, Line, Column, Board).

% Checks possible victory on:

% - horizontal direction
victoryBySequence(Player, Line, Column, Board):-	NewColumn is Column - 4, horizontalVictory(Player, Line, NewColumn, Board, 0).

% - vertical direction
victoryBySequence(Player, Line, Column, Board):-	NewLine is Line - 4, verticalVictory(Player, NewLine, Column, Board, 0).	

% - negative diagonal direction
victoryBySequence(Player, Line, Column, Board):-    NewColumn is Column + 4,
													NewLine is Line - 4,
													negativeDiagonalVictory(Player, NewLine, NewColumn, Board, 0).

% - positive diagonal direction
victoryBySequence(Player, Line, Column, Board):-	NewColumn is Column - 4,
													NewLine is Line - 4,
													positiveDiagonalVictory(Player, NewLine, NewColumn, Board, 0).
												

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  Victory Possibilities %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% - horizontal 
horizontalVictory(_, _, _, _, 5):- !, fail.
horizontalVictory(Player, Line, Column, Board, Iterator):-	retrievePieces('horizontal', 0, Line, Column, Board, _, Pieces), !,
															validateVictory('horizontal', Player, Line, Column, Board, Iterator, Pieces, 0).															

																		
% - vertical									
verticalVictory(_, _, _, _, 5):- !, fail.
verticalVictory(Player, Line, Column, Board, Iterator):-	retrievePieces('vertical', 0, Line, Column, Board, _, Pieces), !, 
															validateVictory('vertical', Player, Line, Column, Board, Iterator, Pieces, 0).		
															

% - negative diagonal
negativeDiagonalVictory(_, _, _, _, 5):- !, fail.							
negativeDiagonalVictory(Player, Line, Column, Board, Iterator):-	retrievePieces('negativeDiagonal', 0, Line, Column, Board, _, Pieces), !,
																	validateVictory('negativeDiagonal', Player, Line, Column, Board, Iterator, Pieces, 0).																													
																																																							
																					
% - positive diagonal
positiveDiagonalVictory(_, _, _, _, 5):- !, fail.
positiveDiagonalVictory(Player, Line, Column, Board, Iterator):-	retrievePieces('positiveDiagonal', 0, Line, Column, Board, _, Pieces), !,
																	validateVictory('positiveDiagonal', Player, Line, Column, Board, Iterator, Pieces, 0).																													
																	

																	
%%%%%%%%%%%%%%%%%%%%%%%
%% Validates Victory %%
%%%%%%%%%%%%%%%%%%%%%%%

validateVictory(_, _, _, _, _, _, [], 5).
validateVictory(Type, Player, Line, Column, Board, Iterator, [H|T], NumPieces):- 	player(Player, Symb, _),
																					Symb = H, !,
																					NewNumPieces is NumPieces + 1,
																					validateVictory(Type, Player, Line, Column, Board, Iterator, T, NewNumPieces).
validateVictory('horizontal', Player, Line, Column, Board, Iterator, _, _):- 	NewColumn is Column + 1,
																				NewIterator is Iterator + 1, !,
																				horizontalVictory(Player, Line, NewColumn, Board, NewIterator).
validateVictory('vertical', Player, Line, Column, Board, Iterator, _, _):-	NewLine is Line + 1,
																			NewIterator is Iterator + 1, !,
																			verticalVictory(Player, NewLine, Column, Board, NewIterator).																				
validateVictory('negativeDiagonal', Player, Line, Column, Board, Iterator, _, _):-	NewLine is Line + 1,
																					NewColumn is Column - 1,
																					NewIterator is Iterator + 1, !,
																					negativeDiagonalVictory(Player, NewLine, NewColumn, Board, NewIterator).																		
validateVictory('positiveDiagonal', Player, Line, Column, Board, Iterator, _, _):-	NewLine is Line + 1,
																					NewColumn is Column + 1,
																					NewIterator is Iterator + 1, !,
																					positiveDiagonalVictory(Player, NewLine, NewColumn, Board, NewIterator).																						


%%%%%%%%%%%%%%%%%%%%%
%% Verify Captures %%
%%%%%%%%%%%%%%%%%%%%%

verifyCaptures(CurrPlayer, Line, Column, Board, NewBoard, Num_Capt):- 	verifyHorizontalCaptures(CurrPlayer, Line, Column, Board, Hor_Board, Hor_Capt), 
																		verifyVerticalCaptures(CurrPlayer, Line, Column, Hor_Board, Ver_Board, Ver_Capt),
																		verifyNegativeDiagonalCaptures(CurrPlayer, Line, Column, Ver_Board, Neg_Board, Neg_Capt),
																		verifyPositiveDiagonalCaptures(CurrPlayer, Line, Column, Neg_Board, NewBoard, Pos_Capt),
																		Num_Capt is Hor_Capt + Ver_Capt + Neg_Capt + Pos_Capt .

% - horizontal captures
verifyHorizontalCaptures(CurrPlayer, Line, Column, Board, NewBoard, Hor_Capt):- 	player(CurrPlayer, Symb, _), 
																					NewColumn is Column - 3, 
																					retrievePieces('horizontal', 1, Line, Column, Board, _, Right_Pieces), 
																					retrievePieces('horizontal', 1, Line, NewColumn, Board, _, Left_Pieces),
																					validateCapture('horizontal', 0, Symb, Line, Column, Board, Tmp_Board, Right_Pieces, Right_Capt),
																					validateCapture('horizontal', 0, Symb, Line, NewColumn, Tmp_Board, NewBoard, Left_Pieces, Left_Capt), 
																					Hor_Capt is Right_Capt + Left_Capt .
																		
% - vertical captures
verifyVerticalCaptures(CurrPlayer, Line, Column, Board, NewBoard, Ver_Capt):- 	player(CurrPlayer, Symb, _), 
																				NewLine is Line - 3, 
																				retrievePieces('vertical', 1, Line, Column, Board, _, Top_Pieces), 
																				retrievePieces('vertical', 1, NewLine, Column, Board, _, Bottom_Pieces),
																				validateCapture('vertical', 0, Symb, Line, Column, Board, Tmp_Board, Top_Pieces, Top_Capt),
																				validateCapture('vertical', 0, Symb, NewLine, Column, Tmp_Board, NewBoard, Bottom_Pieces, Bottom_Capt), 
																				Ver_Capt is Top_Capt + Bottom_Capt .


% - negativeDiagonal captures
verifyNegativeDiagonalCaptures(CurrPlayer, Line, Column, Board, NewBoard, Neg_Capt):- 	player(CurrPlayer, Symb, _), 
																						NewLine is Line - 3,
																						NewColumn is Column + 3,
																						retrievePieces('negativeDiagonal', 1, Line, Column, Board, _, Top_Pieces), 
																						retrievePieces('negativeDiagonal', 1, NewLine, NewColumn, Board, _, Bottom_Pieces), 
																						validateCapture('negativeDiagonal', 0, Symb, Line, Column, Board, Tmp_Board, Top_Pieces, Top_Capt),
																						validateCapture('negativeDiagonal', 0, Symb, NewLine, NewColumn, Tmp_Board, NewBoard, Bottom_Pieces, Bottom_Capt), 
																						Neg_Capt is Top_Capt + Bottom_Capt .

% - positiveDiagonal captures																		
verifyPositiveDiagonalCaptures(CurrPlayer, Line, Column, Board, NewBoard, Pos_Capt):- 	player(CurrPlayer, Symb, _), 
																						NewLine is Line - 3,
																						NewColumn is Column - 3,
																						retrievePieces('positiveDiagonal', 1, Line, Column, Board, _, Top_Pieces), 
																						retrievePieces('positiveDiagonal', 1, NewLine, NewColumn, Board, _, Bottom_Pieces), 
																						validateCapture('positiveDiagonal', 0, Symb, Line, Column, Board, Tmp_Board, Top_Pieces, Top_Capt),
																						validateCapture('positiveDiagonal', 0, Symb, NewLine, NewColumn, Tmp_Board, NewBoard, Bottom_Pieces, Bottom_Capt), 
																						Pos_Capt is Top_Capt + Bottom_Capt .


%%%%%%%%%%%%%%%%%%%%%%%%
%% Validates Captures %%
%%%%%%%%%%%%%%%%%%%%%%%%

validateCapture(Direction, 0, Symb, Line, Column, Board, NewBoard, [H|T], Capt):- 	Symb = H, validateCapture(Direction, 1, Symb, Line, Column, Board, NewBoard, T, Capt).
validateCapture(Direction, 1, Symb, Line, Column, Board, NewBoard, [H|T], Capt):- 	Symb \= H, Symb \= '#', Symb \= '0',  
																					validateCapture(Direction, 2, Symb, Line, Column, Board, NewBoard, T, Capt).
validateCapture(Direction, 2, Symb, Line, Column, Board, NewBoard, [H|T], Capt):- 	Symb \= H, Symb \= '#', Symb \= '0',
																					validateCapture(Direction, 3, Symb, Line, Column, Board, NewBoard, T, Capt).
validateCapture('horizontal', 3, Symb, Line, Column, Board, NewBoard, [H|_], Capt):- 	Symb = H,
																						Col1 is Column + 1, setPiece(Line, Col1, Board, Tmp_Board, '0'),
																						Col2 is Column + 2, setPiece(Line, Col2, Tmp_Board, NewBoard, '0'),
																						Capt is 1.
validateCapture('vertical', 3, Symb, Line, Column, Board, NewBoard, [H|_], Capt):- 	Symb = H,
																					Lin1 is Line + 1, setPiece(Lin1, Column, Board, Tmp_Board, '0'),
																					Lin2 is Line + 2, setPiece(Lin2, Column, Tmp_Board, NewBoard, '0'),
																					Capt is 1.
validateCapture('negativeDiagonal', 3, Symb, Line, Column, Board, NewBoard, [H|_], Capt):- 	Symb = H,
																							Lin1 is Line + 1, Col1 is Column - 1, setPiece(Lin1, Col1, Board, Tmp_Board, '0'),
																							Lin2 is Line + 2, Col2 is Column - 2, setPiece(Lin2, Col2, Tmp_Board, NewBoard, '0'),
																							Capt is 1.
validateCapture('positiveDiagonal', 3, Symb, Line, Column, Board, NewBoard, [H|_], Capt):- 	Symb = H,
																							Lin1 is Line + 1, Col1 is Column + 1, setPiece(Lin1, Col1, Board, Tmp_Board, '0'),
																							Lin2 is Line + 2, Col2 is Column + 2, setPiece(Lin2, Col2, Tmp_Board, NewBoard, '0'),
																							Capt is 1.																
validateCapture(_, _, _, _, _, Board, NewBoard, _, Capt):- 	getPiece(1,1,Board, Piece), 
															setPiece(1,1,Board, NewBoard, Piece),
															Capt is 0.


%%%%%%%%%%%%%%%%%%%%%
%% Update Captures %%
%%%%%%%%%%%%%%%%%%%%%

updatePlayerCaptures(player(CurrPlayer, Curr_Num, Curr_Capt), player(CurrPlayer, Curr_Num, New_Num_Capt), Num_Capt):- New_Num_Capt is Curr_Capt + Num_Capt .

																																						
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%																				
%% Retrieves a list of X pieces in a certain direction %%	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% - horizontal direction																		
retrievePieces(_, 4, Line, Column, Board, OldPieces, NewPieces):-	getPiece(Line, Column, Board, Piece),
																			append([Piece], OldPieces, NewPieces).
retrievePieces('horizontal', Iterator, Line, Column, Board, OldPieces, NewPieces):- 	getPiece(Line, Column, Board, Piece),
																						append([Piece], OldPieces, Pieces),
																						NewColumn is Column + 1,
																						NewIterator is Iterator + 1,	
																						retrievePieces('horizontal', NewIterator, Line, NewColumn, Board, Pieces, NewPieces).
																						
% - vertical direction
retrievePieces('vertical', Iterator, Line, Column, Board, OldPieces, NewPieces):- 	getPiece(Line, Column, Board, Piece),
																					append([Piece], OldPieces, Pieces),
																					NewLine is Line + 1,
																					NewIterator is Iterator + 1,	
																					retrievePieces('vertical', NewIterator, NewLine, Column, Board, Pieces, NewPieces).	

% - negative diagonal direction																																												
retrievePieces('negativeDiagonal', Iterator, Line, Column, Board, OldPieces, NewPieces):- 	getPiece(Line, Column, Board, Piece),
																							append([Piece], OldPieces, Pieces),
																							NewLine is Line + 1,
																							NewColumn is Column - 1,
																							NewIterator is Iterator + 1,
																							retrievePieces('negativeDiagonal', NewIterator, NewLine, NewColumn, Board, Pieces, NewPieces).		
																							
% - positive diagonal direction																																												
retrievePieces('positiveDiagonal', Iterator, Line, Column, Board, OldPieces, NewPieces):- 	getPiece(Line, Column, Board, Piece), 
																							append([Piece], OldPieces, Pieces),
																							NewLine is Line + 1,
																							NewColumn is Column + 1,
																							NewIterator is Iterator + 1,
																							retrievePieces('positiveDiagonal', NewIterator, NewLine, NewColumn, Board, Pieces, NewPieces).																																														