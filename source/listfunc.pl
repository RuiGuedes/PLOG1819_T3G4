%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Board Manipulation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API: boardSize/3, emptyBoard/1, fullBoard/1, getPiece/4 and setPiece/5
% Auxiliary Predicates: getLine/3, getColumn/3, setLine/4 and setColumn/4


% Initial State of Board:
initialBoard([ 	['0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0'],
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


% Retrieves the dimensions of a rectangular board.

% +Board: 	 	Internal Representation of the board to be checked.
% -LineSize: 	Number of Lines in the board.
% -ColumnSize: 	Number of Columns in the board.
boardSize(Board, LineSize, ColSize):-	Board = [FirstLine | _],
										length(Board, LineSize),
										length(FirstLine, ColSize).
												
												
% Succeeds if the board is empty (is in its initial state)

% +Board: Internal Representation of the board to be checked.
emptyBoard(Board):- initialBoard(Board).

% Succeeds if the board is full (resulting in a draw)

% +Board: Internal Representation of the board to be checked.
fullBoard([]).
fullBoard([H|T]):- 	fullLine(H),
					fullBoard(T).
fullLine([]).
fullLine([H|T]):-	H \= '0',
					fullLine(T).
					

% Retrieves a Cell from a given board.

%	+LinNo: Row number for the cell to be retrieved.
%  	+ColNo: Row number for the cell to be retrieved.
%	+Board: Internal Representation of the board.
%	-Piece: Cell value retrieved from the board
getPiece(LinNo, ColNo, Board, Piece):-	boardSize(Board, LineSize, ColSize),
										LinNo >= 1, LinNo =< LineSize,
										ColNo >= 1, ColNo =< ColSize,
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
% The board will be improperly displayed if it contains 100 or more lines (100+ Lines will be offset to the right)
% or if there are more than 26 columns (identifiers will still correspond to a single character, however it'll keep going beyond letters).

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
											displayGame(T, NextLineNumber, ColSize).


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
						dispSepLine(DecSize).	
						

% Displays information about each players' number of captures 						
displayPlayerCaptInfo(player(playerOne, _, P1Captures, _), player(playerTwo, _, P2Captures, _)):-	format('   Player One -> [~p] Captures    ', P1Captures), 
																									format('   Player Two -> [~p] Captures~n~n', P2Captures).
displayPlayerCaptInfo(PlayerTwo, PlayerOne):- displayPlayerCaptInfo(PlayerOne, PlayerTwo).																								
						
						
						
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Input Handling %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API: handleInput/4.
% Auxiliary Predicates: userInput/3, flushInput/0, readLine/1, readColumn/1, validateUserInput/3.

% Handles Player Input for current turn.
% The output provided is compatible with the board accessors, in particular setPiece/5.

% +Board: 	 Internal Representation of the Board in its current state.
% +PlayerID: ID of the Player whose input is being retrieved.
% -Line: 	 Line number chosen by the player for its turn.
% -Column: 	 Column number chosen by the player for its turn.
handleInput(Board, player(_, PieceID, _, _), Line, Column):- 	userInput(PieceID, Line, Column),
																validateUserInput(Board, Line, Column), !.
handleInput(Board, Player, Line, Column):-	emptyBoard(Board), !,
											format('~n   Invalid move. First move must be at the center of the board.~n~n', []),
											handleInput(Board, Player, Line, Column).
handleInput(Board, Player, Line, Column):-	format('~n   Invalid move. Chosen cell is either invalid or occupied.~n~n', []),
											handleInput(Board, Player, Line, Column).

% Retrieves the player input (after processing)

% +PieceID:	Model ID of the player's piece.
% -Line: 	Line number chosen by the player for its turn.
% -Column: 	Column number chosen by the player for its turn.
userInput(PieceID, Line, Column):-	modelToView(PieceID, PieceSymb),
									format('-> Player ~p [~p] turn:~n~n', [PieceID, PieceSymb]),
									write('   Line: '),   read(Line),
									write('   Column: '), read(Column),
									nl.
						
%%%%%%%%% ### TODO ### - Figure out how to properly handle input					
						
% Reads the number of the line input by the user

% -Line: Line number chosen by the player for its turn.
readLine(Line):- 		readLine(0, Line).

readLine(Num, Line):- 	get_code(CharCode),
						Digit is CharCode - 48, %Ascii value for '0'.
						Digit >= 0, Digit =< 9,
						NextNum is Num + Digit * 10,
						readLine(NextNum, Line).					
readLine(Line, Line).


% Reads the letter of the column input by the user.
% The letter is converted to the internal column number.

% -Column: Column number chosen by the player for its turn.
readColumn(Column):- get_code(CharCode),
					 Column is CharCode - 65. %Ascii value for 'A'.

					
% Suceeds if the provided user input is valid.
% If the board has even columns of lines, the middle will be offset to the right / down.

% +Board: 	Internal Representation of the Board in its current state.
% +Line: 	Line number chosen by the player for its turn.
% +Column: 	Column number chosen by the player for its turn.
validateUserInput(Board, Line, Column):- 	emptyBoard(Board),
											!,
											boardSize(Board, LineSize, ColSize),
											Line * 2 =:= LineSize + 1,
											Column * 2 =:= ColSize + 1.
validateUserInput(Board, Line, Column):-	getPiece(Line, Column, Board, '0').
											
											
											
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Game State Transitions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Game Initial State:					startGame/0
% Game State Handling and Transition:	gameStep/3, nextGameStep/4
% Board State Handling and Transition: 	boardStep/7
% Game End States:						winGame/3, drawGame/3

% Initializes the game in its initial state (Empty Board)
startGame:- initialBoard(Board),
			gameStep(Board, player(playerOne, '1', 0, 0), player(playerTwo, '2', 0, 0)).


% Computes a single game state.

% +Board: 		Internal Representation of the Board in its current state.
% +CurrPlayer:	ID of the Player going to play this turn
% +NextPlayer:	ID of this turn's player opponent.
gameStep(Board, CurrPlayer, NextPlayer):- 	displayGame(Board),	
											displayPlayerCaptInfo(CurrPlayer, NextPlayer), 
											handleInput(Board, CurrPlayer, Line, Column),
											boardStep(Board, NewBoard, CurrPlayer, NextPlayer, NewCurrPlayer, Line, Column, Score),											
											nextGameStep(NewBoard, NextPlayer, NewCurrPlayer, Score).


% Handles the transition between two board states. 
% This including updating the board and checking its current status in respect to the game

% +Board:		Internal Representation of the Board in its current state.
% +NewBoard:	Internal Representation of the Board in its resulting state.
% +CurrPlayer:	Internal Representation of the Player going to play this turn.
% +NextPlayer:	Internal Representation of the opponent of the Player playing this turn.
% +SetLine:		Line of the cell where the player is playing his piece.
% +SetColumn:	Column of the cell where the player is playing his piece.
% -Score:		Score of the resulting state (Used to evaluate AI movements and end states) Range:[-100, 100].
boardStep(Board, NewBoard, player(CurrPlayerID, CurrPiece, CurrCaptureNo, CurrSequenceNo), player(_, NextPiece, NextCaptureNo, NextSequenceNo), player(CurrPlayerID, CurrPiece, NewCaptureNo, NewSequenceNo), SetLine, SetColumn, Score) :- 	
		setPiece(SetLine, SetColumn, Board, NewBoard, CurrPiece),
		updateSequence(CurrPiece, NewBoard, SetLine, SetColumn, CurrSequenceNo, NewSequenceNo),
		updateCaptures(CurrPiece, NextPiece, NewBoard, SetLine, SetColumn, CurrCaptureNo, NewCaptureNo),
		value(NewCaptureNo, NewSequenceNo, NextCaptureNo, NextSequenceNo, Score). 

% Handles the transition between two game states		

% +NewBoard:		Internal Representation of the Board to be transitioned to the next state. 
% +NewCurrPlayer:	Internal Representation of the Player going to play the next turn. 
% +NewNextPlayer:	Internal Representation of the opponent of the Player playing the next turn.
% +Score:			Score of the game state (Used to evaluate AI movements and end states) Range:[-100, 100].
nextGameStep(NewBoard, NewCurrPlayer, NewNextPlayer, 100) 	:- 	winGame(NewBoard, NewNextPlayer, NewCurrPlayer). 	% The player who played the previous turn won the game (NewNextPlayer).
nextGameStep(NewBoard, NewCurrPlayer, NewNextPlayer, _) 	:- 	fullBoard(NewBoard), drawGame(NewBoard, NewNextPlayer, NewCurrPlayer).
nextGameStep(NewBoard, NewCurrPlayer, NewNextPlayer, _) 	:- 	gameStep(NewBoard, NewCurrPlayer, NewNextPlayer).																


% TODO - Final Versions of the end game.

% Ends the game, displaying the final state of the board and final stats of the players

% +Board:			Internal Representation of the Board in its final state.
% +WinningPlayer:	Internal Representation of the Player who won the game.
% +LosingPlayer:	Internal Representation of the Player who lost the game.
winGame(Board, player(_, Num, _, _), _LosingPlayer):- 	displayGame(Board),
														modelToView(Num, Symb),
														format('-> Victory: Player ~p won the game!~n~n', [Num, Symb]).

% Ends the game, displaying the final state of the board and final stats of the players

% +Board:			Internal Representation of the Board in its final state.
% +WinningPlayer:	Internal Representation of the Player who won the game.
% +LosingPlayer:	Internal Representation of the Player who lost the game.
drawGame(Board, _WinningPlayer, _LosingPlayer):- 	displayGame(Board),
													write('-> Draw: The board has been filled with neither players winning!~n~n').														


					
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Board Evaluation (AI) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Computes the score of a game state to quantify how well the game is for a player

% +CurrCaptureNo:	Number of captures by the current turn's player
% +CurrSequenceNo:	Number of pieces in a row the current turn's player has
% +NextSequenceNo:	Number of pieces in a row the current player's opponent has.
% -Score:			Score attributed to the game state in the interval [-100, 100], where a maximal score represents a better state for the current player.
value(CurrCaptureNo, CurrSequenceNo, _, _, 100)								:-	(CurrCaptureNo >= 1 ; CurrSequenceNo >= 5).
value(_, _, NextCaptureNo, NextSequenceNo, -100) 							:-	(NextCaptureNo >= 10 ; NextSequenceNo >= 5).
value(CurrCaptureNo, CurrSequenceNo, NextCaptureNo, NextSequenceNo, Score) 	:-	Score = CurrCaptureNo - NextCaptureNo + CurrSequenceNo - NextSequenceNo.
				
				
				
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Sequence and Capture Check %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API: 					updateSequence/6, updateCaptures/7

% Auxiliary predicates: getSequence/5, getSequence/6, sequence/8, getCaptures/7, capture/8


% types of directions and their row and column increment
direction(horizontal ,  0, 1).
direction(vertical   ,  1, 0).
direction(posDiagonal,  1, 1).
direction(negDiagonal, -1, 1).


% Updates the Sequence Number of the player whose turn is being processed.

% Must be called after the board is updated with the player's new piece

% +Piece: 			Model ID of the player's piece.
% +Board:			Internal Representation of the Board this turn, after being updated.
% +SetLine:			Line of the cell where the player placed his piece this turn.
% +SetColumn:		Column of the cell where the player placed his piece this turn.
% +CurrSequenceNo: 	Maximum sequence of pieces in a row the player has before this turn.
% -NewSequenceNo: 	Maximum sequence of pieces in a row the player has after this turn.
updateSequence(Piece, Board, SetLine, SetColumn, CurrSequenceNo, NewSequenceNo) :- 	getSequence(Piece, Board, SetLine, SetColumn, CalcSequenceNo), 
																					max(NewSequenceNo, [CalcSequenceNo, CurrSequenceNo]).

																				
% Calculates the Sequence Number from the player's latest move.

% Must be called after the board is updated with the player's new piece

% +Piece: 			Model ID of the player's piece.
% +Board:			Internal Representation of the Board this turn, after being updated.
% +SetLine:			Line of the cell where the player placed his piece this turn.
% +SetColumn:		Column of the cell where the player placed his piece this turn.
% -CalcSequenceNo: 	Maximum sequence of pieces in a row from this turn's move.
getSequence(Piece, Board, SetLine, SetColumn, MaxSequenceNo) :-	getSequence(horizontal,  Piece, Board, SetLine, SetColumn, HorizontalSequenceNo),
																getSequence(vertical, 	 Piece, Board, SetLine, SetColumn, VerticalSequenceNo),
																getSequence(posDiagonal, Piece, Board, SetLine, SetColumn, PosDiagonalSequenceNo),
																getSequence(negDiagonal, Piece, Board, SetLine, SetColumn, NegDiagonalSequenceNo),
																max(MaxSequenceNo, [HorizontalSequenceNo, VerticalSequenceNo, PosDiagonalSequenceNo, NegDiagonalSequenceNo]).
																
max(Max, [H|T]) :- max(Max, H, T).

max(Max, Max, []).
max(Max, CurrMax, [H|T]) :- H > CurrMax, !,
							max(Max, H, T).
max(Max, CurrMax, [_|T]) :- max(Max, CurrMax, T).
							
																
getSequence(Direction, Piece, Board, SetLine, SetColumn, MaxSequenceNo) :-	direction(Direction, LineInc, ColInc),
																			LineDec is -LineInc, ColDec is -ColInc,
																			sequence(Piece, Board, SetLine, SetColumn, LineDec, ColDec, -1,  LeftSequenceNo), % Starts in -1 to compensate duplicate counting of the start cell.
																			sequence(Piece, Board, SetLine, SetColumn, LineInc, ColInc, LeftSequenceNo, MaxSequenceNo).
																		
% Calculates the Sequence Number in a direction (including the starting cell) accumulated with an initial value.

% +Piece: 			Model ID of the player's piece.
% +Board:			Internal Representation of the Board this turn, after being updated.
% +SetLine:			Line of the cell being processed first.
% +SetColumn:		Column of the cell being processed first.
% +LineInc:			Line value to be incremented each step of the sequence
% +ColInc:			Column value to be incremented each step of the sequence
% +InitialValue:	Initial sequence value from previous accumulations
% -SequenceNo: 		Sequence number of pieces in a row from the given cell and direction.																	
sequence(Piece, Board, SetLine, SetColumn, LineInc, ColInc, InitialValue, SequenceNo) :- 	getPiece(SetLine, SetColumn, Board, Piece),
																							NewLine is SetLine + LineInc,
																							NewColumn is SetColumn + ColInc,
																							NewValue is InitialValue + 1,
																							sequence(Piece, Board, NewLine, NewColumn, LineInc, ColInc, NewValue, SequenceNo).
sequence(_, _, _, _, _, _, SequenceNo, SequenceNo).


% Updates the Sapture Number of the player whose turn is being processed.

% Must be called after the board is updated with the current player's new piece

% +CurrPiece: 		Model ID of the current turn's player piece.
% +NextPiece: 		Model ID of the current player's opponent piece.
% +Board:			Internal Representation of the Board this turn, after being updated.
% +SetLine:			Line of the cell where the player placed his piece this turn.
% +SetColumn:		Column of the cell where the player placed his piece this turn.
% +CurrCaptureNo: 	Number of captures the current player has before this turn.
% -NewCaptureNo: 	Number of captures the current player has after this turn.
updateCaptures(CurrPiece, NextPiece, Board, SetLine, SetColumn, CurrCaptureNo, NewCaptureNo) :- getCaptures(horizontal,  CurrPiece, NextPiece, Board, SetLine, SetColumn, HorizontalCaptureNo),
																								getCaptures(vertical, 	 CurrPiece, NextPiece, Board, SetLine, SetColumn, VerticalCaptureNo),
																								getCaptures(posDiagonal, CurrPiece, NextPiece, Board, SetLine, SetColumn, PosDiagonalCaptureNo),
																								getCaptures(negDiagonal, CurrPiece, NextPiece, Board, SetLine, SetColumn, NegDiagonalCaptureNo),
																								!,
																								NewCaptureNo is CurrCaptureNo + HorizontalCaptureNo + VerticalCaptureNo + PosDiagonalCaptureNo + NegDiagonalCaptureNo.

% Calculates the Capture Number in a given direction from a given cell.

% +Direction 	Internal Representation of the direction to be evaluated
% +CurrPiece: 	Model ID of the current turn's player piece.
% +NextPiece: 	Model ID of the current player's opponent piece.
% +Board:		Internal Representation of the Board this turn, after being updated.
% +SetLine:		Line of the cell where captures are being evaluated (Latest cell to be played by current player).
% +SetColumn:	Column of the cell where captures are being evaluated (Latest cell to be played by current player).
% -CaptureNo: 	Number of captures from the given cell and direction.																											
getCaptures(Direction, CurrPiece, NextPiece, Board, SetLine, SetColumn, CaptureNo) :-	direction(Direction, LineInc, ColInc),
																						LineDec is -LineInc, ColDec is -ColInc,																						
																						capture(CurrPiece, NextPiece, Board, SetLine, SetColumn, LineDec, ColDec, LeftCaptureNo), 	% LeftCaptureNo is either 1 or 0.
																						capture(CurrPiece, NextPiece, Board, SetLine, SetColumn, LineInc, ColInc, RightCaptureNo), 	% LeftCaptureNo is either 1 or 0.
																						!,
																						CaptureNo is LeftCaptureNo + RightCaptureNo.

% Checks if there was a capture in a given way from a given cell.

% +CurrPiece: 	Model ID of the current turn's player piece.
% +NextPiece: 	Model ID of the current player's opponent piece.
% +Board:		Internal Representation of the Board this turn, after being updated.
% +Line1:		Line of the the first cell of the capture being evaluated (Corresponds to the latest cell to be played by current player).
% +Column1:		Column of the the first cell of the capture being evaluated (Corresponds to the latest cell to be played by current player).
% -CaptureNo: 	Numeric alue stating if there is a capture (1) or not (0).
capture(CurrPiece, NextPiece, Board, Line1, Column1, LineInc, ColInc, CaptureNo) :- Line2 is Line1 + LineInc, Column2 is Column1 + ColInc,
																					Line3 is Line2 + LineInc, Column3 is Column2 + ColInc,
																					Line4 is Line3 + LineInc, Column4 is Column3 + ColInc,
																					getPiece(Line1, Column1, Board, CurrPiece),
																					getPiece(Line2, Column2, Board, NextPiece),
																					getPiece(Line3, Column3, Board, NextPiece),
																					getPiece(Line4, Column4, Board, CurrPiece),
																					!,
																					CaptureNo = 1.
capture(_, _, _, _, _, _, _, 0).