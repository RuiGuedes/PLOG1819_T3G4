% ---- BOARD MANIPULATION ----
% API: getPiece() and setPiece()
% Auxiliary Predicates: getLine(), getColumn(), setLine() and setColumn()
%
% Initial State of Board:
board([ ['0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0'],
		['0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0'],
		['0', '0', '1', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0'],
		['0', '0', '0', '1', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0'],
		['0', '0', '0', '0', '1', '1', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0'],
		['0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0'],
		['0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0'],
		['0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0'],
		['0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0'],
		['0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0'],
		['0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0'],
		['0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0'],
		['0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0'],
		['0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0'],
		['0', '0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0'],
		['0', '0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0'],
		['0', '0', '1', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0'],
		['0', '1', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '1', '1', '1', '1', '0'],
		['0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0']
	  ]).

% Symbol converter
modelToView('0','+').
modelToView('1','O').
modelToView('2','X').
modelToView('3','#').

% Retrieves a Cell from a given board.

%	+LinNo: Row number for the cell to be retrieved.
%  	+ColNo: Row number for the cell to be retrieved.
%	+Board: Internal Representation of the board.
%	-Piece: Cell value retrieved from the board
getPiece(LinNo, ColNo, Board, Piece):- 
	LinNo >= 1, LinNo =< 19,
	ColNo >= 1, ColNo =< 19,
	getLine(LinNo, Board, Line), 
	getColumn(ColNo, Line, Piece).
	
getPiece(_, _, _, Piece):- modelToView(Piece, '#').

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
											
displayGame(Board):- length(Board, Size), displayColSymb(Size), displayGame(Board, Size), displayColSymb(Size).
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
										
displayPlayerInfo('playerOne', P1, P2):- 	format('   Player One -> [~p] Captures    ', P1), 
											format('   Player Two -> [~p] Captures~n~n', P2).
displayPlayerInfo('playerTwo', P2, P1):- 	format('   Player One -> [~p] Captures    ', P1), 
											format('   Player Two -> [~p] Captures~n~n', P2).
											

endGame(Board, Num):- 	displayGame(Board),
						modelToView(Num, Symb),
						format('-> Victory: Player ~p was won the game by having a sequence of five <~p> pieces !~n~n', [Num, Symb]).
endGame(Board, Num, 10):- 	displayGame(Board),
							format('-> Victory: Player ~p was won the game by making 10 captures !~n~n', [Num]).																			

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% End - DISPLAY %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%										

% Column identifiers
columnSymb(['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z']).

% Struct info: player - player symbol - number of captures
player('playerOne', '1', _).  
player('playerTwo', '2', _).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Game State Transactions %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

startGame:- board(Board), 
			player('playerOne', P1_Num, _),
			player('playerTwo', P2_Num, _),
			gameStep(Board, player('playerOne', P1_Num, 9), player('playerTwo', P2_Num, 0)).

gameStep(Board, player(CurrPlayer, Curr_Num, Curr_Capt), player(NextPlayer, Next_Num, Next_Capt)):- 	displayGame(Board),	displayPlayerInfo(CurrPlayer, Curr_Capt, Next_Capt),
																										handleInput(Board, CurrPlayer, Line, Column),
																										updateBoard(CurrPlayer, Line, Column, Board, NewBoard), !,											
																										gameTransitionState(player(CurrPlayer, Curr_Num, Curr_Capt), player(NextPlayer, Next_Num, Next_Capt), Line, Column, NewBoard).

% Updates board Status
updateBoard(Player, Line, Column, Board, NewBoard):-	player(Player, Piece),
														setPiece(Line, Column, Board, NewBoard, Piece).

														
% Check possible game state transations: Victory or Captures																										
gameTransitionState(player(CurrPlayer, Curr_Num, _), _, Line, Column, NewBoard):- 	updateGameState(CurrPlayer, Line, Column, NewBoard), endGame(NewBoard, Curr_Num).
gameTransitionState(player(CurrPlayer, Curr_Num, Curr_Capt), player(NextPlayer, Next_Num, Next_Capt), Line, Column, NewBoard):- 	verifyCaptures(CurrPlayer, Line, Column, NewBoard, Num_Capt), 
																																	updatePlayerCaptures(player(CurrPlayer, Curr_Num, Curr_Capt), player(NewPlayer, New_Num, New_Num_Capt), Num_Capt),
																																	checkPlayerCaptures(NewBoard, player(NewPlayer, New_Num, New_Num_Capt), player(NextPlayer, Next_Num, Next_Capt)).
% After 10 captures the current player is considered the winner
checkPlayerCaptures(NewBoard, player(_, Curr_Num, 10), _):- endGame(NewBoard, Curr_Num, 10).
checkPlayerCaptures(NewBoard, player(CurrPlayer, Curr_Num, Curr_Capt), player(NextPlayer, Next_Num, Next_Capt)):- gameStep(NewBoard, player(NextPlayer, Next_Num, Next_Capt), player(CurrPlayer, Curr_Num, Curr_Capt)).							
			

%%%%%%%%%%%%%%%%%%
%% Handle input %%
%%%%%%%%%%%%%%%%%%

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
validateVictory(Type, Player, Line, Column, Board, Iterator, [H|T], NumPieces):- 	player(Player, Symb),
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
																				
% Retrieves a list of 5 pieces in the direction provided  of a certain piece	

% Pieces on horizontal direction																		
retrievePieces(_, 4, Line, Column, Board, OldPieces, NewPieces):-	getPiece(Line, Column, Board, Piece),
																			append([Piece], OldPieces, NewPieces).
retrievePieces('horizontal', Iterator, Line, Column, Board, OldPieces, NewPieces):- 	getPiece(Line, Column, Board, Piece),
																						append([Piece], OldPieces, Pieces),
																						NewColumn is Column + 1,
																						NewIterator is Iterator + 1,	
																						retrievePieces('horizontal', NewIterator, Line, NewColumn, Board, Pieces, NewPieces).
																						
% Pieces on vertical direction
retrievePieces('vertical', Iterator, Line, Column, Board, OldPieces, NewPieces):- 	getPiece(Line, Column, Board, Piece),
																					append([Piece], OldPieces, Pieces),
																					NewLine is Line + 1,
																					NewIterator is Iterator + 1,	
																					retrievePieces('vertical', NewIterator, NewLine, Column, Board, Pieces, NewPieces).	

% Pieces on negative diagonal direction																																												
retrievePieces('negativeDiagonal', Iterator, Line, Column, Board, OldPieces, NewPieces):- 	getPiece(Line, Column, Board, Piece),
																							append([Piece], OldPieces, Pieces),
																							NewLine is Line + 1,
																							NewColumn is Column - 1,
																							NewIterator is Iterator + 1,
																							retrievePieces('negativeDiagonal', NewIterator, NewLine, NewColumn, Board, Pieces, NewPieces).		
																							
% Pieces on positive diagonal direction																																												
retrievePieces('positiveDiagonal', Iterator, Line, Column, Board, OldPieces, NewPieces):- 	getPiece(Line, Column, Board, Piece), 
																							append([Piece], OldPieces, Pieces),
																							NewLine is Line + 1,
																							NewColumn is Column + 1,
																							NewIterator is Iterator + 1,
																							retrievePieces('positiveDiagonal', NewIterator, NewLine, NewColumn, Board, Pieces, NewPieces).		

%%%%%%%%%%%%%%%%%%%%%
%% Update Captures %%
%%%%%%%%%%%%%%%%%%%%%

verifyCaptures(_, _, _, _, Num_Capt):- Num_Capt is 0.
updatePlayerCaptures(player(CurrPlayer, Curr_Num, Curr_Capt), player(CurrPlayer, Curr_Num, New_Num_Capt), Num_Capt):- New_Num_Capt is Curr_Capt + Num_Capt .																							
																																														