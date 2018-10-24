% ----BOARD MANIPULATION----
% API: getPiece() and setPiece()
% Auxiliary Predicates: getLine(), getColumn(), setLine() and setColumn()
%
% Initial State of Board:
board([ ['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
		['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
		['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
		['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
		['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
		['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
		['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
		['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
		['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
		['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
		['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
		['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
		['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
		['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
		['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
		['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
		['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
		['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
		['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#']
	  ]).

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

displayGame(Board):- format("~n   A  B  C  D  E  F  G  H  I  J  K  L  M  N  O  P  Q  R  S~n~n", []), displayGame(Board, 19).
displayGame([H], LineNumber):-displayLine(H, LineNumber), format("~n   A  B  C  D  E  F  G  H  I  J  K  L  M  N  O  P  Q  R  S~n~n", []).												
displayGame([H|T], LineNumber):-	displayLine(H, LineNumber),
									displaySepLine,
									NextLineNumber is LineNumber - 1,
									displayGame(T, NextLineNumber).	

displaySepLine:- format("   |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |~n", []).
displayLine(BoardLine, LineNumber):- LineNumber < 10, format(" ~p ", [LineNumber]) ,format("~p--~p--~p--~p--~p--~p--~p--~p--~p--~p--~p--~p--~p--~p--~p--~p--~p--~p--~p", BoardLine), format(" ~p~n", [LineNumber]).
displayLine(BoardLine, LineNumber):- LineNumber > 9, format("~p ", [LineNumber]) ,format("~p--~p--~p--~p--~p--~p--~p--~p--~p--~p--~p--~p--~p--~p--~p--~p--~p--~p--~p", BoardLine), format(" ~p~n", [LineNumber]). 
