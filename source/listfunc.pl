board([ ['#', '#', '#', '#', '#', '#', '#', '#', '#','#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
		['#', '#', '#', '#', '#', '#', '#', '#', '#','#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
		['#', '#', '#', '#', '#', '#', '#', '#', '#','#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
		['#', '#', '#', '#', '#', '#', '#', '#', '#','#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
		['#', '#', '#', '#', '#', '#', '#', '#', '#','#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
		['#', '#', '#', '#', '#', '#', '#', '#', '#','#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
		['#', '#', '#', '#', '#', '#', '#', '#', '#','#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
		['#', '#', '#', '#', '#', '#', '#', '#', '#','#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
		['#', '#', '#', '#', '#', '#', '#', '#', '#','#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
		['#', '#', '#', '#', '#', '#', '#', '#', '#','#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
		['#', '#', '#', '#', '#', '#', '#', '#', '#','#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
		['#', '#', '#', '#', '#', '#', '#', '#', '#','#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
		['#', '#', '#', '#', '#', '#', '#', '#', '#','#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
		['#', '#', '#', '#', '#', '#', '#', '#', '#','#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
		['#', '#', '#', '#', '#', '#', '#', '#', '#','#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
		['#', '#', '#', '#', '#', '#', '#', '#', '#','#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
		['#', '#', '#', '#', '#', '#', '#', '#', '#','#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
		['#', '#', '#', '#', '#', '#', '#', '#', '#','#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
		['#', '#', '#', '#', '#', '#', '#', '#', '#','#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
		['#', '#', '#', '#', '#', '#', '#', '#', '#','#', '#', '#', '#', '#', '#', '#', '#', '#', '#']
	  ]).

getPiece(LinNo, ColNo, Board, Piece):-	getLine(LinNo, Board, Line), 
									   	getColumn(Line, ColNo, Piece).

getLine(0, [Line|_], Line).
getLine(LinNo, [_|RestBoard], Line):- 	LinNo > 0, 
									  	PrevLineNo is LinNo - 1, 
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

							
setLine(0, [_|T], [NewLine|T], NewLine).					
setLine(LinNo, [H|T], [H|R], NewLine) :-	LinNo > 0,
											PrevLinNo is LinNo - 1,
											setLine(PrevLinNo, T, R, NewLine).

displayGame(Board):- format("~n", []), displayGame(Board, 0).
displayGame([H], LineNumber):-displayLine(H, LineNumber), format("   A  B  C  D  E  F  G  H  I  J  K  L  M  N  O  P  Q  R  S~n~n", []).												
displayGame([H|T], LineNumber):-	displayLine(H, LineNumber),
									displaySepLine,
									NextLineNumber is LineNumber + 1,
									displayGame(T, NextLineNumber).	

displaySepLine:- format("   |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |~n", []).
displayLine(BoardLine, LineNumber):- LineNumber < 10, format("~p  ", [LineNumber]) ,format("~p--~p--~p--~p--~p--~p--~p--~p--~p--~p--~p--~p--~p--~p--~p--~p--~p--~p--~p~n", BoardLine).
displayLine(BoardLine, LineNumber):- LineNumber > 9, format("~p ", [LineNumber]) ,format("~p--~p--~p--~p--~p--~p--~p--~p--~p--~p--~p--~p--~p--~p--~p--~p--~p--~p--~p~n", BoardLine).
