board([ ['_', '_', '_', '_', '_', '_', '_', '_', '_'],
		['_', '_', '_', '_', '_', '_', '_', '_', '_'],
		['_', '_', '_', '_', '_', '_', '_', '_', '_'],
		['_', '_', '_', '_', '_', '_', '_', '_', '_'],
		['_', '_', '_', '_', '_', '_', '_', '_', '_'],
		['_', '_', '_', '_', '_', '_', '_', '_', '_'],
		['_', '_', '_', '_', '_', '_', '_', '_', '_'],
		['_', '_', '_', '_', '_', '_', '_', '_', '_'],
		['_', '_', '_', '_', '_', '_', '_', '_', '_']
	  ]).

getPiece(LinNo, ColNo, Board, Piece) :- getLine(LinNo, Board, Line), getColumn(Line, ColNo, Piece).

getLine(1, [Line|_], Line).
getLine(LinNo, [_|RestBoard], Line) :- 	LinNo > 1, 
											PrevLineNo is LinNo - 1, 
											getLine(PrevLineNo, RestBoard, Line).

getColumn([Piece|_], 1, Piece).
getColumn([_|RestLine], ColNo, Piece) :- 	ColNo > 1, 
											PrevColNo is ColNo - 1, 
											getColumn(RestLine, PrevColNo, Piece). 
											

setPiece(LinNo, ColNo, OldBoard, NewBoard, Piece) :- 	getLine(LinNo, OldBoard, OldLine), 
														setColumn(ColNo, Piece, OldLine, NewLine),
														setLine(LinNo, OldBoard, NewBoard, NewLine).

setColumn(1, Piece, [_|T], [Piece|T]).
setColumn(ColNo, Piece, [H|T], [H|R]) :- 	ColNo > 1, 
											PrevColNo is ColNo - 1,
											setColumn(PrevColNo, Piece, T, R).
							
setLine(1, [_|T], [NewLine|T], NewLine).					
setLine(LinNo, [H|T], [H|R], NewLine) :- 	LinNo > 1,
											PrevLinNo is LinNo - 1,
											setLine(PrevLinNo, T, R, NewLine).