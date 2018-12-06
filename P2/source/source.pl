%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Includes %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(clpfd)).
:- use_module(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Utility Predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API: 
% Auxiliary Predicates: 

% Converts a list of lists in one single list
flatten(List, FlatList):-	flatten(List, [], FlatList0),
							!,
							FlatList = FlatList0.
flatten(Var, Tl, [Var|Tl]):-	var(Var), !.
flatten([], Tl, Tl) :- !.
flatten([Hd|Tl], Tail, List) :- !,
								flatten(Hd, FlatHeadTail, List),
								flatten(Tl, Tail, FlatHeadTail).
								flatten(NonList, Tl, [NonList|Tl]).
							
% Builds a board containing only variables 
build_variable_board(0, _, []).								
build_variable_board(LineSize, ColSize, [Row|T]):- 	length(Row, ColSize),
													NewLineSize is LineSize - 1,
													build_variable_board(NewLineSize, ColSize, T).

% Check if element is whithin the board boundaries													
check_boundaries(LineSize, ColSize, LineNo, ColNo):- 	LineNo >= 0, LineNo < LineSize, 
														ColNo >= 0, ColNo < ColSize, !.							

% Retrieves element from a certain position on the board
get_element(Board, LineNo, ColNo, Element):- 	NewColNo is ColNo + 1,
												nth0(LineNo, Board, Line), 												
												element(NewColNo, Line, Element), !.								

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Board Representation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% After solving a fixed puzzle, refator this predicate to be dynamic
initial_board([	[2, 6, 0, 3, 2, 5, 0],
				[2, 2, 4, 5, 2, 2, 0],
				[5, 0, 4, 3, 6, 2, 4],
				[7, 2, 4, 0, 3, 0, 2],
				[7, 5, 3, 5, 1, 2, 2],
				[2, 4, 2, 2, 2, 2, 6],
				[5, 4, 3, 0, 5, 2, 4],
				[23, 20, 26, 20, 26, 19, 22]
			]).

% Symbol        - Internal - External - Constraints        
% Empty         - 	 0     -    EMP   - No constraints
% Star       	- 	 1     -    STA   - Must be prime, at least 2, have no neighbors orthogonally that are prime or are 1
% Square      	- 	 2     -    SQU   - Must be either 0 or 5 but not have the same digit as a neighbor unless the neighbor is a diamond
% Diamond     	-  	 3     -    DIA   - Is odd and is the sum of all digits left of it in the row
% Triangle    	- 	 4     -    TRI   - Located directly below an even digit & less than it (but not 0)
% Cirle       	- 	 5     -    CIR   - Not a multiple of 3, and all copies are the same digit within the specific grid
% Chess Knight 	- 	 6     -    CHK   - Chess knight - tells amount of even digits (incl. 0) in its attack range
% Hearth      	-  	 7     -    HRT   - Neighboring hearts must add together to a sum of 10

% Symbol converter
model_to_view(0,'EMP').
model_to_view(1,'STA').
model_to_view(2,'SQU').
model_to_view(3,'DIA').
model_to_view(4,'TRI').
model_to_view(5,'CIR').
model_to_view(6,'CHK').
model_to_view(7,'HRT').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  Puzzle Display %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API: 
% Auxiliary Predicates: 

% +Board: Internal Representation of the board to be displayed.								
display_puzzle(Board, State):-	Board = [FirstLine | _],
								length(Board, LineSize),
								length(FirstLine, ColSize),
								nl, display_sep_line(ColSize, State),
								display_puzzle(Board, LineSize, ColSize, LineSize, Board, State).

display_puzzle(_, 1, _, _, _, _).
display_puzzle([H], LineNumber, _, LineSize, Board, State):-	display_line(H, LineNumber, LineSize, Board, State).											
display_puzzle([H|T], LineNumber, ColSize, LineSize, Board, State):-	display_line(H, LineNumber, LineSize, Board, State),										
																		display_sep_line(ColSize, State),
																		NextLineNumber is LineNumber - 1,
																		display_puzzle(T, NextLineNumber, ColSize, LineSize, Board, State).
									
% Display a Row of the board with its identifier, allowing the player to easily identify cells

% +BoardLine:	List representation of the Line to be displayed
display_line(BoardLine, LineNumber, LineSize, Board, State):- 	write('|'),
																display_row(BoardLine, State), 
																ReverseLineNumber is LineSize - LineNumber,
																nth1(LineSize, Board, LastLine),
																nth0(ReverseLineNumber, LastLine, Sum),
																write('| '), write(Sum), nl.

% Display a Row of the board
disp_row('initial', H):- model_to_view(H,Symb), format(' ~p ' , [Symb]).
disp_row('final', H):- format(' [~p] ', [H]).

% +BoardLine: List representation of the Line to be displayed
display_row([H|[]], State):- disp_row(State, H).
display_row([H|T], State):-  disp_row(State, H),
						 	 write('|'), 
							 display_row(T, State).															

							
						
% Displays a Line of characters between two different rows, providing a better visual experience

% +Size - Number of columns in the board to be displayed
sep_line('initial'):- write('|-----').				
sep_line('final'):-	write('|-----').	
initial_sep_line('initial'):- write('|-----').
initial_sep_line('final'):- write('|-----').

display_sep_line(Size, State):- initial_sep_line(State), 
								disp_sep_line(Size, State).						

disp_sep_line(1, _):- 		write('|\n').
disp_sep_line(Size, State):- 	sep_line(State),
								DecSize is Size - 1,
								disp_sep_line(DecSize, State).	
						
						
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Puzzle Solution %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve_puzzle:- 	initial_board(BoardInfo),
				BoardInfo = [FirstLine | _],
				length(BoardInfo, TmpLineSize), LineSize is TmpLineSize - 1, 
				length(FirstLine, ColSize),
				
				build_variable_board(LineSize, ColSize, Board),
				flatten(Board, Vars),
				
				nth1(TmpLineSize, BoardInfo, LastLine),
				
				apply_general_constraints(Vars, Board, BoardInfo, LineSize, ColSize, LastLine),							
				
				append(Board, [LastLine], TMP), !,
				display_puzzle(BoardInfo, 'initial'), nl,
				display_puzzle(TMP, 'final'), nl.
				
																
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Constraints %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

apply_general_constraints(Vars, Board, BoardInfo, LineSize, ColSize, LastLine):- 	domain(Vars, 0, 9),
																					apply_row_constraints(Board, LineSize, LastLine, 0),
																					apply_board_constraints(Board, BoardInfo, 0, LineSize, ColSize),
																					labeling([], Vars).
																		
% Applys row row contraint by making sure that the sum of all elements in each row is equal to a certain value
apply_row_constraints([], LastLine, _, LastLine).
apply_row_constraints([H|T], LineSize, LastLine, CurrLine):- 	nth0(CurrLine, LastLine, LineSum),																
																sum(H, #=, LineSum),
																NewCurrLine is CurrLine + 1,
																apply_row_constraints(T, LineSize, LastLine, NewCurrLine).
																

apply_board_constraints(_, _, LineSize, LineSize, _).
apply_board_constraints(Board, BoardInfo, CurrLine, LineSize, ColSize):- 	nth0(CurrLine, Board, Line), nth0(CurrLine, BoardInfo, LineInfo),
																			apply_line_constraints(Board, BoardInfo, Line, LineInfo, CurrLine, 0, ColSize),
																			NextLine is CurrLine + 1,
																			apply_board_constraints(Board, BoardInfo, NextLine, LineSize, ColSize).

apply_line_constraints(_, _, _, _, _, ColSize, ColSize):- nl.
apply_line_constraints(Board, BoardInfo, Line, LineInfo, CurrLine, CurrCol, ColSize):- 	NextCol is CurrCol + 1,
																						element(NextCol, Line, Var),
																						nth0(CurrCol, LineInfo, VarInfo),
																						apply_element_constraint(VarInfo, Var, Board, BoardInfo, CurrLine, CurrCol), 
																						apply_line_constraints(Board, BoardInfo, Line, LineInfo, CurrLine, NextCol, ColSize).

																

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Element Constraints %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%																
% API: 
% Auxiliary Predicates: 
																
% Neighbors clockwise access - neighbor(Direction, LineInc, ColInc)																
neighbor(0, -1,  0). % Up
neighbor(1, -1,  1). % Up-Right
neighbor(2,  0,  1). % Right
neighbor(3,  1,  1). % Down-Right
neighbor(4,  1,  0). % Down
neighbor(5,  1, -1). % Down-Left
neighbor(6,  0, -1). % Left
neighbor(7, -1, -1). % Up-Left
																														
																
% Empty
apply_element_constraint(0, Var, Board, BoardInfo, LineNo, ColNo).

% Star
apply_element_constraint(1, Var, Board, _, LineNo, ColNo):- Var in {2, 3, 5, 7},
															Board = [Line | _], length(Board, LineSize), length(Line, ColSize),
															apply_star_constraints(Board, LineSize, ColSize, LineNo, ColNo, 0).																													
																	
% Square
apply_element_constraint(2, Var, Board, BoardInfo, LineNo, ColNo):- Var in {0, 5},
																	Board = [Line | _], length(Board, LineSize), length(Line, ColSize),
																	apply_square_constraints(Board, BoardInfo, Var, LineSize, ColSize, LineNo, ColNo, 0).												

% Diamond
apply_element_constraint(3, Var, Board, _, LineNo, ColNo):- Var in {1, 3, 5, 7, 9},
															apply_diamond_constraints(Board, Var, LineNo, ColNo).

% Triangle
apply_element_constraint(4, Var, Board, _, LineNo, ColNo):-	neighbor(0, LineInc, ColInc),
															NewLineNo is LineNo + LineInc,
															NewColNo is ColNo + ColInc,
															apply_triangle_constraints(Board, Var, NewLineNo, NewColNo).

% Cirle - Not a multiple of 3, and all copies are the same digit within the specific grid
apply_element_constraint(5, Var, Board, BoardInfo, _, _):- 	Var #\= 0, Var #\= 3, Var #\= 6, Var #\= 9,
															flatten(Board, BoardFlatten), flatten(BoardInfo, BoardInfoFlatten),
															apply_circle_constraint(BoardFlatten, BoardInfoFlatten, Var, 5).
																	
% Chess Knight
apply_element_constraint(6, Var, Board, BoardInfo, LineNo, ColNo).

% Hearth
apply_element_constraint(7, Var, Board, BoardInfo, LineNo, ColNo).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Star Constraints %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API: 
% Auxiliary Predicates: 
% Constraints info:
% -> Var is equal to 2, 3, 5 or 7
% -> Orthogonal elements != 1, 2, 3, 5, 7
									
apply_star_constraints(_, _, _, _, _, 8).															
apply_star_constraints(Board, LineSize, ColSize, LineNo, ColNo, Direction):- 	neighbor(Direction, LineInc, ColInc),
																				NewLineNo is LineNo + LineInc,
																				NewColNo is ColNo + ColInc, 
																				apply_star_constraint(Board, LineSize, ColSize, NewLineNo, NewColNo),
																				NewDirection is Direction + 2, % Checking only orthogonal elements
																				apply_star_constraints(Board, LineSize, ColSize, LineNo, ColNo, NewDirection).
											
																	
apply_star_constraint(Board, LineSize, ColSize, LineNo, ColNo):-	check_boundaries(LineSize, ColSize, LineNo, ColNo),
																	get_element(Board, LineNo, ColNo, Neighbor),
																	Neighbor #\= 1, Neighbor #\= 2, Neighbor #\= 3, Neighbor #\= 5, Neighbor #\= 7.																	
apply_star_constraint(_, _, _, _, _, _).	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Square Constraints %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API: 
% Auxiliary Predicates: 
% Constraints info:
% -> Var is equal to 0 or 5
% -> Var is different from neighbors unless neighbor is diamond

apply_square_constraints(_, _, _, _, _, _, _, 8).
apply_square_constraints(Board, BoardInfo, Var, LineSize, ColSize, LineNo, ColNo, Direction):- 	neighbor(Direction, LineInc, ColInc),
																								NewLineNo is LineNo + LineInc,
																								NewColNo is ColNo + ColInc, !,
																								apply_square_constraint(Board, BoardInfo, Var, LineSize, ColSize, NewLineNo, NewColNo),
																								NewDirection is Direction + 2, % Checking every neighbor. Neighbors should be every direction 2 -> 1
																								apply_square_constraints(Board, BoardInfo, Var, LineSize, ColSize, LineNo, ColNo, NewDirection).
																	
apply_square_constraint(Board, BoardInfo, Var, LineSize, ColSize, LineNo, ColNo):- 	check_boundaries(LineSize, ColSize, LineNo, ColNo),
																					get_element(Board, LineNo, ColNo, Neighbor),
																					get_element(BoardInfo, LineNo, ColNo, NeighborInfo),
																					NeighborInfo \= 3, % Neighbor is not a diamond																																										
																					!,
																					Var #\= Neighbor .			
apply_square_constraint(_, _, _, _, _, _, _).	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Diamond Constraints %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API: 
% Auxiliary Predicates: 

apply_diamond_constraints(Board, Var, LineNo, ColNo):-	nth0(LineNo, Board, Line),
														get_left_elements(Line, 0, ColNo, LeftElements),
														sum(LeftElements, #=, Var).
														
get_left_elements(_, ColNo, ColNo, []).					
get_left_elements([H|T], CurrCol, ColNo, [H|Rest]):-	NewCurrCol is CurrCol + 1,	
														get_left_elements(T, NewCurrCol, ColNo, Rest).									
														

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Triangle Constraints %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API: 
% Auxiliary Predicates: 

apply_triangle_constraints(Board, Var, NewLineNo, NewColNo):- 	get_element(Board, NewLineNo, NewColNo, Neighbor), !,
																Neighbor in {0, 2, 4, 6, 8},
																Var #\= 0, Var #< Neighbor.
apply_triangle_constraints(_, Var, _, _):- 	Var #\= 0.																
																
																

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Circle Constraints %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API: 
% Auxiliary Predicates: 
% Constraints info:
% -> Var is different from 0, 3, 6 and 9
% -> All circle vars have the same value

% Predicate responsible to ensure that all circles share the same value																	
apply_circle_constraint([], _, _, _).
apply_circle_constraint([H1|T1], [H2|T2], Var, H2):- 	Var #= H1,
														apply_circle_constraint(T1, T2, Var, H2).
apply_circle_constraint([_|T1], [H2|T2], Var, Type):- 	Type \= H2,
														apply_circle_constraint(T1, T2, Var, Type).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Chess Knight Constraints %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API: 
% Auxiliary Predicates: 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Hearth Constraints %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API: 
% Auxiliary Predicates: 




























































						