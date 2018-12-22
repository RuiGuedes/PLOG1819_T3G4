%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Includes %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(random)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Utility Predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API: flatten/2, build_variable_puzzle/3, check_boundaries/4, get_element/4, reset_timer/0

% Converts a list of lists in one single list

% +List:	 List of lists
% -FlatList: Single list with all variables
flatten(List, FlatList):-	flatten(List, [], FlatList0),
							!,
							FlatList = FlatList0.
flatten(Var, Tl, [Var|Tl]):-	var(Var), !.
flatten([], Tl, Tl) :- !.
flatten([Hd|Tl], Tail, List) :- !,
								flatten(Hd, FlatHeadTail, List),
								flatten(Tl, Tail, FlatHeadTail).
								flatten(NonList, Tl, [NonList|Tl]).
							
% Builds a puzzle containing only variables 

% +LineSize: Number of lists to be created
% +ColSize:	 Size of every list to be created
% -List:	 New created list
build_variable_puzzle(0, _, []).								
build_variable_puzzle(LineSize, ColSize, [Row|T]):- length(Row, ColSize),
													NewLineSize is LineSize - 1,
													build_variable_puzzle(NewLineSize, ColSize, T).

% Check if element is whithin the puzzle boundaries

% +LineSize: Max line number
% +ColSize:	 Max column number
% +LineNo:	 Line number
% +ColNo:	 Column number						
check_boundaries(LineSize, ColSize, LineNo, ColNo):-	LineNo >= 0, LineNo < LineSize, 
														ColNo >= 0, ColNo < ColSize, !.							

% Retrieves element from a certain position on the puzzle

% +Puzzle:	Puzzle containing the element to be retrieved
% +LineNo:  Line number
% +ColNo:   Column number
% -Element: Element to be retrieved 
get_element(Puzzle, LineNo, ColNo, Element):- 	NewColNo is ColNo + 1,
												nth0(LineNo, Puzzle, Line), 												
												element(NewColNo, Line, Element), !.

% Retrieves a known number from a certain position and direction of generated puzzle numbers												
get_num_by_direction(Number, PuzzleNums, L, C, LI, CI, LS, CS):-	NL is LS - L + LI, NC is CS - C + CI,
																	!,
																	check_boundaries(LS, CS, NL, NC),
																	nth0(NL, PuzzleNums, PuzzleLine),
																	nth0(NC, PuzzleLine, Number),												
												
% Safer implementation of sum(+Xs, +RelOp, ?Value) that checks if Xs is empty first
safe_sum([], _, _).
safe_sum(Xs, RelOp, Value):- sum(Xs, RelOp, Value).

% Resets timer
reset_timer :- statistics(walltime,_).													

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Puzzle Representation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Puzzles to be solved
puzzle(1,[	[2, 6, 0, 3, 2, 5, 0],
			[2, 2, 4, 5, 2, 2, 0],
			[5, 0, 4, 3, 6, 2, 4],
			[7, 2, 4, 0, 3, 0, 2],
			[7, 5, 3, 5, 1, 2, 2],
			[2, 4, 2, 2, 2, 2, 6],
			[5, 4, 3, 0, 5, 2, 4],
			[23, 20, 26, 20, 26, 19, 22]
		]).
					
puzzle(2,[	[5, 0, 3, 6, 6, 5, 7],
			[0, 2, 2, 5, 7, 2, 7],
			[4, 2, 7, 3, 7, 2, 5],
			[4, 0, 7, 5, 2, 2, 7],
			[4, 6, 1, 3, 2, 5, 7],
			[4, 0, 7, 3, 2, 2, 4],
			[2, 2, 7, 3, 2, 5, 4],
			[30, 43, 24, 31, 37, 27, 28]
		]).

puzzle(3,[	[1, 0, 5, 3, 2, 2, 2],
			[0, 4, 7, 5, 2, 2, 2],
			[4, 4, 7, 3, 2, 5, 7],
			[5, 2, 2, 5, 3, 6, 7],
			[7, 2, 3, 5, 7, 7, 2],
			[7, 5, 3, 7, 4, 2, 2],
			[6, 0, 3, 7, 2, 2, 5],
			[19, 29, 21, 18, 25, 39, 17]
		]).					

% Symbol        - Internal - External - Constraints        
% Empty         - 	 0     -    EMP   - No constraints
% Star       	- 	 1     -    STA   - Must be prime, at least 2, have no neighbors orthogonally that are prime or are 1
% Square      	- 	 2     -    SQU   - Must be either 0 or 5 but not have the same digit as a neighbor unless the neighbor is a diamond
% Diamond     	-  	 3     -    DIA   - Is odd and is the sum of all digits left of it in the row
% Triangle    	- 	 4     -    TRI   - Located directly below an even digit & less than it (but not 0)
% Circle       	- 	 5     -    CIR   - Not a multiple of 3, and all copies are the same digit within the specific grid
% Chess Knight 	- 	 6     -    CHK   - Chess knight - tells amount of even digits (incl. 0) in its attack range
% Heart      	-  	 7     -    HRT   - Neighboring hearts must add together to a sum of 10

% Symbol converter
model_to_view(0,'   '). 
model_to_view(1,' * '). % *
model_to_view(2,' # '). % #
model_to_view(3,' + ').  
model_to_view(4,' & '). 
model_to_view(5,' @ '). % @
model_to_view(6,' $ '). % $
model_to_view(7,' ? '). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Display %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API: display_puzzle_info/0, display_puzzle/2, display_puzzle/6, display_line/5, display_row/2, display_sep_line/2, disp_sep_line/2, display_time/0, display_statistics/0

% Displays puzzle information: Title & Legend 
display_puzzle_info:- 	nl,
						format('#############################################################################################################~n', []),
						format('#                                                                                                           #~n', []),
						format('#    ###### ##  ## ###### ###### ###### ###   ##  ##    ###### ###### ##  ## ###### ###### ###### ######    #~n', []),
						format('#    ###### ##  ## ##  ## ##  ## ###### ###   ##  ##    ###### ##  ## ##  ## ##  ## ##  ## ###### ######    #~n', []),
						format('#    ###    ##  ## ##  ## ##  ## ###    ###   ##  ##    ###    ##  ## ##  ## ##  ## ##  ## ###    ###       #~n', []),
						format('#    ###### ###### ###### ###### ###### ###   ##  ##    ###### ##  ## ##  ## ###### ###### ###### ######    #~n', []),
						format('#    ###### ###### ###### ###    ###### ###     ##      ###### ##  ## ##  ## ###### ###### ###### ######    #~n', []),
						format('#       ### ##  ## ##  ## ###    ###    ###     ##         ### ### ## ##  ## ##  ## ## ### ###       ###    #~n', []),						
						format('#    ###### ##  ## ##  ## ###    ###### ######  ##      ###### ## ### ###### ##  ## ##  ## ###### ######    #~n', []),
						format('#    ###### ##  ## ##  ## ###    ###### ######  ##      ###### ###### ###### ##  ## ##  ## ###### ######    #~n', []),
						format('#                                                                                                           #~n', []),
						format('#############################################################################################################~n~n~n', []),
						format('Legend: Empty [~p], Star [~p], Square [~p], Diamond [~p], Triangle [~p], Circle [~p], Chess Knight [~p], Heart [~p], ', [' ', *, #, +, &, @, $, ?]),
						nl, nl.

% Display puzzle state in a friendly way

% +Puzzle: Internal Representation of the puzzle to be displayed.								
% +State:  Display state: initial or final						
display_puzzle(Puzzle, State):-	Puzzle = [FirstLine | _],
								length(Puzzle, LineSize),
								length(FirstLine, ColSize),
								nl, display_sep_line(ColSize, State),
								display_puzzle(Puzzle, LineSize, ColSize, LineSize, Puzzle, State).

% Display puzzle state in a friendly way

% +Puzzle:   Internal Representation of the puzzle to be displayed.	
% +LineNo:   Current line number
% +ColSize:  Number of columns
% +LineSize: Number of lines
% +Puzzle:   Internal Representation of the puzzle to be displayed.								
% +State:    Display state: initial or final														
display_puzzle(_, 1, _, _, _, _).
display_puzzle([H], LineNo, _, LineSize, Puzzle, State):-			display_line(H, LineNo, LineSize, Puzzle, State).											
display_puzzle([H|T], LineNo, ColSize, LineSize, Puzzle, State):-	display_line(H, LineNo, LineSize, Puzzle, State),										
																	display_sep_line(ColSize, State),
																	NextLineNumber is LineNo - 1,
																	display_puzzle(T, NextLineNumber, ColSize, LineSize, Puzzle, State).
									
% Display a puzzle row

% +PuzzleLine:	List representation of the Line to be displayed
% +LineNo:   Current line number
% +LineSize: Number of lines
% +Puzzle:   Internal Representation of the puzzle to be displayed.								
% +State:    Display state: initial or final
display_line(PuzzleLine, LineNo, LineSize, Puzzle, State):- write('|'),
															display_row(PuzzleLine, State), 
															ReverseLineNumber is LineSize - LineNo,
															nth1(LineSize, Puzzle, LastLine),
															nth0(ReverseLineNumber, LastLine, Sum),
															write('| '), write(Sum), nl.

% Display a row of the puzzle
disp_row('initial', H):- model_to_view(H,Symb), format(' ~p ' , [Symb]).
disp_row('final', H):- format('  ~p  ', [H]).

% +PuzzleLine:	List representation of the Line to be displayed
% +State:    Display state: initial or final
display_row([H|[]], State):- disp_row(State, H).
display_row([H|T], State):-  disp_row(State, H),
						 	 write('|'), 
							 display_row(T, State).																						
						
% Displays a Line of characters between two different rows, providing a better visual experience
sep_line('initial'):- write('|-----').				
sep_line('final'):-	write('|-----').	
initial_sep_line('initial'):- write('|-----').
initial_sep_line('final'):- write('|-----').

% +Size:  Number of columns in the puzzle to be displayed
% +State: Display state: initial or final
display_sep_line(Size, State):- initial_sep_line(State), 
								disp_sep_line(Size, State).						

disp_sep_line(1, _):- 			write('|\n').
disp_sep_line(Size, State):- 	sep_line(State),
								DecSize is Size - 1,
								disp_sep_line(DecSize, State).	
								
% Display time							
display_time:-	statistics(walltime,[_,T]),
				TS is ((T//10)*10)/1000,
				nl, write('Solving Time: '), write(TS), write('s'), nl, nl.
						
% Display statistics						
display_statistics:-	write('Statistics: '), nl,  nl,
						fd_statistics, nl.							

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Puzzle Generation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate_puzzle(PuzzleInfo, Size):-	generate_numbers(PuzzleNums, Size, Size, []), !,
									display_puzzle(PuzzleNums, 'final'),
									generate_elements(PuzzleNums, PuzzleInfo, PuzzleNums, Size, Size).

									
generate_numbers([RowSums], 0, _, RevRowSums):- reverse(RevRowSums, RowSums).
generate_numbers([H|T], LineSize, ColSize, Sums):-	generate_num_row(H, ColSize, 0, Sum),
													DecLineSize is LineSize - 1,
													generate_numbers(T, DecLineSize, ColSize, [Sum|Sums]).
generate_num_row([], 0, Sum, Sum).
generate_num_row([H|T], ColSize, Acc, Sum):-	random(0, 10, H),
												DecColSize is ColSize - 1,
												NewAcc is Acc + H,
												generate_num_row(T, DecColSize, NewAcc, Sum).

generate_elements([RowSums], [RowSums], _, 0, _).
generate_elements([N|NS], [E|ES], PuzzleNums, LineSize, ColSize):- 	generate_row_elements(N, E, PuzzleNums, LineSize, ColSize), !,
																	DecLineSize is LineSize - 1,
																	generate_elements(NS, ES, PuzzleNums, DecLineSize, ColSize).
									
generate_row_elements([], [], _, _, 0).
generate_row_elements([N|NS], [E|ES], PuzzleNums, LineSize, ColSize):- 	generate_element(N, E, PuzzleNums, LineSize, ColSize), !,
																		DecColSize is ColSize - 1,
																		generate_row_elements(NS, ES, PuzzleNums, LineSize, DecColSize).
																		
% Diamond Element
generate_element(N, 3, PuzzleNums, L, C):- 	member(N, [1, 3, 5, 7, 9]),
											length(PuzzleNums, IncLS),
											LI is IncLS - L,
											nth1(LI, PuzzleNums, Line),
											sublist(Line, LeftNums, 0, _, C),
											sumlist(LeftNums, N).
											
% Star Element
generate_element(N, 1, PuzzleNums, L, C):- 	member(N, [2, 3, 5, 7]),
											PuzzleNums = [Line | Rest], length(Rest, LS), length(Line, CS),
											star_validate(PuzzleNums, L, C, LS, CS, 0).
											
% Chess Element
generate_element(N, 6, PuzzleNums, L, C):- 	PuzzleNums = [Line | Rest], length(Rest, LS), length(Line, CS),
											chess_validate(N, PuzzleNums, L, C, LS, CS, 0, 0).

% Square Element
generate_element(N, 2, PuzzleNums, L, C):- 	member(N, [0, 5]),
											PuzzleNums = [Line | Rest], length(Rest, LS), length(Line, CS),
											square_validate(N, PuzzleNums, L, C, LS, CS, 0).	
																			
% Triangle Element
generate_element(N, 4, PuzzleNums, L, C):- 	N > 0,
											PuzzleNums = [Line | Rest], length(Rest, LS), length(Line, CS),
											get_num_by_direction(Above, PuzzleNums, L, C, -1, 0, LS, CS),
											0 =:= Above mod 2,
											N < Above.	
											
% Circle Element
generate_element(1, 5, _, _, _).
											
% No Element											
generate_element(_, 0, _, _, _).											
											
% Neighbor Validations
star_validate(PuzzleNums, L, C, LS, CS, Dir):-	neighbor(Dir, LI, CI),
												get_num_by_direction(Number, PuzzleNums, L, C, LI, CI, LS, CS),
												!,
												member(Number, [0, 4, 6, 8, 9]),
												NewDir is Dir + 1,
												star_validate(PuzzleNums, L, C, LS, CS, NewDir).
star_validate(_, _, _, _, _, 4):- !.												
star_validate(PuzzleNums, L, C, LS, CS, Dir):-	!, Dir < 3,
												NewDir is Dir + 1,
												star_validate(PuzzleNums, L, C, LS, CS, NewDir).
												
chess_validate(N, PuzzleNums, L, C, LS, CS, Dir, Sum):-	write(Dir), attack_range(Dir, LI, CI),
														get_num_by_direction(Number, PuzzleNums, L, C, LI, CI, LS, CS),
														member(Number, [0, 2, 4, 6, 8]),
														!,
														IncSum is Sum + 1,
														NewDir is Dir + 1,
														chess_validate(N, PuzzleNums, L, C, LS, CS, NewDir, IncSum).
chess_validate(N, _, _, _, _, _, 8, N):- !.
chess_validate(N, PuzzleNums, L, C, LS, CS, Dir, Sum):-	!, Dir < 7,
														NewDir is Dir + 1,
														chess_validate(N, PuzzleNums, L, C, LS, CS, NewDir, Sum).

square_validate(N, PuzzleNums, L, C, LS, CS, Dir):-	neighbor(Dir, LI, CI),
													get_num_by_direction(Number, PuzzleNums, L, C, LI, CI, LS, CS),
													!,
													N \= Number,
													NewDir is Dir + 1,
													square_validate(N, PuzzleNums, L, C, LS, CS, NewDir).
square_validate(_, _, _, _, _, _, 4):- !.												
square_validate(N, PuzzleNums, L, C, LS, CS, Dir):-	!, Dir < 3,
													NewDir is Dir + 1,
													square_validate(N, PuzzleNums, L, C, LS, CS, NewDir).														
											
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Puzzle Solution %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API: solve_puzzle/2, solve_puzzle/1

% Solves a certain puzzle

% +Options: Labeling options to optimize solution
% +Puzzle:  Puzzle to be solved
solve_puzzle(Options, PuzzleID):- 	display_puzzle_info, 

									% ---- Puzzle information - INIT ----						
									puzzle(PuzzleID, PuzzleInfo),
									PuzzleInfo = [FirstLine | _],
									length(PuzzleInfo, TmpLineSize), LineSize is TmpLineSize - 1,
									length(FirstLine, ColSize),
									% ---- Puzzle information - END ----
									
									solve(Options, PuzzleInfo, LineSize, ColSize, TmpLineSize).

% Solves a certain puzzle dynamically generated

% +Options:  Labeling options to optimize solution
% +LineSize: Desired number of lines for the generated puzzle
% +ColSize:  Desired number of columns for the generated puzzle
solve_gen_puzzle(Options, Size):-	display_puzzle_info,
									generate_puzzle(PuzzleInfo, Size), !,
									
									TmpLineSize is Size + 1,
									solve(Options, PuzzleInfo, Size, Size, TmpLineSize).

solve(Options, PuzzleInfo, LineSize, ColSize, TmpLineSize):- 	% ---- Variable puzzle generation - INIT ----					
																build_variable_puzzle(LineSize, ColSize, Puzzle),
																flatten(Puzzle, Vars),
																
																nth1(TmpLineSize, PuzzleInfo, LastLine),
																% ---- Variable puzzle generation - END ----
																
																display_puzzle(PuzzleInfo, 'initial'), nl,
																
																% ---- Solving puzzle - INIT ----
																reset_timer, !,																
																apply_puzzle_constraints(Options, Vars, Puzzle, PuzzleInfo, LineSize, ColSize, LastLine),													
																display_time,
																% ---- Solving puzzle - END ----
																
																% ---- Statistics ----
																display_statistics,
																
																% ---- Show solution ---- %
																write('Press any key to show solution ...'),
																get_char(_),
																
																append(Puzzle, [LastLine], TMP), !,						
																display_puzzle(TMP, 'final'), nl.																											
				
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Constraints %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API: apply_puzzle_constraints/7, apply_row_constraints/4 , apply_constraints/5, apply_line_constraints/7, apply_circle_remaining_constraint/2

% Applies the necessary constraints to the puzzle

% +Options: 	Options to optimize labeling
% +Vars:		Domain variables to be constraint
% +Puzzle:   	Internal Representation of the puzzle (variables)
% +PuzzleInfo:  Internal Representation of the puzzle (facts)
% +LineSize: 	Number of lines
% +ColSize: 	Number of columns
% +LastLine:	Internal Representation of the last line of the puzzle to be displayed (fact)
apply_puzzle_constraints(Options, Vars, Puzzle, PuzzleInfo, LineSize, ColSize, LastLine):- 	domain(Vars, 0, 9), !,
																							write('lal'),
																							apply_row_constraints(Puzzle, LineSize, LastLine, 0), !,
																							write('lel'),
																							apply_constraints(Puzzle, PuzzleInfo, 0, LineSize, ColSize), !,
																							write('lol'),
																							apply_circle_remaining_constraint(Puzzle, PuzzleInfo), !,
																							write('lul'),
																							write(Vars),
																							labeling(Options, Vars). % Labeling options are the default
																		
% Applies row row contraint by making sure that the sum of all elements in each row is equal to a certain value

% +Puzzle:   Internal Representation of the puzzle (variables)
% +LineSize: Number of lines
% +LastLine: Internal Representation of the last line of the puzzle to be displayed (fact)
% +CurrLine: Current line
apply_row_constraints([], LastLine, _, LastLine):- !.
apply_row_constraints([H|T], LineSize, LastLine, CurrLine):- 	nth0(CurrLine, LastLine, LineSum),															
																safe_sum(H, #=, LineSum), !,
																NewCurrLine is CurrLine + 1,
																apply_row_constraints(T, LineSize, LastLine, NewCurrLine).
																
% Applies to each element it's corresponding constraint (puzzle)

% +Puzzle:   	Internal Representation of the puzzle (variables)
% +PuzzleInfo:  Internal Representation of the puzzle (facts)
% +CurrLine: Current line
% +LineSize: 	Number of lines
% +ColSize: 	Number of columns
apply_constraints(_, _, LineSize, LineSize, _):- !.
apply_constraints(Puzzle, PuzzleInfo, CurrLine, LineSize, ColSize):- 	write('r'), nth0(CurrLine, Puzzle, Line), nth0(CurrLine, PuzzleInfo, LineInfo), !,
																		apply_line_constraints(Puzzle, PuzzleInfo, Line, LineInfo, CurrLine, 0, ColSize),
																		NextLine is CurrLine + 1,
																		apply_constraints(Puzzle, PuzzleInfo, NextLine, LineSize, ColSize).

% Applies to each element it's corresponding constraint (puzzle line)

% +Puzzle:   	Internal Representation of the puzzle (variables)
% +PuzzleInfo:  Internal Representation of the puzzle (facts)
% +Line:		A certain line of the puzzle
% +LineInfo     Information about the puzzle line
% +CurrLine: 	Current line
% +CurrCol: 	Current column
% +ColSize:  	Number of columns
apply_line_constraints(_, _, _, _, _, ColSize, ColSize):- !.
apply_line_constraints(Puzzle, PuzzleInfo, Line, LineInfo, CurrLine, CurrCol, ColSize):- 	NextCol is CurrCol + 1,
																							nth0(CurrCol, LineInfo, VarInfo),
																							element(NextCol, Line, Var), !,
																							apply_element_constraint(VarInfo, Var, Puzzle, PuzzleInfo, CurrLine, CurrCol), !,
																							write('e'),
																							apply_line_constraints(Puzzle, PuzzleInfo, Line, LineInfo, CurrLine, NextCol, ColSize).
																						

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Element Constraints %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%																
% API: apply_element_constraint/6
																
% Neighbors clockwise access - neighbor(Direction, LineInc, ColInc)																
neighbor(0, -1,  0). % Up
neighbor(1,  0,  1). % Right
neighbor(2,  1,  0). % Down
neighbor(3,  0, -1). % Left
																														
																
% Empty element constraints

% +Type: 		Specifies the type of the variable
% +Var:         Variable to apply the constraints
% +Puzzle:   	Internal Representation of the puzzle (variables)
% +PuzzleInfo:  Internal Representation of the puzzle (facts)
% +LineNo:	 	Line number
% +ColNo:	 	Column number
apply_element_constraint(0, _, _, _, _, _).

% Star element constraints

% +Type: 		Specifies the type of the variable
% +Var:         Variable to apply the constraints
% +Puzzle:   	Internal Representation of the puzzle (variables)
% +LineNo:	 	Line number
% +ColNo:	 	Column number	
apply_element_constraint(1, Var, Puzzle, _, LineNo, ColNo):- 	Var in {2, 3, 5, 7}, % Var is prime
																Puzzle = [Line | _], length(Puzzle, LineSize), length(Line, ColSize), !,
																apply_star_constraints(Puzzle, LineSize, ColSize, LineNo, ColNo, 0).																													
																	
% Square element constraints

% +Type: 		Specifies the type of the variable
% +Var:         Variable to apply the constraints
% +Puzzle:   	Internal Representation of the puzzle (variables)
% +PuzzleInfo:  Internal Representation of the puzzle (facts)
% +LineNo:	 	Line number
% +ColNo:	 	Column number	
apply_element_constraint(2, Var, Puzzle, PuzzleInfo, LineNo, ColNo):- 	Var mod 5 #= 0,	% Var is either 0 or 5
																		Puzzle = [Line | _], length(Puzzle, LineSize), length(Line, ColSize),  !,
																		apply_square_constraints(Puzzle, PuzzleInfo, Var, LineSize, ColSize, LineNo, ColNo, 0).												

% Diamond element constraints

% +Type: 		Specifies the type of the variable
% +Var:         Variable to apply the constraints
% +Puzzle:   	Internal Representation of the puzzle (variables)
% +LineNo:	 	Line number
% +ColNo:	 	Column number	
apply_element_constraint(3, Var, Puzzle, _, LineNo, ColNo):- 	Var mod 2 #\= 0, % Var is even
																apply_diamond_constraints(Puzzle, Var, LineNo, ColNo).

% Triangle element constraints

% +Type: 		Specifies the type of the variable
% +Var:         Variable to apply the constraints
% +Puzzle:   	Internal Representation of the puzzle (variables)
% +LineNo:	 	Line number
% +ColNo:	 	Column number	
apply_element_constraint(4, Var, Puzzle, _, LineNo, ColNo):-	Var #\= 0,
																neighbor(0, LineInc, ColInc), % Retrieves the element directly above
																NewLineNo is LineNo + LineInc,	NewColNo is ColNo + ColInc, !,
																Puzzle = [Line | _], length(Puzzle, LineSize), length(Line, ColSize),  !,
																apply_triangle_constraints(Puzzle, Var, LineSize, ColSize, NewLineNo, NewColNo).

% Cirle element constraints

% +Type: 		Specifies the type of the variable
% +Var:         Variable to apply the constraints
apply_element_constraint(5, Var, _, _, _, _):- 	Var mod 3 #\= 0. % Var is not multiple of 3
																
																	
% Chess Knight element constraints

% +Type: 		Specifies the type of the variable
% +Var:         Variable to apply the constraints
% +Puzzle:   	Internal Representation of the puzzle (variables)
% +LineNo:	 	Line number
% +ColNo:	 	Column number	
apply_element_constraint(6, Var, Puzzle, _, LineNo, ColNo):- 	Puzzle = [Line | _], length(Puzzle, LineSize), length(Line, ColSize),  !,
																apply_chess_knight_constraints(Var, Puzzle, LineSize, ColSize, LineNo, ColNo).

% Heart element constraints

% +Type: 		Specifies the type of the variable
% +Var:         Variable to apply the constraints
% +Puzzle:   	Internal Representation of the puzzle (variables)
% +PuzzleInfo:  Internal Representation of the puzzle (facts)
% +LineNo:	 	Line number
% +ColNo:	 	Column number
apply_element_constraint(7, Var, Puzzle, PuzzleInfo, LineNo, ColNo):- 	Puzzle = [Line | _], length(Puzzle, LineSize), length(Line, ColSize),  !,
																		apply_heart_constraints(Var, Puzzle, PuzzleInfo, LineSize, ColSize, LineNo, ColNo).																																	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Star Constraints %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API: apply_star_constraints/6, star_constraint/5
% Auxiliary Predicates: check_boundaries, get_element

% Applies remaining constraints to the star element

% +Puzzle:   	Internal Representation of the puzzle (variables)
% +LineSize: 	Number of lines
% +ColSize: 	Number of columns
% +LineNo:	 	Line number
% +ColNo:	 	Column number
% +Direction:	Direction where it should apply the constraint									
apply_star_constraints(_, _, _, _, _, 4):- !.															
apply_star_constraints(Puzzle, LineSize, ColSize, LineNo, ColNo, Direction):- 	neighbor(Direction, LineInc, ColInc),
																				NewLineNo is LineNo + LineInc,
																				NewColNo is ColNo + ColInc, !,
																				star_constraint(Puzzle, LineSize, ColSize, NewLineNo, NewColNo), !,
																				NewDirection is Direction + 1, 
																				apply_star_constraints(Puzzle, LineSize, ColSize, LineNo, ColNo, NewDirection).
											
% Star constraint

% +Puzzle:   	Internal Representation of the puzzle (variables)
% +LineSize: 	Number of lines
% +ColSize: 	Number of columns
% +LineNo:	 	Line number
% +ColNo:	 	Column number										
star_constraint(Puzzle, LineSize, ColSize, LineNo, ColNo):-	check_boundaries(LineSize, ColSize, LineNo, ColNo),
															get_element(Puzzle, LineNo, ColNo, Neighbor),
															Neighbor in {0, 4, 6, 8, 9}.
star_constraint(_, _, _, _, _).	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Square Constraints %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API: apply_square_constraints/8, square_constraint/7
% Auxiliary Predicates: check_boundaries, get_element

% Applies remaining constraints to the square element

% +Puzzle:   	Internal Representation of the puzzle (variables)
% +PuzzleInfo:  Internal Representation of the puzzle (facts)
% +Var:         Variable to apply the constraint
% +LineSize: 	Number of lines
% +ColSize: 	Number of columns
% +LineNo:	 	Line number
% +ColNo:	 	Column number
% +Direction:	Direction where it should apply the constraint		
apply_square_constraints(_, _, _, _, _, _, _, 4):- !.
apply_square_constraints(Puzzle, PuzzleInfo, Var, LineSize, ColSize, LineNo, ColNo, Direction):- 	neighbor(Direction, LineInc, ColInc),
																									NewLineNo is LineNo + LineInc, NewColNo is ColNo + ColInc, !,
																									square_constraint(Puzzle, PuzzleInfo, Var, LineSize, ColSize, NewLineNo, NewColNo), !,
																									NewDirection is Direction + 1,
																									apply_square_constraints(Puzzle, PuzzleInfo, Var, LineSize, ColSize, LineNo, ColNo, NewDirection).

% Square constraint

% +Puzzle:   	Internal Representation of the puzzle (variables)
% +PuzzleInfo:  Internal Representation of the puzzle (facts)
% +Var:         Variable to apply the constraint
% +LineSize: 	Number of lines
% +ColSize: 	Number of columns
% +LineNo:	 	Line number
% +ColNo:	 	Column number																								
square_constraint(Puzzle, PuzzleInfo, Var, LineSize, ColSize, LineNo, ColNo):- 	check_boundaries(LineSize, ColSize, LineNo, ColNo),
																				get_element(Puzzle, LineNo, ColNo, Neighbor),
																				get_element(PuzzleInfo, LineNo, ColNo, NeighborInfo),
																				NeighborInfo \= 3, % Neighbor is not a diamond
																				Var #\= Neighbor .			
square_constraint(_, _, _, _, _, _, _).	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Diamond Constraints %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API: apply_diamond_constraints/4, get_left_elements/4

% Applies remaining constraints to the diamond element

% +Puzzle:   Internal Representation of the puzzle (variables)
% +Var:      Variable to apply the constraint
% +LineNo:	 Line number
% +ColNo:	 Column number
apply_diamond_constraints(Puzzle, Var, LineNo, ColNo):-	nth0(LineNo, Puzzle, Line),
														write('hue'),
														get_left_elements(Line, 0, ColNo, LeftElements),
														safe_sum(LeftElements, #=, Var).
apply_diamond_constraints(_, Var, _, _):- Var #= 9.

% Get all elements that are left of a certain element

% +PuzzleLine:   Internal Representation of the puzzle line (variables)
% +CurrCol:      Current column number
% +ColNo:	 	 Element column number						
% -LeftElements: Elements on the left								
get_left_elements(_, ColNo, ColNo, []):- !.				
get_left_elements([H|T], CurrCol, ColNo, [H|Rest]):-	NewCurrCol is CurrCol + 1, write(CurrCol), !,	
														get_left_elements(T, NewCurrCol, ColNo, Rest).									
														

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Triangle Constraints %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API: apply_triangle_constraints/6
% Auxiliary Predicates: check_boundaries, get_element

% Applies remaining constraints to the diamond element

% +Puzzle:   	Internal Representation of the puzzle (variables)
% +Var:         Variable to apply the constraint
% +LineSize: 	Number of lines
% +ColSize: 	Number of columns
% +LineNo:	 	Line number
% +ColNo:	 	Column number	
apply_triangle_constraints(Puzzle, Var, LineSize, ColSize, LineNo, ColNo):- check_boundaries(LineSize, ColSize, LineNo, ColNo),	
																			get_element(Puzzle, LineNo, ColNo, Neighbor),
																			Neighbor mod 2 #= 0,
																			Var #< Neighbor.
apply_triangle_constraints(_, _, _, _, _, _).																
																
																

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Circle Constraints %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API: apply_circle_remaining_constraint/2, get_all_circles/4
% Auxiliary predicates: all_equal/1

% Applies remaining constraints to the circle element

% +Puzzle:   	Internal Representation of the puzzle (variables)
% +PuzzleInfo:  Internal Representation of the puzzle (facts)	
apply_circle_remaining_constraint(Puzzle, PuzzleInfo):- flatten(Puzzle, PuzzleFlatten), flatten(PuzzleInfo, PuzzleInfoFlatten),
														get_all_circles(PuzzleFlatten, PuzzleInfoFlatten, AllCircles, 5),
														write(AllCircles),
														all_equal(AllCircles).

% Retrieves all circle variables present on the puzzle
														
% +Puzzle:   	Internal Representation of the puzzle (variables)
% +PuzzleInfo:  Internal Representation of the puzzle (facts)	
% -Circles:     List of all circles
% +Circle:		Circle type								
get_all_circles([], _, [], _):- !.
get_all_circles([H1|T1], [Circle|T2], [H1|Rest], Circle):- !, get_all_circles(T1, T2, Rest, Circle).
get_all_circles([_|T1], [_|T2], AllCircles, Circle):- !, get_all_circles(T1, T2, AllCircles, Circle).

% Applies constraint that ensures that all elements are equal

% List:	List containing all elements where constraint will be applied
all_equal([]):- !.												
all_equal([_]):- !.
all_equal([H1,H2|T]):- 	H1 #= H2, !,
						all_equal([H2|T]).
																												
															
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Chess Knight Constraints %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API: apply_chess_knight_constraints/6, get_attack_range_elements/7

% Chess Knight attack range clockwise access - attack_range(Direction, LineInc, ColInc)
attack_range(0, -2,  1). % Up-Right
attack_range(1, -1,  2). % Mid-Up-Right
attack_range(2,  1,  2). % Mid-Down-Right
attack_range(3,  2,  1). % Down-Right
attack_range(4,  2, -1). % Down-Left
attack_range(5,  1, -2). % Mid-Down-Left
attack_range(6, -1, -2). % Mid-Up-Left
attack_range(7, -2, -1). % Up-Left

% Applies constraints to the chess knight element

% +Var:         Variable to apply the constraint
% +Puzzle:   	Internal Representation of the puzzle (variables)
% +LineSize: 	Number of lines
% +ColSize: 	Number of columns
% +LineNo:	 	Line number
% +ColNo:	 	Column number	
apply_chess_knight_constraints(Var, Puzzle, LineSize, ColSize, LineNo, ColNo):-	get_attack_range_elements(Puzzle, LineSize, ColSize, LineNo, ColNo, Elements, 0), !,
																				safe_sum(Elements, #=, Var).															

% Retrieves chess knight attack range elements																				
																				
% +Puzzle:   	Internal Representation of the puzzle (variables)
% +LineSize: 	Number of lines
% +ColSize: 	Number of columns
% +LineNo:	 	Line number
% +ColNo:	 	Column number
% -Elements:	Attack range elements
% +Direction:	Direction where it should apply the constraint																						
get_attack_range_elements(_, _, _, _, _, [], 8):- !.
get_attack_range_elements(Puzzle, LineSize, ColSize, LineNo, ColNo, [Result|Rest], Direction):- 	write('fds1'), attack_range(Direction, LineInc, ColInc),
																									NewLineNo is LineNo + LineInc,	NewColNo is ColNo + ColInc,
																									check_boundaries(LineSize, ColSize, NewLineNo, NewColNo),	
																									get_element(Puzzle, NewLineNo, NewColNo, Element),
																									Element mod 2 #= 0 #<=> Result,	% If element is even then Result will be equal to 1. Otherwise Result is equal to 0
																									NewDirection is Direction + 1,
																									get_attack_range_elements(Puzzle, LineSize, ColSize, LineNo, ColNo, Rest, NewDirection).
get_attack_range_elements(Puzzle, LineSize, ColSize, LineNo, ColNo, Elements, Direction):-	write('fds2'), NewDirection is Direction + 1,
																							get_attack_range_elements(Puzzle, LineSize, ColSize, LineNo, ColNo, Elements, NewDirection).																				

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Heart Constraints %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API: apply_heart_constraints/7, get_neighboring_hearts/8

% Applies constraints to the heart element

% +Var:         Variable to apply the constraint
% +Puzzle:   	Internal Representation of the puzzle (variables)
% +PuzzleInfo:  Internal Representation of the puzzle (facts)	
% +LineSize: 	Number of lines
% +ColSize: 	Number of columns
% +LineNo:	 	Line number
% +ColNo:	 	Column number
apply_heart_constraints(Var, Puzzle, PuzzleInfo, LineSize, ColSize, LineNo, ColNo):- 	get_neighboring_hearts(Puzzle, PuzzleInfo, LineSize, ColSize, LineNo, ColNo, Neighbors, 0),
																						append(Neighbors, [Var], Elements), !,
																						safe_sum(Elements, #=, 10).
																	
% Retrieves all heart neighbors
																	
% +Puzzle:   	Internal Representation of the puzzle (variables)
% +PuzzleInfo:  Internal Representation of the puzzle (facts)	
% +LineSize: 	Number of lines
% +ColSize: 	Number of columns
% +LineNo:	 	Line number
% +ColNo:	 	Column number
% -Neighbors:	Heart neighbors
% +Direction:	Direction where it should apply the constraint																		
get_neighboring_hearts(_, _, _, _, _, _, [], 4):- !.																
get_neighboring_hearts(Puzzle, PuzzleInfo, LineSize, ColSize, LineNo, ColNo, [Neighbor|Rest], Direction):-	write('1fds'), neighbor(Direction, LineInc, ColInc),
																											NewLineNo is LineNo + LineInc,	NewColNo is ColNo + ColInc,
																											check_boundaries(LineSize, ColSize, NewLineNo, NewColNo),	
																											get_element(PuzzleInfo, NewLineNo, NewColNo, NeighborInfo),
																											NeighborInfo =:= 7,	% Neighbor is a heart
																											get_element(Puzzle, NewLineNo, NewColNo, Neighbor),
																											NewDirection is Direction + 1,
																											get_neighboring_hearts(Puzzle, PuzzleInfo, LineSize, ColSize, LineNo, ColNo, Rest, NewDirection).
get_neighboring_hearts(Puzzle, PuzzleInfo, LineSize, ColSize, LineNo, ColNo, Neighbors, Direction):-	write('2fds'), NewDirection is Direction + 1,
																										get_neighboring_hearts(Puzzle, PuzzleInfo, LineSize, ColSize, LineNo, ColNo, Neighbors, NewDirection).