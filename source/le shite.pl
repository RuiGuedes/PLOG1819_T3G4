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