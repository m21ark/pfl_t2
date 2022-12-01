''' INITIAL BOARD

board = [['O','O','O','O','O'],
		 ['O','O','O','O','O'],
		 ['O','O','O','O','O'],
		 ['O','O','O','O','O'],
		 ['O','O','O','O','O'],
		 ['O','O','O','O','O']]
'''

''' COMPLETE BOARD

board = [['B','W','B','W','B'],
		 ['O','B','O','B','O'],
		 ['W','O','W','O','W'],
		 ['B','W','O','W','O'],
		 ['W','O','B','O','B'],
		 ['O','B','W','B','W']]
'''		

board = [['B','W','B','W','B'],
		 ['O','O','O','B','O'],
		 ['W','O','O','O','W'],
		 ['B','O','O','W','O'],
		 ['W','O','B','O','B'],
		 ['O','B','W','B','W']]

whiteTurn = False 
whiteCount = 12
blackCount = 12	 

def board_print():
	print("\n ", end="")
	for col in range(0, len(board)):
		for row in range(0, len(board[0])):
			print(f"{board[col][row]} ", end="" )
		print(f"|{col}\n ", end="")
	print("----------")
	print(" 0 1 2 3 4\n ", end="")

def set_piece(row, col, color):
	global board
	# check if can be set
	board[col][row] = color
	
def can_set_any(color):

	for col in range(0, len(board)):
		for row in range(0, len(board[0])):
			if(board[col][row] == 'O'):
				if(check_cross(row, col, color)):
					return True
		
	return False
	
def check_cross(row, col, color):

	if(col-1 >= 0):
		pos = board[col-1][row]
		if(pos == color): return False
		
	if(col+1 <= 4):
		pos = board[col+1][row]
		if(pos == color): return False
		
	if(row-1 >= 0):
		pos = board[col][row-1]
		if(pos == color): return False
		
	if(row+1 <= 4):
		pos = board[col][row+1]
		if(pos == color): return False
	
	print(f"\nPossible to add '{color}' at col={col} row={row} ")
	return True
	
def capture_piece(row, col):
	global whiteTurn
	global whiteCount
	global blackCount
	
	aux = board[col][row]
	
	# detect if not part of a 3match
	if (whiteTurn and aux == 'B'):
		blackCount+=1
		board[col][row] = 'O'
	elif ((not whiteTurn) and aux == 'W'):
		whiteCount+=1
		board[col][row] = 'O'
	else:
		return False
	return True



def transpose():
	return list(zip(*board))

def detect_match():

	i = 0
	for col in board:
		s = "".join(col)
		if (s.find('WWW') != -1):
			print(f"White match in row {i}!")
			print(s)
			return True
		elif (s.find('BBB') != -1):
			print(f"Black match in row {i}!")
			print(s)
			return True
		i+=1
		
	transp = transpose()
	
	i = 0
	for col in transp:
		s = "".join(col)
		if (s.find('WWW') != -1):
			print(f"White match in col {i}!")
			print(s)
			return True
		elif (s.find('BBB') != -1):
			print(f"Black match in col {i}!")
			print(s)
			return True
		i+=1
	

def whiteDrop():
	global whiteTurn
	global whiteCount

	pos = input('\nWhite set piece > ')
	col = int(pos[0]) 
	row = int(pos[2])

	if(not (board[col][row] == 'O' or check_cross(row,col, 'W'))):
		return False
		
	set_piece(row, col, 'W')
	whiteCount-=1
	whiteTurn = False
	
	return True

def blackDrop():
	global whiteTurn
	global blackCount

	pos = input('\nBlack set piece > ')
	col = int(pos[0]) 
	row = int(pos[2])
	
	if(not (board[col][row] == 'O' or check_cross(row,col, 'B'))):
		return False
		
	set_piece(row, col, 'B')
	blackCount-=1
	whiteTurn = True
	
	return True
	

def drop_phase():

	global whiteCount
	global blackCount
	
	global whiteTurn
	whiteTurn = True

	while True:
		board_print()
		
		if(whiteTurn and can_set_any('W')):
			res = whiteDrop()
		elif((not whiteTurn) and can_set_any('B')):
			res = blackDrop()
		else:
			(num_white, num_black) = check_num_board_stones()
			# apenas redundancia
			whiteCount = 12-num_white
			blackCount = 12-num_black
			
			print(f"There are {num_white} W and {num_black} B.")
			print("\nAll stones possible are set.\n")
			print("Starting capture phase!")
			break
		
		if(not res):
			print("Invalid placement!")
		
		if(whiteCount * blackCount == 0): 
			break
				

def whiteMove():

	pos = input('\nWhite move piece > ')
	cur_col = int(pos[0]) 
	cur_row = int(pos[2])
	
	new_col = int(pos[4]) 
	new_row = int(pos[6])
	
	same_row = (abs(cur_col-new_col) == 1) and (cur_row == new_row)
	same_col = (abs(cur_row-new_row) == 1) and (cur_col == new_col)
	
	old_pos = board[cur_col][cur_row]
	new_pos = board[new_col][new_row]
	
	if(same_col or same_row):
		if(old_pos == 'W'):
			if(new_pos == 'O'):
				set_piece(cur_row, cur_col, 'O')
				set_piece(new_row, new_col, 'W')
				return True
			else:
				print("Not empty destiny")
		else:
			print("Not your piece")
	else:
		print("Not adjacent!")
			
	return False

def blackMove():

	pos = input('\nBlack move piece > ')
	cur_col = int(pos[0]) 
	cur_row = int(pos[2])
	
	new_col = int(pos[4]) 
	new_row = int(pos[6])
	
	same_row = (abs(cur_col-new_col) == 1) and (cur_row == new_row)
	same_col = (abs(cur_row-new_row) == 1) and (cur_col == new_col)
	
	old_pos = board[cur_col][cur_row]
	new_pos = board[new_col][new_row]
	
	print(f"cur_row={cur_row} cur_col={cur_col} new_row={new_row} new_col={new_col} old_pos={old_pos} new_pos={new_pos}")
	
	if(same_col or same_row):
		if(old_pos == 'B'):
			if(new_pos == 'O'):
				set_piece(cur_row, cur_col, 'O')
				set_piece(new_row, new_col, 'B')
				return True
			else:
				print("Not empty destiny")
		else:
			print("Not your piece")
	else:
		print("Not adjacent!")
			
	return False
	
def check_if_winner():
	(num_white, num_black) = check_num_board_stones()
	
	if(num_black <= 2):
		print('White wins the game!\n')
		return True
	elif(num_white <= 2):
		print('Black wins the game!\n')
		return True
		
	return False
	
def check_num_board_stones():
	
	total = ""
	for col in board:
		aux = "".join(col)
		total += aux
		
	return (total.count('W'),total.count('B'))
		
	
def capture_phase():

	global whiteTurn
	whiteTurn = True
	
	while (not check_if_winner()):
	
		board_print()

		while True:
			
			if(whiteTurn):
				res = whiteMove()
			else:
				res = blackMove()

			if(res):
				break
			else:
				print("Invalid move!")

	
		if(detect_match()):
			
			board_print()
			
			while True:
			
				pos = input(f'\nWhich {"Black" if whiteTurn else "White"} piece to take > ')
				col = int(pos[0]) 
				row = int(pos[2])
				if(capture_piece(row, col)):
					break
				else:
					print("Invalid capture!")
			
		whiteTurn = not whiteTurn

def main():
	drop_phase()
	capture_phase()
	
	
main()
	
