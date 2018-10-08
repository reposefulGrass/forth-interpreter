
# ==============================================================
# forth.asm - A basic forth interpreter in MIPS asm.
#
# Written by Corey Wingo (csw170030) 
# 
# Global Registers:
#	s1 - the index of the topmost item of the stack (0, 1, 2, ...)
#
# ==============================================================

# ======================== DATA SEGMENT ========================
	.data
	
stack:	
	.align		2
	.space		1024
	
	
prompt_filename:
	.asciiz		"Name of the file to interpret | "
filename:		# File to parse
	.space		100
	
	
program:			# Holds the entire file to parse
	.space		1000
program_length:		# Length of the program to parse (NOTE: Should be equal to the value of `program`)
	.word		1000
program_cursor:		# Pointer to the address of `program`
					# used to iterate through `program` like a cursor.
	.word		0
	
	
curr_token:		# Holds the current token that has just been parsed
	.space		64
curr_token_len:
	.word		64
curr_token_index:		# Index to add characters to the curr_token
	.word		0
	
current_temp_variable:	# Holds a hashcode of the variable name that is being parsed. 
	.word		0	
	
	
variable_space:
	.space		800
variable_index:
	.word		0


keyword_variable_set:	# Flag set to create a variable
	.word		0
keyword_variable_get:	# Flag set to get the value of a variable
	.word		0
keyword_disregard:	# Flag set to discard tokens (used by if statement)
	.word		0
keyword_comment:		# Flag that tells connect_keywords to disregard andthing
			# inbetween the two comment operators (/* */)
	.word		0
keyword_print_string:	# Flag that tells connect_keywords that it is printing a string
	.word		0
	
error_msg_stack_not_enough_operands:
	.asciiz		"ERROR: Not enough operands on the stack!"
error_msg_unable_to_read_file:
	.asciiz		"ERROR: Unable to read file."
error_msg_curr_token_index_out_of_range:
	.ascii		"ERROR: Current Token Index Out Of Range\n"
	.asciiz		"Current Token Index: "
error_msg_parsing_non_digit_str_as_digit_str:
	.asciiz		"ERROR: The string that was being parsed as a non-alpha string contains alpha characters.\n"
error_msg_unknown_word:
	.asciiz		"ERROR: Unkown Word: "
error_msg_no_variable_found:
	.asciiz		"ERROR: An unknown variable was used!"
	

# ======================== CODE SEGMENT =======================
	.text
main:
	li	$s1, 0			# index (top) of the stack
	
	jal 	get_filename
	
	la	$a0, filename
	jal 	read_file

	la	$t0, program		# Set the cursor equal to the begginning 
	sw	$t0, program_cursor	# of the program.
	
	lw	$a0, program_cursor
	jal	parse_program
	
exit:	
	li	$v0, 10			
	syscall
	
	
# ____ GET_FILENAME ______________________________________________
# Prompt the user for a name of the file to parse and
# place it into `filename`. 
#
# Registers:
#	t0 - address of filename
#	t1 - temporary char buffer
# ----------------------------------------------------------------
get_filename:
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	
	la	$a0, prompt_filename
	li	$v0, 4
	syscall
	
	li	$v0, 8
	la	$a0, filename
	li	$a1, 100
	syscall
	
gf_remove_newline:			# Remove the newline so that read_file
	la	$t0, filename		# can open the file using filename.
	
gf_loop:
	lbu	$t1, 0($t0)
	beqz	$t1, gf_loop_exit	# End of filename string: exit
	bne	$t1, 10, gf_skip		# If char == newline, replace it with 0
	li	$t1, 0
gf_skip:
	sb	$t1, 0($t0)
	addi	$t0, $t0, 1
	b	gf_loop
	
gf_loop_exit:
	lw	$ra, 0($sp)
	addi	$sp, $sp, 4
	jr	$ra
	
	
# ____ PARSE _____________________________________________________
# Parse the entire memory given in $a0. 
#
# ======== PROCESS ========
# 1. Read the next available token
#	a. if 1 is returned in $v1, EOF has been reached, do not continue parsing after.
# 2. If the token was a number, simply push it onto the stack
# 3. If the token was anything but number, hash it and find what keyword it corresponds to. 
#    Do whatever operation the keyword is (add - adds the top two items on the stack, etc.)
# 1.	b. if 0 was returned in $v0, loop to parse again.
#
# input:
#	a0 - address of the memory to parse
#
# Registers:
#	s0 - Save state of EOF reached flag, if 1 exit the procedure
#	s2 - the modified cursor (address of program that is being parsed incremented)
# ----------------------------------------------------------------
parse_program:
	sub	$sp, $sp, 12
	sw	$ra, 0($sp)
	sw	$s0, 4($sp)
	sw	$s2, 8($sp)

pp_parse:
	jal	read_next_token			
	move	$s2, $v0			# Move new cursor to s2 so v0 is free.
	move	$s0, $v1			# s0 <= EOF flag
	
	# If the token is a non-digit, hash it and connect it to its corresponding operation, else, skip the hash.
	la	$a0, curr_token
	jal	determine_if_digit
	
	beq	$v0, 1, pp_skip_hash
	
	la	$a0, curr_token
	jal	hash_str
	
	la	$a1, curr_token
	move	$a0, $v0			# a0 - the hash of the token
	jal	connect_keyword
	b	pp_continue
	
pp_skip_hash:
	la	$a0, curr_token		# If the token was a number, parse it and
	jal	parse_digit			# add it to the stack
	move	$a0, $v0
	jal	stack_push			# Since the token was a digit, we don't have to 
						# find what operation it corresponds to
	
pp_continue:
	move	$a0, $s2			# Set a0 back to the new modified cursor. 
						# The loop will reuse a0.
	beq	$s0, 0, pp_parse		# if EOF is returned as 0 (false), parse again
	
	lw	$s2, 8($sp)
	lw	$s0, 4($sp)
	lw	$ra, 0($sp)
	add	$sp, $sp, 8
	jr	$ra


# ____ READ_FILE _________________________________________________
# Read the entire file and place its contents
# inside the memory labeled "program".
#
# Input:
#	a0 - name of file to read (address)
# ----------------------------------------------------------------
read_file:
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	
	li	$a1, 0			# Open filename given in $a0
	li	$a2, 0
	li	$v0, 13
	syscall
	
	move	$a0, $v0			# Read the file and place its
	la	$a1, program		# contents into `program` memory
	lw	$a2, program_length
	li	$v0, 14
	syscall
	
	bgez	$v0, rf_skip_error	# If error reading file, exit
	la	$a0, error_msg_unable_to_read_file
	li	$v0, 4
	syscall
	b	exit
	
rf_skip_error:
	
	li	$v0, 16			# Close the file
	syscall

	lw	$ra, 0($sp)
	add	$sp, $sp, 4
	jr	$ra
	

# ____ SET_CURR_TOKEN_INDEX_ZERO _________________________________
# Set curr_token_index to zero
#
# Registers:
#	t0 - address of curr_token_index
# ----------------------------------------------------------------
sctiz:
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	
	la	$t0, curr_token_index	# Set curr_token_index to 0
	sw	$zero, ($t0)

	lw	$ra, 0($sp)
	addi	$sp, $sp, 4
	jr	$ra
	

# ____ INC_CURR_TOKEN_INDEX ______________________________________
# Increment curr_token_index by 1
# If index >= curr_token_len, output curr_token_index_out_of_range.
#
# Registers:
#	t0 - address of curr_token_index
#	t1 - dereference of $t0
#	t2 - curr_token_len
# ----------------------------------------------------------------
icti:
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	
	la	$t0, curr_token_index	# increment curr_token_index
	lw	$t1, ($t0)
	addi	$t1, $t1, 1

	# Index Out Of Range?
	lw	$t2, curr_token_len	# check if curr_token_index >= curr_token_len
	blt	$t1, $t2, icti_skip_error_index
	la	$a0, error_msg_curr_token_index_out_of_range
	li	$v0, 4
	syscall
	
	move	$a0, $t1			# Print curr_token_index
	li	$v0, 1
	syscall
	
	li	$a0, '\n'			
	li	$v0, 11
	syscall
	
	b	exit
	
icti_skip_error_index:
	sw	$t1, ($t0)
	
	lw	$ra, 0($sp)
	addi	$sp, $sp, 4
	jr	$ra
	

# ____ APPEND_CURR_TOKEN _________________________________________
# Append the character in $a0 at the end of curr_token
#
# Input:
#	a0 - character to append to curr_token
#
# Registers:
# 	s0 - address of curr_token
#	s1 - curr_token_index
# ----------------------------------------------------------------
append_curr_token:
	sub	$sp, $sp, 12
	sw	$ra, 0($sp)
	sw	$s0, 4($sp)
	sw	$s1, 8($sp)
	
	la	$s0, curr_token		# increment $s0 (address of curr_token) to the position 
	lw	$s1, curr_token_index	# in which to add the character
	add	$s0, $s0, $s1		
	
	sb	$a0, ($s0)			# store the character at that position
	jal	icti				

	lw	$s1, 8($sp)
	lw	$s0, 4($sp)
	lw	$ra, 0($sp)
	addi	$sp, $sp, 12
	jr	$ra


# ____ ERASE_CURR_TOKEN _________________________________________
# Set all of the characters in curr_token to 0 (null)
#
# Registers:
#	s0 - the address of the current token
#	s1 - the length of the current token
#	t0 - the index of the loop [0, s1)
# ----------------------------------------------------------------
erase_curr_token:
	sub	$sp, $sp, 12
	sw	$s0, 8($sp)
	sw	$s1, 4($sp)
	sw	$ra, 0($sp)

	la	$s0, curr_token
	lw	$s1, curr_token_len
	li	$t0, 0			# Index
	
ect_erase:
	sb	$zero, ($s0)		# Set the current character at the cursor to 0
	addi	$s0, $s0, 1
	
	addi	$t0, $t0, 1			
	blt	$t0, $s1, ect_erase 		
	
	jal	sctiz

	lw	$ra, 0($sp)
	lw	$s1, 4($sp)
	lw	$s0, 8($sp)
	addi	$sp, $sp, 12
	jr	$ra


# ____ READ_NEXT_TOKEN ___________________________________________
# Read the next token in `program`. Each token is delimited
# by any number of spaces or newlines. 
#
# After reading a token, READ_NEXT_TOKEN consumes all of the
# whitespace till the next token.
#
# Input:
# 	a0 - address of the memory to read from (cursor)
#
# Output:
#	v0 - the address of the modified cursor
#	v1 - 0 => Cursor hasn't reached EOF
#	     1 => Cursor has reached EOF
#
# Registers:
#	s0 - the address of the memory to read from
#	s2 - temporary char buffer
# ----------------------------------------------------------------
read_next_token:
	sub	$sp, $sp, 12
	sw	$s2, 8($sp)
	sw	$s0, 4($sp)
	sw	$ra, 0($sp)
	
	li	$v1, 0			# Assume Cursor has not reached EOF
	jal	erase_curr_token
	move	$s0, $a0
	
	# If the char read isn't a space, nullchar, or newline, append it to curr_token
rnt_read_char:
	lbu	$s2, ($s0)			# Read a char
	beq	$s2, 32, rnt_char_space	# Char read is the space char
	beq	$s2, 0,  rnt_char_null	# Char read is the null char
	beq	$s2, 10, rnt_char_newline # Char read is a newline character
	
	move	$a0, $s2			# Append that char to curr_token
	jal	append_curr_token
	addi	$s0, $s0, 1
	
	b	rnt_read_char
	
rnt_char_null:
	li	$v1, 1			# Have reached the end of the memory to read from
	b	rnt_exit		# set EOF flag (EOF has been reached)
	
rnt_char_newline:
rnt_char_space:
	addi	$s0, $s0, 1			
	
rnt_remove_spaces:			# if the chars ahead of cursor are chars, increment cursor
	lbu	$s2, ($s0)		# (essentially removing them)
	beq	$s2, 32, rnt_char_space	
	beq	$s2, 10, rnt_char_newline
	beq	$s2, 0,  rnt_char_null
	
rnt_exit:

	move	$v0, $s0			# set the cursor to the modified cursor (address of program memory)

	lw	$ra, 0($sp)
	lw	$s0, 4($sp)
	lw	$s2, 8($sp)
	addi	$sp, $sp, 12
	jr	$ra

	
# ____ STACK_PUSH _____________________________________________
# Store $a0 onto the stack
#
# Registers:
#	s1 - the stack pointer
# -------------------------------------------------------------
stack_push:
	sub	$sp, $sp, 4	
	sw	$ra, 0($sp)
	
	la	$t0, stack	
	add	$t0, $t0, $s1		# t0 - the current real address of $s1 * 4
	sw	$a0, 0($t0)		# Store the item upon the stack
	
	li	$a0, 1
	jal 	stack_inc_ptr

	lw	$ra, 0($sp)
	add	$sp, $sp, 4
	jr	$ra


# ____ STACK_POP ______________________________________________
# Load the topmost item on the stack into $v0,
# decrement the stack pointer to point to the 2nd
# top most item. (effectively elemination the 
# topmost item on the stack)
#
# Registers:
#	t0 - address to stack
#
# Output:
#	v0 - the item popped from the stack
# -------------------------------------------------------------
stack_pop:
	sub	$sp, $sp, 8	
	sw	$s0, 4($sp)
	sw	$ra, 0($sp)

	# ==== NOT ENOUGH OPERANDS CHECK ========================
	jal	stack_len
	slti	$s0, $v0, 1		# The stack has at least 1 item
	
	bnez	$s0, error_stack_not_enough_operands
	# =======================================================
	
	la	$s0, stack
	
	li	$a0, 1
	jal	stack_dec_ptr
	
	add	$s0, $s0, $s1		# t0 - the current real address of $s1
	lw	$v0, 0($s0)		# Load the topmost item
	
	lw	$ra, 0($sp)
	lw	$s0, 4($sp)
	add	$sp, $sp, 8
	jr	$ra
	
	
# ____ STACK_INC_PTR __________________________________________
# Increase the stack ptr, $s1, by the
# amount in $a0, in terms of words (4 bytes)
#
# Input:
#	a0 - the amount of spaces to inc
#	
# Registers:
#	s1 - the stack pointer
# -------------------------------------------------------------
stack_inc_ptr:
	sub	$sp, $sp, 4		
	sw	$ra, 0($sp)
	
	sll	$a0, $a0, 2		# increment the stack by ($a0 * 4)
	add	$s1, $s1, $a0
	
	lw	$ra, 0($sp)
	add	$sp, $sp, 4
	jr	$ra
	
	
# ____ STACK_DEC_PTR __________________________________________
# Decrease the stack ptr, $s1, by the
# amount in $a0, in terms of words (4 bytes)
#
# Input:
#	a0 - the amount of spaces to dec
#
# Registers:
#	s1 - the stack pointer
# -------------------------------------------------------------
stack_dec_ptr:
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	
	sll	$a0, $a0, 2		# decrement the stack by ($a0 * 4)
	sub	$s1, $s1, $a0
	
	lw	$ra, 0($sp)
	add	$sp, $sp, 4
	jr	$ra
	
	
# ____ STACK_LEN ______________________________________________
# Return the length of the stack into $v0
#
# Output:
#	v0 - the length of the stack
# -------------------------------------------------------------
stack_len:
	sub	$sp, $sp, 4		
	sw	$ra, 0($sp)
	
	move	$v0, $s1
	srl	$v0, $v0, 2
	
	lw	$ra, 0($sp)
	add	$sp, $sp, 4
	jr	$ra
	
	
# ____ STACK_ADD ______________________________________________
# Pop off the top two items of the stack,
# add them, and then put the result onto the
# stack
#
# Registers:
#	s2 - the first operand
#	s3 - the second operand
# -------------------------------------------------------------
stack_add:
	sub	$sp, $sp, 16
	sw	$ra, 0($sp)
	sw	$s0, 4($sp)
	sw	$s2, 8($sp)
	sw	$s3, 12($sp)
	
	# ==== NOT ENOUGH OPERANDS CHECK ========================
	jal	stack_len
	slti	$s0, $v0, 2		# The stack has at least 2 items
	
	bnez	$s0, error_stack_not_enough_operands
	# =======================================================
	
	jal	stack_pop
	move	$s2, $v0
	
	jal	stack_pop
	move	$s3, $v0
	
	add	$a0, $s2, $s3
	jal	stack_push
	
	lw	$s3, 12($sp)
	lw	$s2, 8($sp)
	lw	$s0, 4($sp)
	lw	$ra, 0($sp)
	add	$sp, $sp, 16
	jr	$ra
	
	
# ____ STACK_SUBTRACT _________________________________________
# Pop off the top two items of the stack,
# subtract them, and then put the result onto the
# stack
#
# Registers:
#	s2 - the first operand
#	s3 - the second operand
# -------------------------------------------------------------
stack_subtract:
	sub	$sp, $sp, 16	
	sw	$ra, 0($sp)
	sw	$s0, 4($sp)
	sw	$s2, 8($sp)
	sw	$s3, 12($sp)
	
	# ==== NOT ENOUGH OPERANDS CHECK ========================
	jal	stack_len
	slti	$s0, $v0, 2		# The stack has at least 2 items
	
	bnez	$s0, error_stack_not_enough_operands
	# =======================================================
	
	jal	stack_pop
	move	$s2, $v0
	
	jal	stack_pop
	move	$s3, $v0
	
	sub	$a0, $s3, $s2		# sub <op1> <op2> = op1 - op2
	jal	stack_push

	lw	$s3, 12($sp)
	lw	$s2, 8($sp)
	lw	$s0, 4($sp)
	lw	$ra, 0($sp)
	add	$sp, $sp, 16
	jr	$ra
	
	
# ____ STACK_MULTIPLY _________________________________________
# Pop off the top two items of the stack, mult them, and then 
# put the result onto the stack.
#
# Any result larger than 32 bits is cut off. 
#
# Registers:
#	s2 - the first operand
#	s3 - the second operand
# -------------------------------------------------------------
stack_multiply:
	sub	$sp, $sp, 16
	sw	$ra, 0($sp)
	sw	$s0, 4($sp)
	sw	$s2, 8($sp)
	sw	$s3, 12($sp)
	
	# ==== NOT ENOUGH OPERANDS CHECK ========================
	jal	stack_len
	slti	$s0, $v0, 2		# The stack has at least 2 items
	
	bnez	$s0, error_stack_not_enough_operands
	# =======================================================
	
	jal	stack_pop
	move	$s2, $v0
	
	jal	stack_pop
	move	$s3, $v0
	
	mul	$a0, $s2, $s3
	jal	stack_push

	lw	$s3, 12($sp)
	lw	$s2, 8($sp)
	lw	$s0, 4($sp)
	lw	$ra, 0($sp)
	add	$sp, $sp, 16
	jr	$ra
	
	
# ____ STACK_DIVIDE ___________________________________________
# Pop off the top two items of the stack,
# divide them, and then put the result onto the
# stack
#
# Registers:
#	s2 - the first operand
#	s3 - the second operand
# -------------------------------------------------------------
stack_divide:
	sub	$sp, $sp, 16	
	sw	$ra, 0($sp)
	sw	$s0, 4($sp)
	sw	$s2, 8($sp)
	sw	$s3, 12($sp)
	
	# ==== NOT ENOUGH OPERANDS CHECK ========================
	jal	stack_len
	slti	$s0, $v0, 2		# The stack has at least 2 items
	
	bnez	$s0, error_stack_not_enough_operands
	# =======================================================
	
	jal	stack_pop
	move	$s2, $v0
	
	jal	stack_pop
	move	$s3, $v0
	
	div	$a0, $s3, $s2
	jal	stack_push

	lw	$s3, 12($sp)
	lw	$s2, 8($sp)
	lw	$s0, 4($sp)
	lw	$ra, 0($sp)
	add	$sp, $sp, 16
	jr	$ra
	
	
# ____ STACK_DUPLICATE ________________________________________
# Duplicate the topmost item on the stack.
#
# Registers:
#	s2 - the item to be duplicated
# -------------------------------------------------------------
stack_duplicate:
	sub	$sp, $sp, 12
	sw	$ra, 0($sp)
	sw	$s0, 4($sp)
	sw	$s2, 8($sp)
	
	# ==== NOT ENOUGH OPERANDS CHECK ========================
	jal	stack_len
	slti	$s0, $v0, 1		# The stack has at least 2 items
	
	bnez	$s0, error_stack_not_enough_operands
	# =======================================================
	
	jal	stack_pop
	move	$s2, $v0
	
	add	$a0, $s2, $zero		
	jal	stack_push
	
	add	$a0, $s2, $zero		# Duplication 
	jal	stack_push
	
	lw	$s2, 8($sp)
	lw	$s0, 4($sp)
	lw	$ra, 0($sp)
	add	$sp, $sp, 12
	jr	$ra
	
	
# ____ STACK_PRINT ____________________________________________
# Print the topmost item on the stack
#
# Registers:
#	s2 - Item to be printed
# -------------------------------------------------------------
stack_print:
	sub	$sp, $sp, 12	
	sw	$ra, 0($sp)
	sw	$s0, 4($sp)
	sw	$s2, 8($sp)
	
	# ==== NOT ENOUGH OPERANDS CHECK ========================
	jal	stack_len
	slti	$s0, $v0, 1		# The stack has at least 2 items
	
	bnez	$s0, error_stack_not_enough_operands
	# =======================================================
	
	jal	stack_pop
	move	$s2, $v0
	
	move	$a0, $s2			# Print the integer popped off the stack
	li	$v0, 1
	syscall
	
	li	$a0, '\n'		# print newline
	li	$v0, 11
	syscall
	
	lw	$s2, 8($sp)
	lw	$s0, 4($sp)
	lw	$ra, 0($sp)
	add	$sp, $sp, 12
	jr	$ra
	
	
# ____ STACK_SWAP ___________________________________________
# Swap the 2 topmost items on the stack
#
# Registers:
#	s2 - 1st operand
#	s3 - 2nd operand
# -------------------------------------------------------------
stack_swap:
	sub	$sp, $sp, 16		
	sw	$ra, 0($sp)
	sw	$s0, 4($sp)
	sw	$s2, 8($sp)
	sw	$s3, 12($sp)
	
	# ==== NOT ENOUGH OPERANDS CHECK ========================
	jal	stack_len
	slti	$s0, $v0, 2		# The stack has at least 2 items
	
	bnez	$s0, error_stack_not_enough_operands
	# =======================================================
	
	jal	stack_pop
	move	$s2, $v0
	
	jal	stack_pop
	move	$s3, $v0
	
	move	$a0, $s2
	jal	stack_push
	
	move	$a0, $s3
	jal	stack_push
	
	lw	$s3, 12($sp)
	lw	$s2, 8($sp)
	lw	$s0, 4($sp)
	lw	$ra, 0($sp)
	add	$sp, $sp, 16
	jr	$ra
	
	
# ____ STACK_ROTATE ___________________________________________
# Rotate the topmost 3 items on the stack
#
# Registers:
#	s2 - 1st operand
#	s3 - 2nd operand
#	s4 - 3rd operand
# -------------------------------------------------------------
stack_rotate:
	sub	$sp, $sp, 20		
	sw	$ra, 0($sp)
	sw	$s0, 4($sp)
	sw	$s2, 8($sp)
	sw	$s3, 12($sp)
	sw	$s4, 16($sp)
	
	# ==== NOT ENOUGH OPERANDS CHECK ========================
	jal	stack_len
	slti	$s0, $v0, 3		# The stack has at least 2 items
	
	bnez	$s0, error_stack_not_enough_operands
	# =======================================================
	
	jal	stack_pop
	move	$s2, $v0
	
	jal	stack_pop
	move	$s3, $v0
	
	jal	stack_pop
	move	$s4, $v0
	
	move	$a0, $s3
	jal	stack_push
	
	move	$a0, $s2
	jal	stack_push
	
	move	$a0, $s4
	jal	stack_push
	
	lw	$s4, 16($sp)
	lw	$s3, 12($sp)
	lw	$s2, 8($sp)
	lw	$s0, 4($sp)
	lw	$ra, 0($sp)
	add	$sp, $sp, 20
	jr	$ra
	
	
# ____ STACK_DROP _____________________________________________
# Drop the topmost item on the stack
#
# Registers:
#	s2 - 1st operand
# -------------------------------------------------------------
stack_drop:
	sub	$sp, $sp, 12
	sw	$ra, 0($sp)
	sw	$s0, 4($sp)
	sw	$s2, 8($sp)
	
	# ==== NOT ENOUGH OPERANDS CHECK ========================
	jal	stack_len
	slti	$t0, $v0, 1		# The stack has at least 2 items
	
	bnez	$t0, error_stack_not_enough_operands
	# =======================================================
	
	jal	stack_pop
	
	lw	$s2, 8($sp)
	lw	$s0, 4($sp)
	lw	$ra, 0($sp)
	add	$sp, $sp, 12
	jr	$ra
	
	
# ____ STACK_AND ______________________________________________
# Compare the 2 topmost items on the stack. If both are
# non-zero, push a 1 onto the stack, else 0.
#
# Stack:
# 	... | item2 | item1 ] 
# 
# item1 & item2 = 1 => Push 1 onto stack
# item1 & item2 = 0 => push 0 onto stack
#
# Registers:
#	s2 - 1st operand
#	s3 - 2nd operand
# -------------------------------------------------------------
stack_and:
	sub	$sp, $sp, 12
	sw	$ra, 0($sp)
	sw	$s0, 4($sp)
	sw	$s2, 8($sp)
	
	# ==== NOT ENOUGH OPERANDS CHECK ========================
	jal	stack_len
	slti	$t0, $v0, 2		# The stack has at least 2 items
	
	bnez	$t0, error_stack_not_enough_operands
	# =======================================================
	
	jal	stack_pop
	move	$s2, $v0
	
	jal	stack_pop
	move	$s3, $v0
	
	and	$a0, $s2, $s3
	jal	stack_push
	
	lw	$s2, 8($sp)
	lw	$s0, 4($sp)
	lw	$ra, 0($sp)
	add	$sp, $sp, 12
	jr	$ra
	
# ____ STACK_OR _______________________________________________
# Compare the 2 topmost items on the stack. If one is a
# non-zero, push a 1 onto the stack, else 0.
#
# Stack:
# 	... | item2 | item1 ] 
# 
# item1 | item2 = 1 => Push 1 onto stack
# item1 | item2 = 0 => push 0 onto stack
#
# Registers:
#	s2 - 1st operand
#	s3 - 2nd operand
# -------------------------------------------------------------
stack_or:
	sub	$sp, $sp, 12
	sw	$ra, 0($sp)
	sw	$s0, 4($sp)
	sw	$s2, 8($sp)
	
	# ==== NOT ENOUGH OPERANDS CHECK ========================
	jal	stack_len
	slti	$t0, $v0, 2		# The stack has at least 2 items
	
	bnez	$t0, error_stack_not_enough_operands
	# =======================================================
	
	jal	stack_pop
	move	$s2, $v0
	
	jal	stack_pop
	move	$s3, $v0
	
	or	$a0, $s2, $s3
	jal	stack_push
	
	lw	$s2, 8($sp)
	lw	$s0, 4($sp)
	lw	$ra, 0($sp)
	add	$sp, $sp, 12
	jr	$ra
	
	
# ____ STACK_XOR _____________________________________
# Compare the 2 topmost items on the stack. If both are
# opposite bits (0 1), push a 1 onto the stack, else 0.
#
# Stack:
# 	... | item2 | item1 ] 
#
# Registers:
#	s2 - 1st operand
#	s3 - 2nd operand
# -------------------------------------------------------------
stack_xor:
	sub	$sp, $sp, 12
	sw	$ra, 0($sp)
	sw	$s0, 4($sp)
	sw	$s2, 8($sp)
	
	# ==== NOT ENOUGH OPERANDS CHECK ========================
	jal	stack_len
	slti	$t0, $v0, 2		# The stack has at least 2 items
	
	bnez	$t0, error_stack_not_enough_operands
	# =======================================================
	
	jal	stack_pop
	move	$s2, $v0
	
	jal	stack_pop
	move	$s3, $v0
	
	xor	$a0, $s2, $s3
	jal	stack_push
	
	lw	$s2, 8($sp)
	lw	$s0, 4($sp)
	lw	$ra, 0($sp)
	add	$sp, $sp, 12
	jr	$ra
	
	
# ____ STACK_INVERT _____________________________________
# Take the topmost item on the stack and invert it.
#
# Stack:
# 	... | item1 ] 
# 
# item1 = 0        -> push 1
# item2 = non-zero -> push 0
#
# Registers:
#	s2 - 1st operand
# -------------------------------------------------------------
stack_invert:
	sub	$sp, $sp, 12
	sw	$ra, 0($sp)
	sw	$s0, 4($sp)
	sw	$s2, 8($sp)
	
	# ==== NOT ENOUGH OPERANDS CHECK ========================
	jal	stack_len
	slti	$t0, $v0, 1		# The stack has at least 2 items
	
	bnez	$t0, error_stack_not_enough_operands
	# =======================================================
	
	jal	stack_pop
	move	$s2, $v0
	
	seq	$a0, $s2, 0
	beq	$a0, 1, si_operate
	
	li	$a0, 0
	
si_operate:
	jal	stack_push
	
	lw	$s2, 8($sp)
	lw	$s0, 4($sp)
	lw	$ra, 0($sp)
	add	$sp, $sp, 12
	jr	$ra
	
	
# ____ STACK_GREATER-THAN _____________________________________
# Compare the 2 topmost items on the stack. Push a 0 or 1 onto
# the stack denoted false, true, respectively. 
#
# Stack:
# 	... | item2 | item1 ] 
# 
# item1 > item2 => Push 1 onto stack
# item1 < item2 => push 0 onto stack
#
# Registers:
#	s2 - 1st operand
# -------------------------------------------------------------
stack_greater:
	sub	$sp, $sp, 12
	sw	$ra, 0($sp)
	sw	$s0, 4($sp)
	sw	$s2, 8($sp)
	
	# ==== NOT ENOUGH OPERANDS CHECK ========================
	jal	stack_len
	slti	$t0, $v0, 2		# The stack has at least 2 items
	
	bnez	$t0, error_stack_not_enough_operands
	# =======================================================
	
	jal	stack_pop
	move	$s2, $v0
	
	jal	stack_pop
	move	$s3, $v0
	
	sgt	$a0, $s3, $s2
	jal	stack_push
	
	lw	$s2, 8($sp)
	lw	$s0, 4($sp)
	lw	$ra, 0($sp)
	add	$sp, $sp, 12
	jr	$ra
	
	
# ____ STACK_LESS-THAN _____________________________________
# Compare the 2 topmost items on the stack. Push a 0 or 1 onto
# the stack denoted false, true, respectively. 
#
# Stack:
# 	... | item2 | item1 ] 
# 
# item1 < item2 => Push 1 onto stack
# item1 > item2 => push 0 onto stack
#
# Registers:
#	s2 - 1st operand
# -------------------------------------------------------------
stack_lesser:
	sub	$sp, $sp, 12
	sw	$ra, 0($sp)
	sw	$s0, 4($sp)
	sw	$s2, 8($sp)
	
	# ==== NOT ENOUGH OPERANDS CHECK ========================
	jal	stack_len
	slti	$t0, $v0, 2		# The stack has at least 2 items
	
	bnez	$t0, error_stack_not_enough_operands
	# =======================================================
	
	jal	stack_pop
	move	$s2, $v0
	
	jal	stack_pop
	move	$s3, $v0
	
	slt	$a0, $s3, $s2
	jal	stack_push
	
	lw	$s2, 8($sp)
	lw	$s0, 4($sp)
	lw	$ra, 0($sp)
	add	$sp, $sp, 12
	jr	$ra
	
	
# ____ STACK_EQUAL-TO _____________________________________
# Compare the 2 topmost items on the stack. Push a 0 or 1 onto
# the stack denoted false, true, respectively. 
#
# Stack:
# 	... | item2 | item1 ] 
# 
# item1 > item2 => Push 1 onto stack
# item1 < item2 => push 0 onto stack
#
# Registers:
#	s2 - 1st operand
# -------------------------------------------------------------
stack_equal:
	sub	$sp, $sp, 12
	sw	$ra, 0($sp)
	sw	$s0, 4($sp)
	sw	$s2, 8($sp)
	
	# ==== NOT ENOUGH OPERANDS CHECK ========================
	jal	stack_len
	slti	$t0, $v0, 2		# The stack has at least 2 items
	
	bnez	$t0, error_stack_not_enough_operands
	# =======================================================
	
	jal	stack_pop
	move	$s2, $v0
	
	jal	stack_pop
	move	$s3, $v0
	
	seq	$a0, $s2, $s3
	jal	stack_push
	
	lw	$s2, 8($sp)
	lw	$s0, 4($sp)
	lw	$ra, 0($sp)
	add	$sp, $sp, 12
	jr	$ra
	
	
# ____ ERROR_STACK_NOT_ENOUGH_OPERANDS ____________________
# Excepts the program with the specified error if, when 
# accessing the stack, there are not enough operands for the 
# currently called function.
# -------------------------------------------------------------
error_stack_not_enough_operands:
	la	$a0, error_msg_stack_not_enough_operands
	li	$v0, 4
	syscall
	
	b	exit

	
# ____ HASH_STR __________________________________________________
# hash a string using the polynomial method. 
#
# Input:
#	a0 - the address to the string to hash
#
# Output:
#	v0 - the hash of the string at a0
#
# Registers:
#	t0 - the hashcode (sum of hashes)
#	t1 - temporary char buffer
# ----------------------------------------------------------------
hash_str:
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	
	li	$t0, 0
	li	$t2, 0
	
hash_read:
	lb	$t1, 0($a0)
	beqz	$t1, hash_exit
	
	sllv	$t1, $t1, $t2		# Multiply by 2
	addi	$t2, $t2, 1
	
	addu	$t0, $t0, $t1
	addi	$a0, $a0, 1
	j	hash_read
	
hash_exit:
	move	$v0, $t0
	
	lw	$ra, 0($sp)
	add	$sp, $sp, 4
	jr	$ra
	

# ____ DETERMINE_IF_ALPHA _____________________________________
# Given a string, determine if it has only letters in it.
# No Digits.
#
# Input:
#	a0 - string to analyze
#
# Output:
#	v0 - 1 <- alpha string
#	     0 <- non-alpha string
#
# Registers:
#	t0 - temporary char buffer
# -------------------------------------------------------------
determine_if_alpha:
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	li	$v0, 1			# it is assumed that it is alpha

dia_read_str:
	lb	$t0, 0($a0)
	addi	$a0, $a0, 1
	beqz	$t0, dia_exit
	blt	$t0, 48, dia_read_str
	bgt	$t0, 57, dia_read_str
	
dia_str_has_digits:
	li	$v0, 0

dia_exit:
	lw	$ra, 0($sp)
	add	$sp, $sp, 4
	jr	$ra
	
	
# ____ DETERMINE_IF_DIGIT _____________________________________
# Given a string, determine if it has ONLY digits in it.
#
# Input:
#	a0 - string to analyze
#
# Output:
#	v0 - 1 <- digit string
#	     0 <- non-digit string
#
# Registers:
#	t0 - temporary char buffer
#	t1 - temporary `conditional if` result
#	t2 - remporary `conditional if` result 
# -------------------------------------------------------------
determine_if_digit:
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	
	li	$v0, 1			# it is assumed that it is a digit str

did_read_str:
	lb	$t0, 0($a0)
	addi	$a0, $a0, 1
	beqz	$t0, did_exit

	# $t0 > 47 && $t0 < 58
	sgt	$t1, $t0, 47		# determine if the char is a digit
	slti	$t2, $t0, 58
	and	$t1, $t1, $t2
	bnez	$t1, did_read_str
	
did_str_is_alpha:
	li	$v0, 0

did_exit:
	lw	$ra, 0($sp)
	add	$sp, $sp, 4
	jr	$ra


# ____ PARSE_DIGIT ____________________________________________
# Given a string, check if it is alpha, if so, except. 
# If not, return the integer representation of the string.
#
# Input:
#	a0 - the string to parse
#
# Output
# 	v0 - the integer representation of the string
#
# Registers:
#	t0 - temporary char buffer
#	t1 - the integer representation of the char in $t0
# -------------------------------------------------------------
parse_digit:
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	jal	determine_if_digit
	lw	$a0, 0($sp)
	add	$sp, $sp, 4
	
	beq	$v0, 1, pd_skip_error		
	# ==== ERROR_PARSING_NON_DIGIT_STR_AS_DIGIT_STR  ========
	la	$a0, error_msg_parsing_non_digit_str_as_digit_str
	li	$v0, 4
	syscall
	# =======================================================
pd_skip_error:
	li	$v0, 0
	
pd_parse_str:
	lb	$t0, 0($a0)
	beqz	$t0, pd_exit
	addi	$a0, $a0, 1
	
	mul	$v0, $v0, 10
	
	sub	$t1, $t0, 48			# Turn the char into an integer
	add	$v0, $v0, $t1
	b	pd_parse_str
	
pd_exit:
	lw	$ra, 0($sp)
	add	$sp, $sp, 4
	jr	$ra
	
	
# ____ VARIABLE CREATE ________________________________________
# Create a variable in the variable space with the name
# $a0, and the value $a1
#
# a0 - hash code of variable name (string)
# a1 - The value associated with the variable
# -------------------------------------------------------------
variable_create:
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)	

	la	$t0, variable_space
	lw	$t1, variable_index
	add	$t0, $t0, $t1

	sw	$a0, 0($t0)		# Store $a0 at the i(th) index of the var space
	
	addi	$t0, $t0, 4 

	sw	$a1, 0($t0)		# Store $a1 at the i+4(th) index of the var space
	
	addi	$t1, $t1, 8
	sw	$t1, variable_index

	lw	$ra, 0($sp)
	addi	$sp, $sp, 4
	jr	$ra
	

# ____ VARIABLE GET ________________________________________
# Get a variable in the variable space named $a0 (hashed)
#
# a0 - hash code of variable name (string) to get 
# -------------------------------------------------------------
variable_get:
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	
	la	$t0, variable_space
	lw	$t1, variable_index
	add	$t1, $t0, $t1
	
	# ==== LOOK THROUGH VARIABLE SPACE, BY INCREMENTS OF 8 ====
vg_loop:
	sub	$t1, $t1, 8
	
	blt	$t1, $t0, no_variable		# has looked through the entire space, no variable found
	
	lw	$t2, 0($t1)
	beq	$a0, $t2, vg_loop_exit
	
	j	vg_loop
	
no_variable:
	la	$a0, error_msg_no_variable_found
	li	$v0, 4
	syscall
	b	exit

vg_loop_exit:
	addi	$t1, $t1, 4
	lw	$v0, 0($t1)			# Get the value at the index that was found!
	
	lw	$ra, 0($sp)
	addi	$sp, $sp, 4
	jr	$ra
	
	
# ____ CONNECT_KEYWORDS _______________________________________
# Given the hashcode of the token read, compare against
# pre-hashed keywords corresponding to the stack procedures
# add, sub, mult, dup, print, etc. Run those functions
#
# Input:
#	a0 - the hashcode of the token read
#	a1 - the addr of the token 
#
# Registers:
#	t0 - temporary hold for $v0
# -------------------------------------------------------------
connect_keyword:
	sub	$sp, $sp, 12
	sw	$s2, 8($sp)
	sw	$s0, 4($sp)
	sw	$ra, 0($sp)
	
	# ---- VARIABLE GET FLAG ----
	
	lw	$t0, keyword_variable_get
	bne	$t0, 1, ck_skip_variable_get
	
	# a0 - hashcode of string variable
	jal	variable_get
	
	move	$a0, $v0
	jal	stack_push
	
	li	$t0, 0
	sw	$t0, keyword_variable_get
	
	b	ck_exit
ck_skip_variable_get:
	
	# ---- VARIABLE SET FLAG ----
	
	lw	$t0, keyword_variable_set
	
	beq	$t0, 1, ck_get_variable_name
	beq	$t0, 2, ck_get_variable_value

	j	ck_skip_variable_set
	
ck_get_variable_name:
	sw	$a0, current_temp_variable

	li	$t0, 2
	sw	$t0, keyword_variable_set
	b	ck_exit

ck_get_variable_value:
	move	$s0, $a0
	
	lw	$a0, current_temp_variable
	
	move	$s2, $a0
	jal	stack_pop
	move	$a0, $s2
	
	move	$a1, $v0
	jal	variable_create

	move	$a0, $s0
	li	$t0, 0
	sw	$t0, keyword_variable_set

	# DO NOT EXIT, WE ARE PAST THE VARIABLE VALUE, AT THE NEXT
	# TOKEN, PARSE THIS TOKEN DOWN THE PIPELINE

ck_skip_variable_set:
	
	# ---- DISREGARD FLAG ----
	lw	$t0, keyword_disregard
	bne	$t0, 1, ck_skip_disregard_flag
	
	bne	$a0, 1608, ck_skip_disregard_then	
	
	li	$t0, 0
	sw	$t0, keyword_disregard
	
ck_skip_disregard_then:
	b	ck_exit

ck_skip_disregard_flag:

	# ---- PRINT-STRING FLAG ----
	lw	$t0, keyword_print_string
	bne	$t0, 1, ck_skip_print_string_flag
	
	bne	$a0, 34, ck_skip_print_string_end
	li	$t0, 0
	sw	$t0, keyword_print_string
	b	ck_exit
	
ck_skip_print_string_end:
	la	$a0, curr_token
	li	$v0, 4
	syscall
	
	li	$a0, 32
	li	$v0, 11
	syscall
	b	ck_exit
	
ck_skip_print_string_flag:

	# ---- COMMENT FLAG ----
	lw	$t0, keyword_comment
	bne	$t0, 1, ck_skip_comment_flag
	
	bne	$a0, 136, ck_comment_loop
	li	$t0, 0
	sw	$t0, keyword_comment
	
ck_comment_loop:
	b	ck_exit
	
ck_skip_comment_flag:

	# IF
	bne	$a0, 309, ck_skip_if		# hashcode("if") = 309
	jal	stack_pop
	xor	$v0, $v0, 1
	sw	$v0, keyword_disregard
	b	ck_exit
ck_skip_if:

	# THEN
	bne	$a0, 1608, ck_skip_then		# hashcode("then") = 1608
	b	ck_exit
ck_skip_then:

	# PRINT-STRING
	bne	$a0, 114, ck_skip_print_string	# hashcode("."") = 114
	li	$t0, 1
	sw	$t0, keyword_print_string
	b	ck_exit
ck_skip_print_string:

	# VARIABLE CREATE
	bne	$a0, 61, ck_skip_var_create	# hashcode("=") = 61
	li	$t0, 1
	sw	$t0, keyword_variable_set
	
	b	ck_exit
ck_skip_var_create:

	# VARIABLE GET
	bne	$a0, 64, ck_skip_var_get		# hashcode("@") = 64
	li	$t0, 1
	sw	$t0, keyword_variable_get
	
	b	ck_exit
ck_skip_var_get:

	# NEW-LINE
	bne	$a0, 326, ck_skip_newline	# hashcode("nk") = 326

	li	$a0, 10
	li	$v0, 11
	syscall
	
	b	ck_exit
ck_skip_newline:

	# ASK-NUMBER
	bne	$a0, 10860, ck_skip_ask_number
	li	$v0, 5
	syscall
	move	$a0, $v0
	jal	stack_push
	b	ck_exit
	
ck_skip_ask_number:

	# DUPLICATE
	bne	$a0, 782, ck_skip_dup 		# hashcode("dup") = 782
	jal	stack_duplicate
	b	ck_exit
ck_skip_dup:

	# COMMENT START
	bne	$a0, 131, ck_skip_comment_start
	li	$t0, 1
	sw	$t0, keyword_comment
	b	ck_exit
ck_skip_comment_start:
	
	# COMMENT END
	bne	$a0, 136, ck_skip_comment_end
	b	ck_exit
ck_skip_comment_end:
	

	# SWAP
	bne	$a0, 1637, ck_skip_swap
	jal	stack_swap
	b	ck_exit
	
ck_skip_swap:

	# ADDITION
	bne	$a0, 43, ck_skip_add		# hashcode("+") = 43
	jal	stack_add
	b	ck_exit
ck_skip_add:

	# SUBTRACT
	bne	$a0, 45, ck_skip_subtract	# hashcode("-") = 45
	jal	stack_subtract
	b	ck_exit
ck_skip_subtract:

	# MULTIPLY
	bne	$a0, 42, ck_skip_multiply	# hashcode("*") = 42
	jal	stack_multiply
	b	ck_exit
ck_skip_multiply:

	# DIVIDE
	bne	$a0, 47, ck_skip_divide		# hashcode("/") = 47
	jal	stack_divide
	b	ck_exit
ck_skip_divide:

	# GREATER-THAN
	bne	$a0, 62, ck_skip_greater		# hashcode(">") = 62
	jal	stack_greater
	b	ck_exit
ck_skip_greater:
	
	# LESS-THAN
	bne	$a0, 60, ck_skip_lesser		# hashcode("<") = 60
	jal	stack_lesser
	b	ck_exit
ck_skip_lesser:

	# AND
	bne	$a0, 38, ck_skip_and		# hashcode("&") = 38
	jal	stack_and
	b	ck_exit
ck_skip_and:
	
	# OR
	bne	$a0, 124, ck_skip_or		# hashcode("|") = 128
	jal	stack_or
	b	ck_exit
ck_skip_or:
	
	# XOR
	bne	$a0, 94, ck_skip_xor		# hashcode("^") = 94
	jal	stack_xor
	b	ck_exit
ck_skip_xor:
	
	# INVERT
	bne	$a0, 33, ck_skip_invert		# hashcode("!") = 33
	jal	stack_invert
	b	ck_exit
ck_skip_invert:
	
	
	# EQUAL
	bne	$a0, 183, ck_skip_equal		# hashcode("==") = 183
	jal	stack_equal
	b	ck_exit
ck_skip_equal:

	# PRINT
	bne	$a0, 46, ck_skip_print		# hashcode(".") = 46
	jal	stack_print
	b	ck_exit
ck_skip_print:

	# ROTATE
	bne	$a0, 800, ck_skip_rotate		# hashcode("rot") = 800
	jal	stack_rotate
	b	ck_exit
ck_skip_rotate:

	# DROP
	bne	$a0, 1668, ck_skip_drop		# hashcode("drop") = 1668
	jal	stack_drop
	b	ck_exit
ck_skip_drop:

	# All of the keywords have been compared to against, 
	# That means the token is an undeclared word, which implies an error:
	
	la	$a0, error_msg_unknown_word	# print error_msg_unknown_word
	li	$v0, 4
	syscall
	
	move	$a0, $a1				# print the unknown word
	li	$v0, 4
	syscall
	
	j	exit				# exit the program (not the procedure)

ck_exit:
	lw	$ra, 0($sp)
	lw	$s0, 4($sp)
	lw	$s2, 8($sp)
	add	$sp, $sp, 12
	jr	$ra