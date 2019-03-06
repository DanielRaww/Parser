#Daniel Raw, CSCI312
import re, sys, string

debug = True
dict = { }
tokens = [ ]


#  Expression class and its subclasses
class Expression( object ):
	def __str__(self):
		return "" #return a string

class BinaryExpr( Expression ):
	def __init__(self, op, left, right):
		self.op = op
		self.left = left
		self.right = right

	def __str__(self):
		return str(self.op) + " " + str(self.left) + " " + str(self.right) #the formation of the return statement

class Number( Expression ):
	def __init__(self, value):
		self.value = value

	def __str__(self):
		return str(self.value) #return a string representation

class String (Expression):
	def __init__(self, String):
		self.String = String

	def __str__(self):
		return str(self.String)

class VarRef (Expression):
	def __init__(self, Identifier):
		self.Identifier = Identifier

	def __str__(self):
		return str(self.Identifier)

class Statement(object):
	def __str__(self):
		return "" #return a String

class Block(Statement):
	def __init__(self, left, right):
		self.left = left
		self.right = right

	def __str__(self):
		return str(self.left) + "\n" + str(self.right)

class WHILEstatement(Statement):
	def __init__(self, expr, block):
		self.expr = expr
		self.block = block

	def __str__(self):
		return "while " + str(self.expr) + "\n" + str(self.block) + "\nendwhile" #the formation of the return statement

class IFstatement(Statement):
	def __init__(self, expr, IFblock, ELSEblock):
		self.expr = expr
		self.IFblock = IFblock
		self.ELSEblock = ELSEblock

	def __str__(self):
		if (self.ELSEblock is not None):
			return "if " + str(self.expr) + "\n" + str(self.IFblock) + "\nelse\n" + str(self.ELSEblock) + "\nendif" #the formation of the return statement
		else:
			return "if " + " " + str(self.expr) + "\n" + str(self.IFblock) + "\nendif" #the formation of the return statement

class Assign(Statement):
	def __init__(self, cur, expr):
		self.cur = cur
		self.expr = expr

	def __str__(self):
		return "= " + str(self.cur) + " " + str(self.expr) #the formation of the return statement

class StmtList(Statement):
	def __init__(self, list):
		self.list = list

	def __str__(self):
		emptyStr = " "
		for i in self.list: #iterates through the entire list and then adds it to the empty string
			emptyStr += str(i) + "\n"
		return emptyStr


def error( msg ):
	#print msg
	sys.exit(msg)

# The "parse" function. This builds a list of tokens from the input string,
# and then hands it to a recursive descent parser for the PAL grammar.

def match(matchtok):
	tok = tokens.peek( )
	if (tok != matchtok): error("Expecting "+ matchtok)
	tokens.next( )
	return tok

def factor( ):
	""" factor     = number | string | ident |  "(" expression ")"  """

	tok = tokens.peek( )
	if debug: print ("Factor: ", tok)
	if re.match(Lexer.number, tok):
		expr = Number(tok)
		tokens.next( )
		return expr
	if tok == "(":
		tokens.next( )
		expr = addExpr( )
		tokens.peek( )
		tok = match(")")
		return expr
	if re.match(Lexer.identifier, tok): #matches the identifier in Lexer
		identifier = VarRef(tok)
		tokens.next()
		return identifier
	if re.match(Lexer.string, tok): #matches the string in Lexer
		astring = String()
		tokens.next()
		return astring
	error("Invalid operand")
	return


def term( ):
	""" term    = factor { ('*' | '/') factor } """

	tok = tokens.peek( )
	if debug: print ("Term: ", tok)
	left = factor( )
	tok = tokens.peek( )
	while tok == "*" or tok == "/":
		tokens.next()
		right = factor( )
		left = BinaryExpr(tok, left, right)
		tok = tokens.peek( )
	return left


def addExpr( ):
	""" addExpr    = term { ('+' | '-') term } """

	tok = tokens.peek( )
	if debug: print ("addExpr: ", tok)
	left = term( )
	tok = tokens.peek( )
	while tok == "+" or tok == "-":
		tokens.next()
		right = term( )
		left = BinaryExpr(tok, left, right)
		tok = tokens.peek( )
	return left


def relationalExpr():
	""" relationExpr = addExpr [ relation addExpr ] """

	tok = tokens.peek( )
	if debug: print("relationalExpr: ", tok) #if debug, then print
	left = addExpr() #left side of the statement
	tok = tokens.peek()
	while re.match(Lexer.relational, tok): #matches to see if it is in relation in lexer
		tokens.next()
		right = addExpr() #right side of the statement
		left = BinaryExpr(tok, left, right) #left side of the statement
		tok = tokens.peek( )
	return left


def andExpr():
	""" andExpr    = relationalExpr { "and" relationalExpr } """

	tok = tokens.peek( )
	if debug: print("andExpr: ", tok) #if debug, then print
	left = relationalExpr() #left side of the statement
	tok = tokens.peek( )
	while tok == "and": #checks to see if the token is and
		tokens.next()
		right = relationalExpr() #right side of the statement
		left = BinaryExpr(tok, left, right) #left side of the statement
		tok = tokens.peek()
	return left


def expression():
	""" expression = andExpr { "or" andExpr } """

	tok = tokens.peek( )
	if debug: print("expression: ", tok) #if debug, then print
	left = andExpr() #left side of the statement
	tok = tokens.peek( )
	while tok == 'or': #checks to see if the token is or
		tokens.next()
		right = andExpr() #right side of the statement
		left = BinaryExpr(tok, left, right) #left side of the statement
		tok = tokens.peek()
	return left

def parseBlock():
	""" block = ":" eoln indent stmtList undent """

	tok = tokens.peek()
	if debug: print("parseBlock: ", tok)
	match (":") #matches ":"
	match(";") #matches eoln
	match("@") #matches indent
	tok = tokens.peek()
	block = parseStmt() #calls parseStmt
	tok = tokens.peek()
	while (tok != "~"): #if the token is not a tilda
		oldblock = block
		block = Block(oldblock, parseStmt())
		tok = tokens.peek()
	match("~")
	return block #return the block representatin


def parseWhile():
	""" whileStatement = "while"  expression  block """

	tok = tokens.peek()
	if debug: print("parseWhile: ", tok)
	if (tok == "while"): #if the token says while
		match("while") #match while
		expr = expression()
		block = parseBlock()
		return WHILEstatement(expr, block) #call Whilestatement putting expression and parseBlock in the parameters
	error ("INVALID WHILE STATEMENT")


def parseIf():
	""" ifStatement = "if" expression block   [ "else" block ] """

	tok = tokens.peek()
	if debug: print("parseIf: ", tok)
	if (tok == "if"): #if the token says if
		tokens.next()
		expr = expression()
		block_if = parseBlock()
		tok = tokens.peek()
		if (tok == "else"): #if the token says else
			match("else")
			block_else = parseBlock()
			tok = tokens.peek()
		else:
			block_else = None
		return IFstatement(expr, block_if, block_else) #calls IFstatement putting expression, parseBlock, and parseBlock in the parameters
	error ("INVALID IF/ELSE STATEMENT")


def parseAssign():
	""" assign = ident "=" expression  eoln """

	tok = tokens.peek()
	if debug: print("parseAssign: ", tok)
	if re.match(Lexer.identifier, tok): #matches the identifier in Lexer
		identi = VarRef(tok)
	tok = tokens.next()
	match("=")	#matches =
	expr = expression()
	match(";") #marches eoln
	tok = tokens.peek()
	return Assign(identi, expr) #calls Assign putting VarRef and parse:expression in the parameters


def parseStmt():
	""" statement = ifStatement |  whileStatement  |  assign """

	tok = tokens.peek()
	if debug: print("parseStmt: ", tok)
	if (tok == "if"): #when the token is at if
		return parseIf() #call parseIF
	if (tok == "while"): #when the token is at while
		return parseWhile() #call parseWhile
	recAssignment = parseAssign()
	return recAssignment #return parseAssign


def parseStmtList(  ):
	""" gee = { Statement } """

	tok = tokens.peek( )
	emptylist = [ ] #create an empty list
	while tok is not None:
                # need to store each statement in a list
		ast = parseStmt()
		emptylist.append(ast) #append parseStmt into the empty list
		tok = tokens.peek()
	return StmtList(emptylist)
		#print(str(ast))
	#return ast

def parse( text ) :
	global tokens
	tokens = Lexer( text )
	#	expr = expression( )
	#	print (str(expr))
	#     Or:
	stmtlist = parseStmtList( )
	print(str(stmtlist))
	return


# Lexer, a private class that represents lists of tokens from a Gee
# statement. This class provides the following to its clients:
#
#   o A constructor that takes a string representing a statement
#       as its only parameter, and that initializes a sequence with
#       the tokens from that string.
#
#   o peek, a parameterless message that returns the next token
#       from a token sequence. This returns the token as a string.
#       If there are no more tokens in the sequence, this message
#       returns None.
#
#   o removeToken, a parameterless message that removes the next
#       token from a token sequence.
#
#   o __str__, a parameterless message that returns a string representation
#       of a token sequence, so that token sequences can print nicely

class Lexer :


	# The constructor with some regular expressions that define Gee's lexical rules.
	# The constructor uses these expressions to split the input expression into
	# a list of substrings that match Gee tokens, and saves that list to be
	# doled out in response to future "peek" messages. The position in the
	# list at which to dole next is also saved for "nextToken" to use.

	special = r"\(|\)|\[|\]|,|:|;|@|~|;|\$"
	relational = "<=?|>=?|==?|!="
	arithmetic = "\+|\-|\*|/"
	#char = r"'."
	string = r"'[^']*'" + "|" + r'"[^"]*"'
	number = r"\-?\d+(?:\.\d+)?"
	literal = string + "|" + number
	#idStart = r"a-zA-Z"
	#idChar = idStart + r"0-9"
	#identifier = "[" + idStart + "][" + idChar + "]*"
	identifier = "[a-zA-Z]\w*"
	lexRules = literal + "|" + special + "|" + relational + "|" + arithmetic + "|" + identifier

	def __init__( self, text ) :
		self.tokens = re.findall( Lexer.lexRules, text )
		self.position = 0
		self.indent = [ 0 ]


	# The peek method. This just returns the token at the current position in the
	# list, or None if the current position is past the end of the list.

	def peek( self ) :
		if self.position < len(self.tokens) :
			return self.tokens[ self.position ]
		else :
			return None


	# The removeToken method. All this has to do is increment the token sequence's
	# position counter.

	def next( self ) :
		self.position = self.position + 1
		return self.peek( )


	# An "__str__" method, so that token sequences print in a useful form.

	def __str__( self ) :
		return "<Lexer at " + str(self.position) + " in " + str(self.tokens) + ">"



def chkIndent(line):
	ct = 0
	for ch in line:
		if ch != " ": return ct
		ct += 1
	return ct


def delComment(line):
	pos = line.find("#")
	if pos > -1:
		line = line[0:pos]
		line = line.rstrip()
	return line

def mklines(filename):
	inn = open(filename, "r")
	lines = [ ]
	pos = [0]
	ct = 0
	for line in inn:
		ct += 1
		line = line.rstrip( )+";"
		line = delComment(line)
		if len(line) == 0 or line == ";": continue
		indent = chkIndent(line)
		line = line.lstrip( )
		if indent > pos[-1]:
			pos.append(indent)
			line = '@' + line
		elif indent < pos[-1]:
			while indent < pos[-1]:
				del(pos[-1])
				line = '~' + line
		print (ct, "\t", line)
		lines.append(line)
	# print len(pos)
	undent = ""
	for i in pos[1:]:
		undent += "~"
	lines.append(undent)
	# print undent
	return lines



def main():
	"""main program for testing"""
	global debug
	ct = 0
	for opt in sys.argv[1:]:
		if opt[0] != "-": break
		ct = ct + 1
		if opt == "-d": debug = True
	if len(sys.argv) < 2+ct:
		print ("Usage:  %s filename" % sys.argv[0])
		return
	parse("".join(mklines(sys.argv[1+ct])))
	return


main()
