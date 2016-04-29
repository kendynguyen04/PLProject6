import ply.yacc as yacc

# Modified by Jimmy and Ken Nguyen
# Get the token map from the lexer.  This is required.
from lex import tokens

DEBUG = True

# Namespace & built-in functions

name = {}

def cons(l): return [l[0]] + l[1]
name['cons'] = cons

def concat(l): return l[0] + l[1]
name['concat'] = concat

def listar(l): return l
name['list'] = listar

def car(l): return l[0][0]
name['car'] = car

def cdr(l): return l[0][1:]
name['cdr'] = cdr

def eq(l): return l[0] == l[1]
name['eq'] = eq
name['='] = eq

def _and(l): return not False in l
name['and'] = _and

def _or(l): return True in l
name['or'] = _or

def cond(l):
    if l[0]:
        return l[1]
name['cond'] = cond

def add(l):
    sum = 0
    for i in l:
        if str(i) in dict:
            sum = sum + dict[str(i)]
        else:
            sum = sum + i
    return sum
name['+'] = add

def minus(l):
    if str(l[0]) in dict:
        num = dict[str(l[0])]
    else:
        num = l[0]
    for i in l[1:]:
        if str(i) in dict:
            num = num - dict[str(i)]
        else:
            num = num - i
    return num
name['-'] = minus

def mult(l):
    prod = 1
    for i in l:
        if str(i) in dict:
            prod = prod * dict[str(i)]
        else:
            prod = prod * i
    return prod
name['*'] = mult

def divide(l):
    if str(l[0]) in dict:
        quo = dict[str(l[0])]
    else:
        quo = l[0]
    for i in l[1:]:
        if str(i) in dict:
            quo = quo / dict[str(i)]
        else:
            quo = quo / i
    return quo
name['/'] = divide

def _print(l):
    print lisp_str(l[0])
name['print'] = _print

dict = {}
def let(l):
    val = []
    exp = []
    for i in l:
        if i in dict:
           val.append(dict[str(i)])
    for i in l:
        if i in dict:
            del(dict[str(i)])
        if(i == 'Error, variable is undefined.'):
            return i
    else:
        for i in l:
            if i in dict:
                del(dict[str(i)])
            if isinstance(i, str) == False:
                exp.append(i)
        if(len(exp) > 0):
            return exp
        else:
            return val

name['let'] = let

def _if(l):
    print l
    if(l[0] == True or l[0] != 0):
        return l[1]
    elif(l[0] == False):
        return l[2]
    else:
        return "Error encountered. No boolean equivalent or incorrect number of input values."
name['if'] = _if

#  Evaluation functions

def lisp_eval(simb, items):
    if simb in name:
        for i in items:
            if(isinstance(i, str) and i not in dict):
                return ['Error, variable is undefined.'] + items
            else:
                return call(name[simb], eval_lists(items))
    elif(isinstance(simb, str)):
        dict[str(simb)] = items[0]
        return simb
    else:
        return [simb] + items

def call(f, l):
    try:
        return f(eval_lists(l))
    except TypeError:
        return f

def eval_lists(l):
    r = []
    for i in l:
        if is_list(i):
            if i:
                r.append(lisp_eval(i[0], i[1:]))
            else:
                r.append(i)
        else:
            r.append(i)
    return r

# Utilities functions

def is_list(l):
    return type(l) == type([])

def lisp_str(l):
    if type(l) == type([]):
        if not l:
            return "()"
        r = "("
        for i in l[:-1]:
            r += lisp_str(i) + " "
        r += lisp_str(l[-1]) + ")"
        return r
    elif l is True:
        return "#t"
    elif l is False:
        return "#f"
    elif l is None:
        return 'nil'
    else:
        return str(l)

# BNF

def p_exp_atom(p):
    'exp : atom'
    p[0] = p[1]


def p_exp_qlist(p):
    'exp : quoted_list'
    p[0] = p[1]


def p_exp_call(p):
    'exp : call'
    p[0] = p[1]


def p_quoted_list(p):
    'quoted_list : QUOTE list'
    p[0] = p[2]


def p_list(p):
    'list : LPAREN items RPAREN'
    p[0] = p[2]


def p_items(p):
    'items : item items'
    p[0] = [p[1]] + p[2]


def p_items_empty(p):
    'items : empty'
    p[0] = []


def p_empty(p):
    'empty :'
    pass


def p_item_atom(p):
    'item : atom'
    p[0] = p[1]


def p_item_list(p):
    'item : list'
    p[0] = p[1]


def p_item_list(p):
    'item : quoted_list'
    p[0] = p[1]


def p_item_call(p):
    'item : call'
    p[0] = p[1]


def p_item_empty(p):
    'item : empty'
    p[0] = p[1]

def p_call(p):
    'call : LPAREN SIMB items RPAREN'
    if DEBUG: print "Calling", p[2], "with", p[3]
    p[0] = lisp_eval(p[2], p[3])

def p_atom_simbol(p):
    'atom : SIMB'
    p[0] = p[1]


def p_atom_bool(p):
    'atom : bool'
    p[0] = p[1]


def p_atom_num(p):
    'atom : NUM'
    p[0] = p[1]


def p_atom_word(p):
    'atom : TEXT'
    p[0] = p[1]


def p_atom_empty(p):
    'atom :'
    pass


def p_true(p):
    'bool : TRUE'
    p[0] = True


def p_false(p):
    'bool : FALSE'
    p[0] = False


def p_nil(p):
    'atom : NIL'
    p[0] = None


# Error rule for syntax errors
def p_error(p):
    print "Syntax error!! ", p

# Build the parser
# Use this if you want to build the parser using SLR instead of LALR
# yacc.yacc(method="SLR")
yacc.yacc()
