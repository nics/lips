package lips

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"
)

// reader chars
const (
	charsBlank      = " \t\r\n"
	charsSymbol     = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!#$%&+*/:<=>?@^_|~.\\"
	charsNumber     = "0123456789"
	charsString     = "\""
	charsList       = "([{"
	charsSemicolon  = ";"
	charsQuote      = "'"
	charsQuasiquote = "`"
	charsUnquote    = ","
	charsSign       = "-"
)

const prelude = `
        (define nil ())
        (define quote (flambda (form) (car form)))
        (define caar (lambda (x) (car (car x))))
        (define cadr (lambda (x) (car (cdr x))))
        (define cdar (lambda (x) (cdr (car x))))
        (define cddr (lambda (x) (cdr (cdr x))))
        (define cadar (lambda (x) (car (cdr (car x)))))
        (define caddar (lambda (x) (car (cdr (cdr (car x))))))
        (define list (lambda args args))
        (define assqval (lambda (key alist) (cdr (assq key alist))))`

// errors

type FatalError string
type Error string

func (e FatalError) Error() string { return string(e) }
func (e Error) Error() string      { return string(e) }

func NewFatalError(s string) FatalError { return FatalError(s) }
func NewError(s string) Error           { return Error(s) }

// interpreter

type ReaderFunc func(*Interpreter, *bufio.Reader, byte) (Cell, error)

type Interpreter struct {
	readers []ReaderFunc
	symbols map[string]Cell
	Globals Cell
	special Cell
	nothing Cell
}

func NewInterpreter() *Interpreter {
	self := new(Interpreter)

	self.readers = make([]ReaderFunc, 256)
	self.symbols = make(map[string]Cell)
	self.special = Cons(self.Symbol("*syntax-table*"), nil)
	self.nothing = Cons(nil, nil)

	for char, _ := range self.readers {
		self.readers[char] = readUnknown
	}
	for _, char := range []byte(charsBlank) {
		self.readers[char] = readBlank
	}
	for _, char := range []byte(charsSymbol) {
		self.readers[char] = readSymbol
	}
	for _, char := range []byte(charsNumber) {
		self.readers[char] = readNumber
	}
	for _, char := range []byte(charsString) {
		self.readers[char] = readString
	}
	for _, char := range []byte(charsList) {
		self.readers[char] = readList
	}
	for _, char := range []byte(charsSemicolon) {
		self.readers[char] = readSemicolon
	}
	for _, char := range []byte(charsQuote) {
		self.readers[char] = readQuote
	}
	for _, char := range []byte(charsQuasiquote) {
		self.readers[char] = readQuasiquote
	}
	for _, char := range []byte(charsUnquote) {
		self.readers[char] = readUnquote
	}
	for _, char := range []byte(charsSign) {
		self.readers[char] = readSign
	}

	self.Symbol("quote")
	self.Symbol("quasiquote")
	self.Symbol("unquote")
	self.Symbol("unquote-splicing")

	self.Globals = Cons(Cons(self.Symbol("t"), self.Symbol("t")), self.Globals)
	self.Globals = Cons(Cons(self.Symbol("define"), Fubr(fubr_define)), self.Globals)
	self.Globals = Cons(Cons(self.Symbol("eval"), Subr(subr_eval)), self.Globals)
	self.Globals = Cons(Cons(self.Symbol("apply"), Subr(subr_apply)), self.Globals)
	self.Globals = Cons(Cons(self.Symbol("flambda"), Fubr(fubr_fxpr)), self.Globals)
	self.Globals = Cons(Cons(self.Symbol("lambda"), Fubr(fubr_expr)), self.Globals)
	self.Globals = Cons(Cons(self.Symbol("cons"), Subr(subr_cons)), self.Globals)
	self.Globals = Cons(Cons(self.Symbol("car"), Subr(subr_car)), self.Globals)
	self.Globals = Cons(Cons(self.Symbol("cdr"), Subr(subr_cdr)), self.Globals)
	self.Globals = Cons(Cons(self.Symbol("rplaca"), Subr(subr_rplaca)), self.Globals)
	self.Globals = Cons(Cons(self.Symbol("rplacd"), Subr(subr_rplacd)), self.Globals)
	self.Globals = Cons(Cons(self.Symbol("assq"), Subr(subr_assq)), self.Globals)
	self.Globals = Cons(Cons(self.Symbol("setq"), Fubr(fubr_setq)), self.Globals)
	self.Globals = Cons(Cons(self.Symbol("let"), Fubr(fubr_let)), self.Globals)
	self.Globals = Cons(Cons(self.Symbol("while"), Fubr(fubr_while)), self.Globals)
	self.Globals = Cons(Cons(self.Symbol("if"), Fubr(fubr_if)), self.Globals)
	self.Globals = Cons(Cons(self.Symbol("map"), Subr(subr_map)), self.Globals)
	self.Globals = Cons(Cons(self.Symbol("+"), Subr(subr_add)), self.Globals)
	self.Globals = Cons(Cons(self.Symbol("-"), Subr(subr_subtract)), self.Globals)
	self.Globals = Cons(Cons(self.Symbol("*"), Subr(subr_multiply)), self.Globals)
	self.Globals = Cons(Cons(self.Symbol("/"), Subr(subr_divide)), self.Globals)
	self.Globals = Cons(Cons(self.Symbol("%"), Subr(subr_modulus)), self.Globals)
	self.Globals = Cons(Cons(self.Symbol("<"), Subr(subr_less)), self.Globals)
	self.Globals = Cons(Cons(self.Symbol(">"), Subr(subr_more)), self.Globals)
	self.Globals = Cons(Cons(self.Symbol("<="), Subr(subr_lessOrEqual)), self.Globals)
	self.Globals = Cons(Cons(self.Symbol(">="), Subr(subr_moreOrEqual)), self.Globals)
	self.Globals = Cons(Cons(self.Symbol("=="), Subr(subr_equal)), self.Globals)
	self.Globals = Cons(Cons(self.Symbol("!="), Subr(subr_notEqual)), self.Globals)
	self.Globals = Cons(Cons(self.Symbol("and"), Fubr(fubr_and)), self.Globals)
	self.Globals = Cons(Cons(self.Symbol("or"), Fubr(fubr_or)), self.Globals)
	self.Globals = Cons(Cons(self.Symbol("println"), Subr(subr_println)), self.Globals)
	self.Globals = Cons(Cons(self.Symbol("print"), Subr(subr_print)), self.Globals)
	self.Globals = Cons(Cons(self.Symbol("load"), Subr(subr_load)), self.Globals)
	self.Globals = Cons(Cons(self.Symbol("exit"), Subr(subr_exit)), self.Globals)
	self.Globals = Cons(self.special, self.Globals)

	self.ReadString(prelude)

	return self
}

func (self *Interpreter) Symbol(name string) Cell {
	cell, is := self.symbols[name]
	if !is {
		cell = Symbol(name)
		self.symbols[name] = cell
	}
	return cell
}

func (self *Interpreter) ReadString(code string) (cell Cell, e error) {
	return self.Read(strings.NewReader(code))
}

func (self *Interpreter) ReadFile(path string) (cell Cell, e error) {
	file, e := os.OpenFile(path, 0, 0)
	if file != nil {
		cell, e = self.Read(file)
		file.Close()
	}
	return
}

func (self *Interpreter) Read(r io.Reader) (cell Cell, e error) {
	in := bufio.NewReader(r)

	var expr Cell
	for expr, e = self.ReadExpression(in); e == nil; expr, e = self.ReadExpression(in) {
		cell, e = self.Eval(expr, self.Globals)
	}
	if e == io.EOF {
		e = nil
	}
	return
}

func (self *Interpreter) ReadExpression(in *bufio.Reader) (cell Cell, e error) {
	var char byte
	for e == nil && cell == nil {
		if char, e = skipBlanks(self, in); e == nil {
			if cell, e = self.readers[char](self, in, char); cell == self.nothing {
				cell = nil
				break
			}
		}
	}
	return
}

func (self *Interpreter) Eval(expr Cell, env Cell) (cell Cell, e error) {
	switch expr.(type) {
	case *TypeString, *TypeNumber, *TypeExpr, *TypeSubr, *TypeFubr:
		cell = expr
	case *TypeSymbol:
		if cell = Assq(expr, env); cell == nil {
			e = NewError(fmt.Sprintf("Error: undefined %s.", Sexp(expr)))
		}
		cell = Cdr(cell)
	case *TypeCons:
		cell, e = self.Eval(Car(expr), env)
		cell, e = self.Apply(cell, Cdr(expr), env)
	}
	return
}

func (self *Interpreter) Apply(fun, args Cell, env Cell) (cell Cell, e error) {
	switch fun.(type) {
	case *TypeFubr:
		cell, e = fun.(*TypeFubr).ptr(self, args, env)
	case *TypeSubr:
		cell, e = self.evalArgs(args, env)
		cell, e = fun.(*TypeSubr).ptr(self, cell, env)
	case *TypeFxpr:
		cell, e = self.pairList(Car(fun.(*TypeFxpr).one), Cons(args, Cons(env, nil)), fun.(*TypeFxpr).two)
		cell, e = self.evalList(Cdr(fun.(*TypeFxpr).one), cell)
	case *TypeExpr:
		cell, e = self.evalArgs(args, env)
		cell, e = self.pairList(Car(fun.(*TypeExpr).one), cell, fun.(*TypeExpr).two)
		cell, e = self.evalList(Cdr(fun.(*TypeExpr).one), cell)
	default:
		e = NewError(fmt.Sprintf("Error: cannot apply %s.", Sexp(fun)))
	}
	return
}

func (self *Interpreter) evalArgs(args Cell, env Cell) (cell Cell, e error) {
	if args == nil {
		return
	}
	head, e := self.Eval(Car(args), env)
	tail, e := self.evalArgs(Cdr(args), env)
	cell = Cons(head, tail)
	return
}

func (self *Interpreter) evalList(expr Cell, env Cell) (cell Cell, e error) {
	for ; expr != nil; expr = Cdr(expr) {
		cell, e = self.Eval(Car(expr), env)
	}
	return
}

func (self *Interpreter) pairList(expr Cell, args Cell, env Cell) (cell Cell, e error) {
	cell = env
	if _, is := expr.(*TypeCons); is {
		for ; expr != nil; expr, args = Cdr(expr), Cdr(args) {
			cell = Cons(Cons(Car(expr), Car(args)), cell)
		}
		return
	}
	cell = Cons(Cons(expr, args), cell)
	return
}

// cell

type Cell interface {
	Sexp() string
}

type TypeString string
type TypeSymbol string
type TypeNumber int
type TypeCons Pair
type TypeFxpr Pair
type TypeExpr Pair
type TypeFubr struct {
	ptr Func
}
type TypeSubr struct {
	ptr Func
}

type Func func(*Interpreter, Cell, Cell) (Cell, error)
type Pair struct {
	one Cell
	two Cell
}

func (self *TypeString) Sexp() string { return fmt.Sprintf("\"%s\"", string(*self)) }
func (self *TypeSymbol) Sexp() string { return string(*self) }
func (self *TypeNumber) Sexp() string { return fmt.Sprintf("%d", int(*self)) }
func (self *TypeFxpr) Sexp() string   { return fmt.Sprintf("(flambda %s)", self.one.Sexp()) }
func (self *TypeExpr) Sexp() string   { return fmt.Sprintf("(lambda %s)", self.one.Sexp()) }
func (self *TypeFubr) Sexp() string   { return fmt.Sprintf("<fsubr %p>", self.ptr) }
func (self *TypeSubr) Sexp() string   { return fmt.Sprintf("<subr %p>", self.ptr) }
func (self *TypeCons) Sexp() string {
	cell := Cell(self)
	sexp := "("
	for is := true; is; _, is = cell.(*TypeCons) {
		sexp += Sexp(Car(cell))
		if cell = Cdr(cell); cell != nil {
			sexp += " "
		}
	}
	if cell != nil {
		sexp += ". " + cell.Sexp()
	}
	sexp += ")"
	return sexp
}

func Sexp(cell Cell) string {
	if cell != nil {
		return cell.Sexp()
	}
	return "nil"
}

func asString(cell Cell) (s string, e error) {
	if cast, is := cell.(*TypeString); is {
		s = string(*cast)
	} else {
		e = NewError(fmt.Sprintf("Error: %s is not a string", Sexp(cell)))
	}
	return
}

func asInt(cell Cell) (i int, e error) {
	if cast, is := cell.(*TypeNumber); is {
		i = int(*cast)
	} else {
		e = NewError(fmt.Sprintf("Error: %s is not a number", Sexp(cell)))
	}
	return
}

func Symbol(val string) Cell {
	cast := TypeSymbol(val)
	return &cast
}
func String(val string) Cell {
	cast := TypeString(val)
	return &cast
}
func Number(val int) Cell {
	cast := TypeNumber(val)
	return &cast
}
func Fubr(ptr Func) Cell           { return &TypeFubr{ptr} }
func Subr(ptr Func) Cell           { return &TypeSubr{ptr} }
func Fxpr(one Cell, two Cell) Cell { return &TypeFxpr{one, two} }
func Expr(one Cell, two Cell) Cell { return &TypeExpr{one, two} }
func Cons(one Cell, two Cell) Cell { return &TypeCons{one, two} }

func Car(cons Cell) (cell Cell) {
	if cons, is := cons.(*TypeCons); is {
		cell = cons.one
	}
	return
}

func Cdr(cons Cell) (cell Cell) {
	if cons, is := cons.(*TypeCons); is {
		cell = cons.two
	}
	return
}

func Caar(cell Cell) Cell { return Car(Car(cell)) }
func Cadr(cell Cell) Cell { return Car(Cdr(cell)) }
func Cdar(cell Cell) Cell { return Cdr(Car(cell)) }

func Rplaca(cons Cell, cell Cell) Cell {
	if cons, is := cons.(*TypeCons); is {
		cons.one = cell
	}
	return cell
}

func Rplacd(cons Cell, cell Cell) Cell {
	if cons, is := cons.(*TypeCons); is {
		cons.two = cell
	}
	return cell
}

func Assq(cell Cell, list Cell) Cell {
	for list != nil {
		if cell == Caar(list) {
			return Car(list)
		}
		list = Cdr(list)
	}
	return nil
}

// builtin

func fubr_define(lips *Interpreter, args Cell, env Cell) (cell Cell, e error) {
	if args == nil {
		return
	}
	cell = Cons(Car(args), nil)
	Rplacd(lips.Globals, Cons(cell, Cdr(lips.Globals)))
	expr, e := lips.Eval(Cadr(args), env)
	cell = Rplacd(cell, expr)
	return
}

func subr_eval(lips *Interpreter, args Cell, env Cell) (cell Cell, e error) {
	if evalEnv := Cadr(args); evalEnv != nil {
		cell, e = lips.Eval(Car(args), evalEnv)
	} else {
		cell, e = lips.Eval(Car(args), env)
	}
	return
}

func subr_apply(lips *Interpreter, args Cell, env Cell) (cell Cell, e error) {
	cell, e = lips.Apply(Car(args), Cdr(args), env)
	return
}

func fubr_setq(lips *Interpreter, args Cell, env Cell) (cell Cell, e error) {
	key := Car(args)
	if _, is := key.(*TypeSymbol); is {
		cell, e = lips.Eval(Cadr(args), env)
		if tmp := Assq(key, env); tmp != nil {
			Rplacd(tmp, cell)
		} else {
			e = NewError(fmt.Sprintf("Error: undefined %s.", Sexp(key)))
		}
	}
	return
}

func fubr_let(lips *Interpreter, args Cell, env Cell) (cell Cell, e error) {
	var tmp Cell
	for cell = Car(args); cell != nil; cell = Cdr(cell) {
		tmp, e = lips.Eval(Car(Cdar(cell)), env)
		tmp = Cons(Caar(cell), tmp)
		env = Cons(tmp, env)
	}
	cell, e = lips.evalList(Cdr(args), env)
	return
}

func fubr_while(lips *Interpreter, args Cell, env Cell) (cell Cell, e error) {
	var expr Cell
	for expr, e = lips.Eval(Car(args), env); expr != nil; expr, e = lips.Eval(Car(args), env) {
		cell, e = lips.evalList(Cdr(args), env)
	}
	return
}

func fubr_if(lips *Interpreter, args Cell, env Cell) (cell Cell, e error) {
	var expr Cell
	if expr, e = lips.Eval(Car(args), env); expr != nil {
		cell, e = lips.Eval(Cadr(args), env)
	} else {
		cell, e = lips.Eval(Cadr(Cdr(args)), env)
	}
	return
}

func subr_map(lips *Interpreter, args Cell, env Cell) (cell Cell, e error) {
	head := Cons(nil, nil)
	tail := head
	expr := Car(args)

	for args = Cdar(args); args != nil; args = Car(args) {
		cell, e = lips.Apply(expr, mapArgs(args), env)
		tail = Rplacd(tail, Cons(cell, nil))
	}
	cell = Cdr(head)
	return
}
func mapArgs(args Cell) Cell {
	if args != nil {
		cell := Caar(args)
		Rplaca(args, Cdar(args))
		tail := mapArgs(Cdr(args))
		return Cons(cell, tail)
	}
	return nil
}

func fubr_fxpr(lips *Interpreter, args Cell, env Cell) (cell Cell, e error) {
	cell = Fxpr(args, env)
	return
}
func fubr_expr(lips *Interpreter, args Cell, env Cell) (cell Cell, e error) {
	cell = Expr(args, env)
	return
}
func subr_cons(lips *Interpreter, args Cell, env Cell) (cell Cell, e error) {
	cell = Cons(Car(args), Cadr(args))
	return
}
func subr_car(lips *Interpreter, args Cell, env Cell) (cell Cell, e error) {
	cell = Caar(args)
	return
}
func subr_cdr(lips *Interpreter, args Cell, env Cell) (cell Cell, e error) {
	cell = Cdar(args)
	return
}
func subr_rplaca(lips *Interpreter, args Cell, env Cell) (cell Cell, e error) {
	cell = Rplaca(Car(args), Cadr(args))
	return
}
func subr_rplacd(lips *Interpreter, args Cell, env Cell) (cell Cell, e error) {
	cell = Rplacd(Car(args), Cadr(args))
	return
}
func subr_assq(lips *Interpreter, args Cell, env Cell) (cell Cell, e error) {
	cell = Assq(Car(args), Cadr(args))
	return
}

func subr_add(lips *Interpreter, args Cell, env Cell) (cell Cell, e error) {
	number, e := asInt(Car(args))
	var n int
	for args = Cdr(args); e == nil && args != nil; args = Cdr(args) {
		n, e = asInt(Car(args))
		number += n
	}
	cell = Number(number)
	return
}

func subr_subtract(lips *Interpreter, args Cell, env Cell) (cell Cell, e error) {
	number, e := asInt(Car(args))
	if Cdr(args) != nil {
		var n int
		for args = Cdr(args); e == nil && args != nil; args = Cdr(args) {
			n, e = asInt(Car(args))
			number -= n
		}
	} else {
		number = 0 - number
	}
	cell = Number(number)
	return
}

func subr_multiply(lips *Interpreter, args Cell, env Cell) (cell Cell, e error) {
	number, e := asInt(Car(args))
	var n int
	for args = Cdr(args); e == nil && args != nil; args = Cdr(args) {
		n, e = asInt(Car(args))
		number *= n
	}
	cell = Number(number)
	return
}

func subr_divide(lips *Interpreter, args Cell, env Cell) (cell Cell, e error) {
	number, e := asInt(Car(args))
	if Cdr(args) != nil {
		var n int
		for args = Cdr(args); e == nil && args != nil; args = Cdr(args) {
			n, e = asInt(Car(args))
			number /= n
		}
	} else {
		number = 1 / number
	}
	cell = Number(number)
	return
}

func subr_modulus(lips *Interpreter, args Cell, env Cell) (cell Cell, e error) {
	number, e := asInt(Car(args))
	if Cdr(args) != nil {
		var n int
		for args = Cdr(args); e == nil && args != nil; args = Cdr(args) {
			n, e = asInt(Car(args))
			number %= n
		}
	} else {
		number = 1 % number
	}
	cell = Number(number)
	return
}

func subr_less(lips *Interpreter, args Cell, env Cell) (cell Cell, e error) {
	for ; Cdr(args) != nil; args = Cdr(args) {
		var n1, n2 int
		n1, e = asInt(Car(args))
		n2, e = asInt(Cadr(args))
		if e != nil || !(n1 < n2) {
			return
		}
	}
	cell = lips.Symbol("t")
	return
}

func subr_more(lips *Interpreter, args Cell, env Cell) (cell Cell, e error) {
	for ; Cdr(args) != nil; args = Cdr(args) {
		var n1, n2 int
		n1, e = asInt(Car(args))
		n2, e = asInt(Cadr(args))
		if e != nil || !(n1 > n2) {
			return
		}
	}
	cell = lips.Symbol("t")
	return
}

func subr_lessOrEqual(lips *Interpreter, args Cell, env Cell) (cell Cell, e error) {
	for ; Cdr(args) != nil; args = Cdr(args) {
		var n1, n2 int
		n1, e = asInt(Car(args))
		n2, e = asInt(Cadr(args))
		if e != nil || !(n1 <= n2) {
			return
		}
	}
	cell = lips.Symbol("t")
	return
}

func subr_moreOrEqual(lips *Interpreter, args Cell, env Cell) (cell Cell, e error) {
	for ; Cdr(args) != nil; args = Cdr(args) {
		var n1, n2 int
		n1, e = asInt(Car(args))
		n2, e = asInt(Cadr(args))
		if e != nil || !(n1 >= n2) {
			return
		}
	}
	cell = lips.Symbol("t")
	return
}

func subr_equal(lips *Interpreter, args Cell, env Cell) (cell Cell, e error) {
	for ; Cdr(args) != nil; args = Cdr(args) {
		var n1, n2 int
		n1, e = asInt(Car(args))
		n2, e = asInt(Cadr(args))
		if e != nil || n1 != n2 {
			return
		}
	}
	cell = lips.Symbol("t")
	return
}

func subr_notEqual(lips *Interpreter, args Cell, env Cell) (cell Cell, e error) {
	for ; Cdr(args) != nil; args = Cdr(args) {
		var n1, n2 int
		n1, e = asInt(Car(args))
		n2, e = asInt(Cadr(args))
		if e != nil || n1 == n2 {
			return
		}
	}
	cell = lips.Symbol("t")
	return
}

func fubr_and(lips *Interpreter, args Cell, env Cell) (cell Cell, e error) {
	cell = lips.Symbol("t")
	for ; args != nil && cell != nil; args = Cdr(args) {
		cell, e = lips.Eval(Car(args), env)
	}
	return
}

func fubr_or(lips *Interpreter, args Cell, env Cell) (cell Cell, e error) {
	for ; args != nil && cell == nil; args = Cdr(args) {
		cell, e = lips.Eval(Car(args), env)
	}
	return
}

func subr_println(lips *Interpreter, args Cell, env Cell) (cell Cell, e error) {
	for ; args != nil; args = Cdr(args) {
		fmt.Print(Sexp(Car(args)))
		if Cdr(args) != nil {
			fmt.Print(" ")
		}
	}
	fmt.Print("\n")
	return
}

func subr_print(lips *Interpreter, args Cell, env Cell) (cell Cell, e error) {
	for ; args != nil; args = Cdr(args) {
		fmt.Print(Sexp(Car(args)))
		if Cdr(args) != nil {
			fmt.Print(" ")
		}
	}
	return
}

func subr_load(lips *Interpreter, args Cell, env Cell) (cell Cell, e error) {
	if path, e := asString(Car(args)); e == nil {
		cell, e = lips.ReadFile(path)
	}
	return
}

func subr_exit(lips *Interpreter, args Cell, env Cell) (cell Cell, e error) {
	c := 0
	if cell = Car(args); cell != nil {
		c, e = asInt(cell)
	}
	if e == nil {
		os.Exit(c)
	}
	return
}

// readers

func skipBlanks(self *Interpreter, in *bufio.Reader) (char byte, e error) {
	for char, e = in.ReadByte(); e == nil && charReadsBlank(self, char); char, e = in.ReadByte() {
	}
	return
}

func charReadsBlank(self *Interpreter, char byte) bool {
	return char == ' ' || char == '\t' || char == '\r' || char == '\n'
}
func charReadsSymbol(self *Interpreter, char byte) bool {
	return (char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z') ||
		char == '!' || char == '#' || char == '$' || char == '%' ||
		char == '&' || char == '+' || char == '*' || char == '/' ||
		char == ':' || char == '<' || char == '=' || char == '>' ||
		char == '?' || char == '@' || char == '^' || char == '_' ||
		char == '|' || char == '~' || char == '.' || char == '\\'
}
func charReadsNumber(self *Interpreter, char byte) bool {
	return char >= '0' && char <= '9'
}
func charReadsString(self *Interpreter, char byte) bool {
	return char == '"'
}
func charReadsList(self *Interpreter, char byte) bool {
	return char == '(' || char == '[' || char == '{'
}
func charReadsSemicolon(self *Interpreter, char byte) bool {
	return char == ';'
}
func charReadsQuote(self *Interpreter, char byte) bool {
	return char == '\''
}
func charReadsQuasiquote(self *Interpreter, char byte) bool {
	return char == '`'
}
func charReadsUnquote(self *Interpreter, char byte) bool {
	return char == ','
}
func charReadsSign(self *Interpreter, char byte) bool {
	return char == '-'
}

func readUnknown(self *Interpreter, in *bufio.Reader, char byte) (cell Cell, e error) {
	e = NewError(fmt.Sprintf("Error: illegal character %c.", char))
	return
}

func readSymbol(self *Interpreter, in *bufio.Reader, char byte) (cell Cell, e error) {
	buffer := bytes.NewBuffer(nil)
	buffer.WriteByte(char)
	for char, e = in.ReadByte(); e == nil && (charReadsSymbol(self, char) ||
		charReadsNumber(self, char) || charReadsSign(self, char)); char, e = in.ReadByte() {
		buffer.WriteByte(char)
	}
	in.UnreadByte()
	cell = self.Symbol(buffer.String())
	return
}

func readNumber(self *Interpreter, in *bufio.Reader, char byte) (cell Cell, e error) {
	buffer := bytes.NewBuffer(nil)
	buffer.WriteByte(char)
	for char, e = in.ReadByte(); e == nil && charReadsNumber(self, char); char, e = in.ReadByte() {
		buffer.WriteByte(char)
	}
	in.UnreadByte()
	if number, e := strconv.Atoi(buffer.String()); e == nil {
		cell = Number(number)
	}
	return
}

func readString(self *Interpreter, in *bufio.Reader, char byte) (cell Cell, e error) {
	buffer := bytes.NewBuffer(nil)
	ending := char
	escape := false
	for char, e = in.ReadByte(); e == nil && (escape || char != ending); char, e = in.ReadByte() {
		escape = !escape && char == '\\'
		buffer.WriteByte(char)
	}
	if e == io.EOF {
		e = NewError("Error: EOF in string.")
	}
	cell = String(buffer.String())
	return
}

func readList(self *Interpreter, in *bufio.Reader, char byte) (head Cell, e error) {
	var tail Cell
	var cell Cell

	var end byte
	switch char {
	case '(':
		end = ')'
	case '[':
		end = ']'
	case '{':
		end = '}'
	}

	head = Cons(nil, nil)
	tail = head
	for char, e = skipBlanks(self, in); e == nil; char, e = skipBlanks(self, in) {
		if char == end {
			break
		}
		if char == ')' || char == ']' || char == '}' {
			e = NewError("Error: unmatched parentheses.")
			break
		}
		if char == '.' {
			if cell, e = self.ReadExpression(in); e == nil {
				Rplacd(tail, cell)
			}
		} else {
			in.UnreadByte()
			if cell, e = self.ReadExpression(in); e == nil {
				tail = Rplacd(tail, Cons(cell, nil))
			} else if e == io.EOF {
				e = NewError("Error: EOF in list.")
			}
		}
	}
	head = Cdr(head)
	if _, is := Car(head).(*TypeSymbol); is {
		if expr := Assq(Car(head), Cdr(self.special)); expr != nil {
			head, e = self.Apply(Cdr(expr), Cdr(head), self.Globals)
		}
		if head == nil {
			return
		}
	}
	if head == nil {
		head = self.nothing
	}
	return
}

func readSemicolon(self *Interpreter, in *bufio.Reader, char byte) (cell Cell, e error) {
	for char, e = in.ReadByte(); e == nil && (char != '\n') && (char != '\r'); char, e = in.ReadByte() {
	}
	return
}

func readQuote(self *Interpreter, in *bufio.Reader, char byte) (cell Cell, e error) {
	cell, e = self.ReadExpression(in)
	switch e {
	case io.EOF:
		e = NewError("Error: EOF in quoted literal.")
	case nil:
		cell = Cons(cell, nil)
		cell = Cons(self.Symbol("quote"), cell)
	}
	return
}

func readQuasiquote(self *Interpreter, in *bufio.Reader, char byte) (cell Cell, e error) {
	cell, e = self.ReadExpression(in)
	switch e {
	case io.EOF:
		e = NewError("Error: EOF in quasiquoted literal.")
	case nil:
		cell = Cons(cell, nil)
		cell = Cons(self.Symbol("quasiquote"), cell)
	}
	return
}

func readUnquote(self *Interpreter, in *bufio.Reader, char byte) (cell Cell, e error) {
	if char, e = in.ReadByte(); e == nil {
		in.UnreadByte()
		cell, e = self.ReadExpression(in)
		switch e {
		case io.EOF:
			e = NewError("Error: EOF in quasiquoted literal.")
		case nil:
			cell = Cons(cell, nil)
			if char == '@' {
				cell = Cons(self.Symbol("unquote-splicing"), cell)
			} else {
				cell = Cons(self.Symbol("unquote"), cell)
			}
		}
	}
	return
}

func readSign(self *Interpreter, in *bufio.Reader, char byte) (cell Cell, e error) {
	sign := char
	if char, e = in.ReadByte(); e == nil {
		in.UnreadByte()
		if charReadsNumber(self, char) {
			cell, e = readNumber(self, in, sign)
		} else {
			cell, e = readSymbol(self, in, sign)
		}
	}
	return
}

func readBlank(self *Interpreter, in *bufio.Reader, char byte) (cell Cell, e error) {
	return
}
