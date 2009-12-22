package lips

import (
    "os";
    "strings";
    "strconv";
    "fmt";
    "io";
    "bufio";
    "bytes";
)

// reader chars

const (
    _CharsBlank      = "\t\f\v\r\n ";
    _CharsSymbol     = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!#$%&+*/:<=>?@\\^_|~.";
    _CharsNumber     = "0123456789";
    _CharsString     = "\"";
    _CharsList       = "(";
    _CharsSemicolon  = ";";
    _CharsQuote      = "'";
    _CharsQuasiquote = "`";
    _CharsUnquote    = ",";
    _CharsSign       = "-";
)

// errors

type FatalError string;
type Error      string;

func (e FatalError) String() string { return string(e) }
func (e Error)      String() string { return string(e) }

func NewFatalError (s string) FatalError { return FatalError(s) }
func NewError      (s string) Error      { return Error(s) }

// interpreter

type (
    _Readers [] func(*LIPS, *bufio.Reader, byte) (Cell, os.Error);
    _Symbols map[string] Cell;
)

type LIPS struct {
    readers _Readers;
    symbols _Symbols;
    Globals Cell;
    special Cell;
    Nothing Cell;
}

func NewLIPS() *LIPS {
    self := new(LIPS);
    
    self.readers = make(_Readers, 256);
    self.symbols = make(_Symbols);
    self.special = Cons(self.Symbol("*syntax-table*"), nil);
    self.Nothing = Cons(nil, nil);
    
    for char, _ := range self.readers                    { self.readers[char] = readUnknown }
    for _, char := range strings.Bytes(_CharsBlank)      { self.readers[char] = readBlank }
    for _, char := range strings.Bytes(_CharsSymbol)     { self.readers[char] = readSymbol }
    for _, char := range strings.Bytes(_CharsNumber)     { self.readers[char] = readNumber }
    for _, char := range strings.Bytes(_CharsString)     { self.readers[char] = readString }
    for _, char := range strings.Bytes(_CharsList)       { self.readers[char] = readList }
    for _, char := range strings.Bytes(_CharsSemicolon)  { self.readers[char] = readSemicolon }
    for _, char := range strings.Bytes(_CharsQuote)      { self.readers[char] = readQuote }
    for _, char := range strings.Bytes(_CharsQuasiquote) { self.readers[char] = readQuasiquote }
    for _, char := range strings.Bytes(_CharsUnquote)    { self.readers[char] = readUnquote }
    for _, char := range strings.Bytes(_CharsSign)       { self.readers[char] = readSign }
    
    self.Symbol("quote");
    self.Symbol("quasiquote");
    self.Symbol("unquote");
    self.Symbol("unquote-splicing");
    
    self.Globals = Cons(Cons(self.Symbol("t"),       self.Symbol("t")),       self.Globals);
    self.Globals = Cons(Cons(self.Symbol("define"),  Fubr(fubr_define)),      self.Globals);
    self.Globals = Cons(Cons(self.Symbol("eval"),    Subr(subr_eval)),        self.Globals);
    self.Globals = Cons(Cons(self.Symbol("apply"),   Subr(subr_apply)),       self.Globals);
    self.Globals = Cons(Cons(self.Symbol("flambda"), Fubr(fubr_fxpr)),        self.Globals);
    self.Globals = Cons(Cons(self.Symbol("lambda"),  Fubr(fubr_expr)),        self.Globals);
    self.Globals = Cons(Cons(self.Symbol("cons"),    Subr(subr_cons)),        self.Globals);
    self.Globals = Cons(Cons(self.Symbol("car"),     Subr(subr_car)),         self.Globals);
    self.Globals = Cons(Cons(self.Symbol("cdr"),     Subr(subr_cdr)),         self.Globals);
    self.Globals = Cons(Cons(self.Symbol("rplaca"),  Subr(subr_rplaca)),      self.Globals);
    self.Globals = Cons(Cons(self.Symbol("rplacd"),  Subr(subr_rplacd)),      self.Globals);
    self.Globals = Cons(Cons(self.Symbol("assq"),    Subr(subr_assq)),        self.Globals);
    self.Globals = Cons(Cons(self.Symbol("setq"),    Fubr(fubr_setq)),        self.Globals);
    self.Globals = Cons(Cons(self.Symbol("let"),     Fubr(fubr_let)),         self.Globals);
    self.Globals = Cons(Cons(self.Symbol("while"),   Fubr(fubr_while)),       self.Globals);
    self.Globals = Cons(Cons(self.Symbol("if"),      Fubr(fubr_if)),          self.Globals);
    self.Globals = Cons(Cons(self.Symbol("map"),     Subr(subr_map)),         self.Globals);
    self.Globals = Cons(Cons(self.Symbol("+"),       Subr(subr_add)),         self.Globals);
    self.Globals = Cons(Cons(self.Symbol("-"),       Subr(subr_subtract)),    self.Globals);
    self.Globals = Cons(Cons(self.Symbol("*"),       Subr(subr_multiply)),    self.Globals);
    self.Globals = Cons(Cons(self.Symbol("/"),       Subr(subr_divide)),      self.Globals);
    self.Globals = Cons(Cons(self.Symbol("%"),       Subr(subr_modulus)),     self.Globals);
    self.Globals = Cons(Cons(self.Symbol("<"),       Subr(subr_less)),        self.Globals);
    self.Globals = Cons(Cons(self.Symbol(">"),       Subr(subr_more)),        self.Globals);
    self.Globals = Cons(Cons(self.Symbol("<="),      Subr(subr_lessOrEqual)), self.Globals);
    self.Globals = Cons(Cons(self.Symbol(">="),      Subr(subr_moreOrEqual)), self.Globals);
    self.Globals = Cons(Cons(self.Symbol("=="),      Subr(subr_equal)),       self.Globals);
    self.Globals = Cons(Cons(self.Symbol("!="),      Subr(subr_notEqual)),    self.Globals);
    self.Globals = Cons(Cons(self.Symbol("and"),     Fubr(fubr_and)),         self.Globals);
    self.Globals = Cons(Cons(self.Symbol("or"),      Fubr(fubr_or)),          self.Globals);
    self.Globals = Cons(Cons(self.Symbol("println"), Subr(subr_println)),     self.Globals);
    self.Globals = Cons(Cons(self.Symbol("print"),   Subr(subr_print)),       self.Globals);
    self.Globals = Cons(Cons(self.Symbol("load"),    Subr(subr_load)),        self.Globals);
    self.Globals = Cons(Cons(self.Symbol("exit"),    Subr(subr_exit)),        self.Globals);
    self.Globals = Cons(self.special,                                         self.Globals);
    
    return self
}

func (self *LIPS) Symbol(name string) Cell {
    cell, is := self.symbols[name];
    if !is {
        cell = Symbol(name); self.symbols[name] = cell
    }
    return cell
}

func (self *LIPS) ReadFile(path string) (cell Cell, e os.Error) {
    file, e := os.Open(path, 0, 0);
    if file != nil {
        cell, e = self.Read(file);
        file.Close()
    }
    return
}

func (self *LIPS) Read(r io.Reader) (cell Cell, e os.Error) {
    in := bufio.NewReader(r);
    
    var expr Cell;
    for expr, e = self.ReadExpression(in); e == nil;
        expr, e = self.ReadExpression(in) {
        cell, e = self.Eval(expr, self.Globals);
    }
    if e == os.EOF { e = nil }
    return
}

func (self *LIPS) ReadExpression(in *bufio.Reader) (cell Cell, e os.Error) {
    var char byte;
    for e == nil && cell == nil {
        if char, e = skipBlanks(self, in); e == nil {
            if cell, e = self.readers[char](self, in, char); cell == self.Nothing { cell = nil; break }
        }
    }
    return
}

func (self *LIPS) Eval(expr Cell, env Cell) (cell Cell, e os.Error) {
    switch t := expr.(type) {
    case *TypeString, *TypeNumber, *TypeExpr, *TypeSubr, *TypeFubr:
        cell = expr
    case *TypeSymbol:
        if cell = Assq(expr, env); cell == nil {
            e = NewError(fmt.Sprintf("Error: undefined %s.", Sexp(expr)))
        }
        cell = Cdr(cell)
    case *TypeCons:
        cell, e = self.Eval(Car(expr), env);
        cell, e = self.Apply(cell, Cdr(expr), env)
    }
    return
}

func (self *LIPS) Apply(fun, args Cell, env Cell) (cell Cell, e os.Error) {
    switch t := fun.(type) {
    case *TypeFubr:
        cell, e = fun.(*TypeFubr).ptr(self, args, env)
    case *TypeSubr:
        cell, e = self.evalArgs(args, env);
        cell, e = fun.(*TypeSubr).ptr(self, cell, env)
    case *TypeFxpr:
        cell, e = self.pairList(Car(fun.(*TypeFxpr).one), Cons(args, Cons(env, nil)), fun.(*TypeFxpr).two);
        cell, e = self.evalList(Cdr(fun.(*TypeFxpr).one), cell)
    case *TypeExpr:
        cell, e = self.evalArgs(args, env);
        cell, e = self.pairList(Car(fun.(*TypeExpr).one), cell, fun.(*TypeExpr).two);
        cell, e = self.evalList(Cdr(fun.(*TypeExpr).one), cell)
    default:
        e = NewError(fmt.Sprintf("Error: cannot apply %s.", Sexp(fun)))
    }
    return
}

func (self *LIPS) evalArgs(args Cell, env Cell) (cell Cell, e os.Error) {
    if args == nil {
        return
    }
    head, e := self.Eval    (Car(args), env);
    tail, e := self.evalArgs(Cdr(args), env);
    cell = Cons(head, tail);
    return
}

func (self *LIPS) evalList(expr Cell, env Cell) (cell Cell, e os.Error) {
    for ; expr != nil; expr = Cdr(expr) {
        cell, e = self.Eval(Car(expr), env)
    }
    return
}

func (self *LIPS) pairList(expr Cell, args Cell, env Cell) (cell Cell, e os.Error) {
    cell = env;
    if _, is := expr.(*TypeCons); is {
        for ; expr != nil; expr, args = Cdr(expr), Cdr(args) {
            cell = Cons(Cons(Car(expr), Car(args)), cell)
        }
        return
    }
    cell = Cons(Cons(expr, args), cell);
    return
}

// cell

type Cell interface { Sexp() string }

type (
    TypeString string;
    TypeSymbol string;
    TypeNumber int;
    TypeFxpr   Pair;
    TypeExpr   Pair;
    TypeFubr   struct { ptr Func };
    TypeSubr   struct { ptr Func };
    TypeCons   Pair;
    
    Func func(*LIPS, Cell, Cell) (Cell, os.Error);
    
    Pair struct {
        one Cell;
        two Cell
    }
)

func (self *TypeString) Sexp() string { return fmt.Sprintf("\"%s\"", string(*self)) }
func (self *TypeSymbol) Sexp() string { return string(*self) }
func (self *TypeNumber) Sexp() string { return fmt.Sprintf("%d", int(*self)) }
func (self *TypeFxpr)   Sexp() string { return fmt.Sprintf("(flambda %s)", self.one.Sexp()) }
func (self *TypeExpr)   Sexp() string { return fmt.Sprintf("(lambda %s)",  self.one.Sexp()) }
func (self *TypeFubr)   Sexp() string { return fmt.Sprintf("<fsubr %p>", self.ptr) }
func (self *TypeSubr)   Sexp() string { return fmt.Sprintf("<subr %p>", self.ptr) }
func (self *TypeCons)   Sexp() string {
    cell := Cell(self);
    sexp := "(";
    for is := true ; is; _, is = cell.(*TypeCons) {
        sexp += Sexp(Car(cell));
        if cell = Cdr(cell); cell != nil {
            sexp += " "
        }
    }
    if cell != nil {
        sexp += ". " + cell.Sexp()
    }
    sexp += ")";
    return sexp;
}

func Sexp(cell Cell) string {
    if cell != nil {
        return cell.Sexp()
    }
    return "nil"
}

func asString(cell Cell) (s string, e os.Error) {
    if cast, is := cell.(*TypeString); is {
        s = string(*cast)
    } else {
        e = NewError(fmt.Sprintf("Error: %s is not a string", Sexp(cell)))
    }
    return
} 

func asInt(cell Cell) (i int, e os.Error) {
    if cast, is := cell.(*TypeNumber); is {
        i = int(*cast)
    } else {
        e = NewError(fmt.Sprintf("Error: %s is not a number", Sexp(cell)))
    }
    return
} 

func Symbol (val string)         Cell { cast := TypeSymbol(val); return &cast }
func String (val string)         Cell { cast := TypeString(val); return &cast }
func Number (val int)            Cell { cast := TypeNumber(val); return &cast }
func Fubr   (ptr Func)           Cell { return &TypeFubr{ ptr } }
func Subr   (ptr Func)           Cell { return &TypeSubr{ ptr } }
func Fxpr   (one Cell, two Cell) Cell { return &TypeFxpr{ one, two } }
func Expr   (one Cell, two Cell) Cell { return &TypeExpr{ one, two } }
func Cons   (one Cell, two Cell) Cell { return &TypeCons{ one, two } }

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

func Caar  (cell Cell) Cell { return Car(Car(cell)) }
func Cadr  (cell Cell) Cell { return Car(Cdr(cell)) }
func Cdar  (cell Cell) Cell { return Cdr(Car(cell)) }
func Caddr (cell Cell) Cell { return Car(Cdr(Cdr(cell))) }
func Cadar (cell Cell) Cell { return Car(Cdr(Car(cell))) }

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
        if cell == Caar(list) { return Car(list) }
        list = Cdr(list)
    }
    return nil
}

// builtin

func fubr_define(lips *LIPS, args Cell, env Cell) (cell Cell, e os.Error) {
    if args == nil {
        return
    }
    cell = Cons(Car(args), nil);
    Rplacd(lips.Globals, Cons(cell, Cdr(lips.Globals)));
    expr, e := lips.Eval(Cadr(args), env);
    cell = Rplacd(cell, expr);
    return
}

func subr_eval(lips *LIPS, args Cell, env Cell) (cell Cell, e os.Error) {
    if evalEnv := Cadr(args); evalEnv != nil {
        cell, e = lips.Eval(Car(args), evalEnv)
    } else {
        cell, e = lips.Eval(Car(args), env)
    }
    return
}

func subr_apply(lips *LIPS, args Cell, env Cell) (cell Cell, e os.Error) {
    cell, e = lips.Apply(Car(args), Cdr(args), env);
    return
}

func fubr_setq(lips *LIPS, args Cell, env Cell) (cell Cell, e os.Error) {
    key := Car(args);
    if _, is := key.(*TypeSymbol); is {
        cell, e = lips.Eval(Cadr(args), env);
        if tmp := Assq(key, env); tmp != nil {
            Rplacd(tmp, cell)
        } else {
            e = NewError(fmt.Sprintf("Error: undefined %s.", Sexp(key)))
        }
    }
    return
}

func fubr_let(lips *LIPS, args Cell, env Cell) (cell Cell, e os.Error) {
    var tmp Cell;
    for cell = Car(args); cell != nil; cell = Cdr(cell) {
      tmp, e = lips.Eval(Cadar(cell), env);
      tmp = Cons(Caar(cell), tmp);
      env = Cons(tmp, env)
    }
    cell, e = lips.evalList(Cdr(args), env);
    return
}

func fubr_while(lips *LIPS, args Cell, env Cell) (cell Cell, e os.Error) {
    var expr Cell;
    for expr, e = lips.Eval(Car(args), env); expr != nil; 
        expr, e = lips.Eval(Car(args), env) {
        cell, e = lips.evalList(Cdr(args), env)
    }
    return
}

func fubr_if(lips *LIPS, args Cell, env Cell) (cell Cell, e os.Error) {
    var expr Cell;
    if expr, e = lips.Eval(Car(args), env); expr != nil {
        cell, e = lips.Eval( Cadr(args), env)
    } else {
        cell, e = lips.Eval(Caddr(args), env)
    }
    return
}

func subr_map(lips *LIPS, args Cell, env Cell) (cell Cell, e os.Error) {
    head := Cons(nil, nil);
    tail := head;
    expr := Car(args);
    
    for args = Cdar(args) ; args != nil; args = Car(args) {
        cell, e = lips.Apply(expr, mapArgs(args), env);
        tail = Rplacd(tail, Cons(cell, nil))
    }
    cell = Cdr(head);
    return
}
func mapArgs(args Cell) Cell {
    if args != nil {
        cell := Caar(args);
        Rplaca(args, Cdar(args));
        tail := mapArgs(Cdr(args));
        return Cons(cell, tail)
    }
    return nil
}

func fubr_fxpr   (lips *LIPS, args Cell, env Cell) (cell Cell, e os.Error) { cell = Fxpr(args, env); return }
func fubr_expr   (lips *LIPS, args Cell, env Cell) (cell Cell, e os.Error) { cell = Expr(args, env); return }
func subr_cons   (lips *LIPS, args Cell, env Cell) (cell Cell, e os.Error) { cell = Cons(Car(args), Cadr(args)); return }
func subr_car    (lips *LIPS, args Cell, env Cell) (cell Cell, e os.Error) { cell = Caar(args); return }
func subr_cdr    (lips *LIPS, args Cell, env Cell) (cell Cell, e os.Error) { cell = Cdar(args); return }
func subr_rplaca (lips *LIPS, args Cell, env Cell) (cell Cell, e os.Error) { cell = Rplaca(Car(args), Cadr(args)); return }
func subr_rplacd (lips *LIPS, args Cell, env Cell) (cell Cell, e os.Error) { cell = Rplacd(Car(args), Cadr(args)); return }
func subr_assq   (lips *LIPS, args Cell, env Cell) (cell Cell, e os.Error) { cell = Assq(Car(args), Cadr(args)); return }

func subr_add(lips *LIPS, args Cell, env Cell) (cell Cell, e os.Error) {
    number, e := asInt(Car(args));
    var n int;
    for args = Cdr(args); e == nil && args != nil; args = Cdr(args) {
        n, e = asInt(Car(args));
        number += n
    }
    cell = Number(number);
    return
}

func subr_subtract(lips *LIPS, args Cell, env Cell) (cell Cell, e os.Error) {
    number, e := asInt(Car(args));
    if Cdr(args) != nil {
        var n int;
        for args = Cdr(args); e == nil && args != nil; args = Cdr(args) {
            n, e = asInt(Car(args));
            number -= n
        }
    } else {
        number = 0 - number
    }
    cell = Number(number);
    return
}

func subr_multiply(lips *LIPS, args Cell, env Cell) (cell Cell, e os.Error) {
    number, e := asInt(Car(args));
    var n int;
    for args = Cdr(args); e == nil && args != nil; args = Cdr(args) {
        n, e = asInt(Car(args));
        number *= n
    }
    cell = Number(number);
    return
}

func subr_divide(lips *LIPS, args Cell, env Cell) (cell Cell, e os.Error) {
    number, e := asInt(Car(args));
    if Cdr(args) != nil {
        var n int;
        for args = Cdr(args); e == nil && args != nil; args = Cdr(args) {
            n, e = asInt(Car(args));
            number /= n
        }
    } else {
        number = 1 / number
    }
    cell = Number(number);
    return
}

func subr_modulus(lips *LIPS, args Cell, env Cell) (cell Cell, e os.Error) {
    number, e := asInt(Car(args));
    if Cdr(args) != nil {
        var n int;
        for args = Cdr(args); e == nil && args != nil; args = Cdr(args) {
            n, e = asInt(Car(args));
            number %= n
        }
    } else {
        number = 1 % number
    }
    cell = Number(number);
    return
}

func subr_less(lips *LIPS, args Cell, env Cell) (cell Cell, e os.Error) {
    for ; Cdr(args) != nil; args = Cdr(args) {
        n1, e := asInt( Car(args));
        n2, e := asInt(Cadr(args));
        if e != nil || !(n1 < n2) {
            return
        }
    }
    cell = lips.Symbol("t");
    return
}

func subr_more(lips *LIPS, args Cell, env Cell) (cell Cell, e os.Error) {
    for ; Cdr(args) != nil; args = Cdr(args) {
        n1, e := asInt( Car(args));
        n2, e := asInt(Cadr(args));
        if e != nil || !(n1 > n2) {
            return
        }
    }
    cell = lips.Symbol("t");
    return
}

func subr_lessOrEqual(lips *LIPS, args Cell, env Cell) (cell Cell, e os.Error) {
    for ; Cdr(args) != nil; args = Cdr(args) {
        n1, e := asInt( Car(args));
        n2, e := asInt(Cadr(args));
        if e != nil || !(n1 <= n2) {
            return
        }
    }
    cell = lips.Symbol("t");
    return
}

func subr_moreOrEqual(lips *LIPS, args Cell, env Cell) (cell Cell, e os.Error) {
    for ; Cdr(args) != nil; args = Cdr(args) {
        n1, e := asInt( Car(args));
        n2, e := asInt(Cadr(args));
        if e != nil || !(n1 >= n2) {
            return
        }
    }
    cell = lips.Symbol("t");
    return
}

func subr_equal(lips *LIPS, args Cell, env Cell) (cell Cell, e os.Error) {
    for ; Cdr(args) != nil; args = Cdr(args) {
        n1, e := asInt( Car(args));
        n2, e := asInt(Cadr(args));
        if e != nil || n1 != n2 {
            return
        }
    }
    cell = lips.Symbol("t");
    return
}

func subr_notEqual(lips *LIPS, args Cell, env Cell) (cell Cell, e os.Error) {
    for ; Cdr(args) != nil; args = Cdr(args) {
        n1, e := asInt( Car(args));
        n2, e := asInt(Cadr(args));
        if e != nil || n1 == n2 {
            return
        }
    }
    cell = lips.Symbol("t");
    return
}

func fubr_and(lips *LIPS, args Cell, env Cell) (cell Cell, e os.Error) {
    cell = lips.Symbol("t");
    for ; args != nil && cell != nil; args = Cdr(args) {
        cell, e = lips.Eval(Car(args), env)
    }
    return
}

func fubr_or(lips *LIPS, args Cell, env Cell) (cell Cell, e os.Error) {
    for ; args != nil && cell == nil; args = Cdr(args) {
        cell, e = lips.Eval(Car(args), env)
    }
    return
}

func subr_println(lips *LIPS, args Cell, env Cell) (cell Cell, e os.Error) {
    for ; args != nil; args = Cdr(args) {
        fmt.Print(Sexp(Car(args)));
        if Cdr(args) != nil {
            fmt.Print(" ")
        }
    }
    fmt.Print("\n");
    return
}

func subr_print(lips *LIPS, args Cell, env Cell) (cell Cell, e os.Error) {
    for ; args != nil; args = Cdr(args) {
        fmt.Print(Sexp(Car(args)));
        if Cdr(args) != nil {
            fmt.Print(" ")
        }
    }
    return
}

func subr_load(lips *LIPS, args Cell, env Cell) (cell Cell, e os.Error) {
    if path, e := asString(Car(args)); e == nil {
        cell, e = lips.ReadFile(path)
    }
    return
}

func subr_exit(lips *LIPS, args Cell, env Cell) (cell Cell, e os.Error) {
    if cell = Car(args); cell != nil {
        if code, e := asInt(cell); e == nil {
            os.Exit(code)
        }
    } else {
        os.Exit(0)
    }
    return
}

// readers

func skipBlanks(self *LIPS, in *bufio.Reader) (char byte, e os.Error) {
    for char, e = in.ReadByte(); e == nil && charReadsBlank(self, char);
        char, e = in.ReadByte() {  }
    return
}

func charReadsUnknown    (self *LIPS, char byte) bool { return self.readers[char] == readUnknown }
func charReadsBlank      (self *LIPS, char byte) bool { return self.readers[char] == readBlank }
func charReadsSymbol     (self *LIPS, char byte) bool { return self.readers[char] == readSymbol }
func charReadsNumber     (self *LIPS, char byte) bool { return self.readers[char] == readNumber }
func charReadsString     (self *LIPS, char byte) bool { return self.readers[char] == readString }
func charReadsList       (self *LIPS, char byte) bool { return self.readers[char] == readList }
func charReadsSemicolon  (self *LIPS, char byte) bool { return self.readers[char] == readSemicolon }
func charReadsQuote      (self *LIPS, char byte) bool { return self.readers[char] == readQuote }
func charReadsQuasiquote (self *LIPS, char byte) bool { return self.readers[char] == readQuasiquote }
func charReadsUnquote    (self *LIPS, char byte) bool { return self.readers[char] == readUnquote }
func charReadsSign       (self *LIPS, char byte) bool { return self.readers[char] == readSign }

func readUnknown(self *LIPS, in *bufio.Reader, char byte) (cell Cell, e os.Error) {
    e = NewError(fmt.Sprintf("Error: illegal character %c.", char));
    return
}

func readSymbol(self *LIPS, in *bufio.Reader, char byte) (cell Cell, e os.Error) {
    buffer := bytes.NewBuffer(nil);
    buffer.WriteByte(char);
    for char, e = in.ReadByte(); e == nil && (charReadsSymbol(self, char) ||
        charReadsNumber(self, char) || charReadsSign(self, char)); 
        char, e = in.ReadByte() {
        buffer.WriteByte(char)
    }
    in.UnreadByte();
    cell = self.Symbol(buffer.String());
    return
}

func readNumber(self *LIPS, in *bufio.Reader, char byte) (cell Cell, e os.Error) {
    buffer := bytes.NewBuffer(nil);
    buffer.WriteByte(char);
    for char, e = in.ReadByte(); e == nil && charReadsNumber(self, char);
        char, e = in.ReadByte() {
        buffer.WriteByte(char)
    }
    in.UnreadByte();
    if number, e := strconv.Atoi(buffer.String()); e == nil {
        cell = Number(number);
    }
    return
}

func readString(self *LIPS, in *bufio.Reader, char byte) (cell Cell, e os.Error) {
    buffer := bytes.NewBuffer(nil);
    ending := char;
    escape := false;
    for char, e = in.ReadByte(); e == nil && (escape || char != ending);
        char, e = in.ReadByte() {
        escape = !escape && char == '\\';
        buffer.WriteByte(char)
    }
    cell = String(buffer.String());
    return
}

func readList(self *LIPS, in *bufio.Reader, char byte) (head Cell, e os.Error) {
    var tail Cell;
    var cell Cell;
    
    head = Cons(nil, nil);
    tail = head;
    for char, e = skipBlanks(self, in); e == nil;
        char, e = skipBlanks(self, in) {
        if char == ')' { break }
        if char == '.' {
            if cell, e = self.ReadExpression(in); e == nil { Rplacd(tail, cell) }
        } else {
            in.UnreadByte();
            if cell, e = self.ReadExpression(in); e == nil {
                tail = Rplacd(tail, Cons(cell, nil))
            }
        }
    }
    head = Cdr(head);
    if _, is := Car(head).(*TypeSymbol); is {
        if expr := Assq(Car(head), Cdr(self.special)); expr != nil {
            head, e = self.Apply(Cdr(expr), Cdr(head), self.Globals)
        }
        if head == nil {
            return
        }
    }
    if head == nil {
        head = self.Nothing
    }
    return
}

func readSemicolon(self *LIPS, in *bufio.Reader, char byte) (cell Cell, e os.Error) {
    for char, e = in.ReadByte(); e == nil && (char != '\n') && (char != '\r');
        char, e = in.ReadByte() {  }
    return
}

func readQuote(self *LIPS, in *bufio.Reader, char byte) (cell Cell, e os.Error) {
    if cell, e = self.ReadExpression(in); e == nil {
        cell = Cons(self.Symbol("quote"), Cons(cell, nil))
    }
    return
}

func readQuasiquote(self *LIPS, in *bufio.Reader, char byte) (cell Cell, e os.Error) {
    if cell, e = self.ReadExpression(in); e == nil {
        cell = Cons(self.Symbol("quasiquote"), Cons(cell, nil))
    }
    return
}

func readUnquote(self *LIPS, in *bufio.Reader, char byte) (cell Cell, e os.Error) {
    if char, e = in.ReadByte(); e == nil {
        in.UnreadByte();
        if char == '@' {
            cell = Cons(self.Symbol("unquote-splicing"), Cons(cell, nil))
        } else {
            cell = Cons(self.Symbol("unquote"), Cons(cell, nil))
        }
    }
    return
}

func readSign(self *LIPS, in *bufio.Reader, char byte) (cell Cell, e os.Error) {
    sign := char;
    if char, e = in.ReadByte(); e == nil {
        in.UnreadByte();
        if charReadsNumber(self, char) {
            cell, e = readNumber(self, in, sign)
        } else {
            cell, e = readSymbol(self, in, sign)
        }
    }
    return
}

func readBlank(self *LIPS, in *bufio.Reader, char byte) (cell Cell, e os.Error) {
    return
}
