package main

import (
    "bufio";
    "flag";
    "fmt";
    "os";
    "lips";
)

func readFile(self *lips.LIPS, path string) {
    if _, e := self.ReadFile(path); e != nil {
        fmt.Fprintln(os.Stderr, e);
        os.Exit(1)
    }
}

func repl(self *lips.LIPS) {
    in := bufio.NewReader(os.Stdin);
    
    fmt.Println("Welcome to LIPS, a misspelled LISP.");
    var expr, cell lips.Cell;
    var e os.Error;
    for {
        if expr, e = self.ReadExpression(in); e == nil {
            if cell, e = self.Eval(expr, self.Globals); e == nil {
                fmt.Println("=> " + lips.Sexp(cell));
                continue
            }
        }
        fmt.Fprintln(os.Stderr, e);
        if _, is := e.(lips.Error); !is {
            os.Exit(1)
        }
    }
}

func main() {
    self := lips.NewLIPS();
    
    readFile(self, "lips.lisp");
    
    flag.Parse();
    switch len(flag.Args()) {
    case 0: 
        repl(self)
    case 1:
        readFile(self, flag.Arg(0))
    }
}
