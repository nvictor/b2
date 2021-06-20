// b2 is an interpreter for the DARTMOUTH BASIC programming language.
// The implementation is a translation of Peter Norvig's Python implementation
// and is meant as a programming exercise in the Go programming language. See
// Peter's implementation here:
// https://github.com/norvig/pytudes/blob/master/ipynb/BASIC.ipynb
package main

import (
	"flag"
	"fmt"
	"io/ioutil"
	"math"
	"math/rand"
	"os"
	"regexp"
	"sort"
	"strconv"
	"strings"
)

// ParseProgram parses a program, returning slices of sorted statements S.
func ParseProgram(s string) []S {
	var statements []S
	var parseError bool
	for i, l := range SplitLines(s) {
		Tokens = GetTokens(l)
		s, err := ParseLine()
		if err != nil {
			fmt.Fprintf(
				os.Stderr,
				"parse error: line %d, %v, remaining Tokens %q\n",
				i+1, err, Tokens)
			parseError = true
		}
		if len(Tokens) != 0 {
			fmt.Fprintf(
				os.Stderr,
				"parse error: line %d, extra Tokens %q\n",
				i+1, Tokens)
			parseError = true
		}
		statements = append(statements, s)
	}
	SortStatements(statements)
	if !parseError {
		return statements
	} else {
		return make([]S, 0)
	}
}

// The internal data types have intentionally short names because of the
// verboseness of the structures they form.

// S represents a statement.
type S struct {
	line int
	kind string
	exps []E
}

// E represents an expression. All numbers have a float64 value.
type E struct {
	num float64
	str string
	vbl *V
	fnc *F
	opc *O
}

// V represents a variable. Variables can have subscripts.
type V struct {
	val string
	sub []E
}

// F represents a function call.
type F struct {
	fn string
	x  E
}

// O represents an operation.
type O struct {
	op string
	x  E
	y  E
}

// SplitLines splits s on new lines (\n) returning a slice of non-empty strings.
func SplitLines(s string) []string {
	keep := []string{}
	lines := strings.Split(s, "\n")
	for i := range lines {
		if len(lines[i]) > 0 {
			keep = append(keep, lines[i])
		}
	}
	return keep
}

// Tokens is a slice of tokens.
var Tokens []string

// GetTokens slices s into the slice Tokens.
func GetTokens(s string) []string {
	// NOTE: The regexp is matched twice (solution from Peter Norvig's
	// implementation) to ensures that split tokens like "GO TO" or combined
	// ones like "10GOTO" are treated correctly.
	var find = func(s string) []string {
		return regexp.MustCompile(``+
			// number
			`\d*\.?\d+(?:E-?\d+)?`+
			// functions
			`|SIN|COS|TAN|ATN|EXP|ABS|LOG|SQR|RND|INT|FN[A-Z]`+
			// keywords
			`|LET|READ|DATA|PRINT|GOTO|IF|FOR|NEXT|END|DEF`+
			`|GOSUB|RETURN|DIM|REM|TO|THEN|STEP|STOP`+
			// variable names (letter + optional digit)
			`|[A-Z]\d?`+
			// labels (strings in double quotes)
			`|".*?"`+
			// multi-character relational operators
			`|<>|>=|<=`+
			// any non-space single character
			`|\S`).FindAllString(s, -1)
	}
	return find(strings.Join(find(s), ""))
}

// ParseLine parses a single line returning S with a possible error.
func ParseLine() (S, error) {
	var err error
	var s S
	if s.line, err = ParseLineNumber(); err != nil {
		return s, err
	}
	if s.kind, err = ParseStatementKind(); err != nil {
		return s, err
	}
	if s.exps, err = ParseFuncs[s.kind](); err != nil {
		return s, err
	}
	return s, nil
}

// ParseLineNumber parses a line number like 10, 99.
func ParseLineNumber() (int, error) {
	if MatchDigits(Peek()) {
		return strconv.Atoi(*Pop())
	}
	return -1, fmt.Errorf("missing line number")
}

// MatchDigits matches s with digits.
func MatchDigits(s *string) bool { return s != nil && reMatchDigits(*s) }

var reMatchDigits = regexp.MustCompile(`^\d+$`).MatchString

// Peek returns a pointer to the first value in Tokens or nil if Tokens is
// empty.
func Peek() *string {
	switch {
	case len(Tokens) > 0:
		return &Tokens[0]
	default:
		return nil
	}
}

// Pop removes and returns the first value in Tokens or returns nil if Tokens
// is empty.
func Pop() *string {
	first := Peek()
	if first != nil {
		Tokens = Tokens[1:]
	}
	return first
}

// ParseStatementKind parses a statement kind.
func ParseStatementKind() (string, error) {
	first := Peek()
	if !MatchLetters(first) || !Firsts[*first] {
		if first != nil {
			return "", fmt.Errorf("unknown statement kind %s", *first)
		} else {
			return "", fmt.Errorf("expected a statement")
		}
	}
	return *Pop(), nil
}

// MatchLetters matches s with letters.
func MatchLetters(s *string) bool { return s != nil && reMatchLetters(*s) }

var reMatchLetters = regexp.MustCompile(`^[A-Z]+$`).MatchString

// Firsts is a set of valid statements first tokens.
var Firsts = map[string]bool{
	"LET":    true,
	"READ":   true,
	"DATA":   true,
	"PRINT":  true,
	"GOTO":   true,
	"IF":     true,
	"FOR":    true,
	"NEXT":   true,
	"END":    true,
	"STOP":   true,
	"DEF":    true,
	"GOSUB":  true,
	"RETURN": true,
	"DIM":    true,
	"REM":    true,
}

// ParseFuncs maps statements first tokens to parse functions.
var ParseFuncs = map[string](func() ([]E, error)){
	"LET":    ParseStatementLet,
	"READ":   ParseStatementReadOrDim,
	"DATA":   ParseStatementData,
	"PRINT":  ParseStatementPrint,
	"GOTO":   ParseStatementGotoOrGosub,
	"IF":     ParseStatementIf,
	"FOR":    ParseStatementFor,
	"NEXT":   ParseStatementNext,
	"END":    ParseStatementEmpty,
	"STOP":   ParseStatementEmpty,
	"DEF":    ParseStatementDef,
	"GOSUB":  ParseStatementGotoOrGosub,
	"RETURN": ParseStatementEmpty,
	"DIM":    ParseStatementReadOrDim,
	"REM":    ParseStatementRem,
}

// ParseStatementLet parses a LET statement.
func ParseStatementLet() ([]E, error) {
	var e E
	var err error
	var exps []E
	var v V
	if v, err = ParseVariable(); err != nil {
		return exps, err
	}
	exps = append(exps, E{vbl: &v})
	if PopString("=") == nil {
		return exps, fmt.Errorf(`expecting "=" after variable`)
	}
	if e, err = ParseExpression(1); err != nil {
		return exps, err
	}
	exps = append(exps, e)
	return exps, nil
}

// ParseVariable parses a variable with or without subscript like A3 or A(I) or
// M(2*I, 3).
func ParseVariable() (V, error) {
	var err error
	var v V
	if v.val, err = ParseVariableName(); err != nil {
		return v, err
	}
	if PopString("(") != nil {
		var e E
		if len(Tokens) > 0 {
			if e, err = ParseExpression(1); err != nil {
				return v, err
			}
			v.sub = append(v.sub, e)
		}
		for PopString(",") != nil {
			if e, err = ParseExpression(1); err != nil {
				return v, err
			}
			v.sub = append(v.sub, e)
		}
		if PopString(")") == nil {
			return v, fmt.Errorf(`expected ")" to close subscript`)
		}
	}
	return v, nil
}

// ParseVariableName parses a variable name like A, A1, A2.
func ParseVariableName() (string, error) {
	if MatchVarName(Peek()) {
		return *Pop(), nil
	}
	return "", fmt.Errorf("expected a variable name")
}

// MatchVarName matches s with a variable name.
func MatchVarName(s *string) bool {
	return s != nil && (len(*s) == 1 || len(*s) == 2) && reMatchVarName(*s)
}

var reMatchVarName = regexp.MustCompile(`^[A-Z][0-9]?$`).MatchString

// PopString removes and returns the first value in Tokens if that value
// matches the string s; otherwise returns nil.
func PopString(s string) *string {
	token := Peek()
	if token != nil && *token == s {
		Tokens = Tokens[1:]
	} else {
		token = nil
	}
	return token
}

// ParseExpression parses an expression using precendence value p.
func ParseExpression(p int) (E, error) {
	var e1, e2, e3 E
	var err error
	// An expression is either a primary expression
	if e1, err = ParsePrimaryExpression(); err != nil {
		return e1, err
	}
	// or zero or more pairs of operators followed by an expression
	for Precedence(Peek()) >= p {
		o := Pop()
		// Precedence starts at p and changes depending on the
		// precedence and associativity values of each operator o.
		e2, err = ParseExpression(Precedence(o) + Associativity(o))
		if err != nil {
			return e2, err
		}
		e3 = E{opc: &O{x: e1, op: *o, y: e2}}
		e1 = e3
	}
	return e1, nil
}

// ParsePrimaryExpression parses a primary expression.
func ParsePrimaryExpression() (E, error) {
	var e E
	var err error
	var v V
	switch {
	// a primary expression is either a number
	case MatchNumber(Peek()):
		if e.num, err = strconv.ParseFloat(*Pop(), 64); err != nil {
			return e, err
		}
	// or a variable reference
	case MatchVarName(Peek()):
		if v, err = ParseVariable(); err != nil {
			return e, err
		}
		e.vbl = &v
	// or a function call like SIN(X), COS(1.0), FNA(Y)
	case MatchFuncName(Peek()):
		e.fnc = &F{fn: *Pop()}
		if e.fnc.x, err = ParsePrimaryExpression(); err != nil {
			return e, err
		}
	// turns -X into call NEG(X)
	case PopString("-") != nil:
		e.fnc = &F{fn: "NEG"}
		if e.fnc.x, err = ParsePrimaryExpression(); err != nil {
			return e, err
		}
	// or a parenthesized expression like (X)
	case PopString("(") != nil:
		if e, err = ParseExpression(1); err != nil {
			return e, err
		}
		if PopString(")") == nil {
			return e, fmt.Errorf(`expected ")" to end expression`)
		}
	default:
		return e, fmt.Errorf("unknown expression")
	}
	return e, nil
}

// MatchNumber matches s with a number.
func MatchNumber(s *string) bool { return s != nil && reMatchNumber(*s) }

var reMatchNumber = regexp.MustCompile(`^[\d]*[\.]?[\d]+$`).MatchString

// MatchFuncName matches a function name.
func MatchFuncName(s *string) bool {
	return s != nil && len(*s) == 3 && reMatchFuncName(*s)
}

var reMatchFuncName = regexp.MustCompile(`^[A-Z]{2}[0-9A-Z]$`).MatchString

// Precedence returns the precedence value of the operator in *s.
func Precedence(s *string) int {
	switch {
	case s == nil:
		return 0
	case *s == "^":
		return 3
	case *s == "*":
		return 2
	case *s == "/":
		return 2
	case *s == "%":
		return 2
	case *s == "+":
		return 1
	case *s == "-":
		return 1
	default:
		return 0
	}
}

// Associativity returns the associativity value for the operator in *s.
func Associativity(s *string) int {
	switch {
	case s == nil:
		return 1
	case *s == "^":
		return 0
	default:
		return 1
	}
}

// ParseStatementReadOrDim parses READ and DIM statements.
func ParseStatementReadOrDim() ([]E, error) {
	var err error
	var exps []E
	if len(Tokens) > 0 {
		var v V
		if v, err = ParseVariable(); err != nil {
			return exps, err
		}
		exps = append(exps, E{vbl: &v})
	}
	for PopString(",") != nil {
		var v V
		if v, err = ParseVariable(); err != nil {
			return exps, err
		}
		exps = append(exps, E{vbl: &v})
	}
	return exps, nil
}

// ParseStatementData parses a DATA statement.
func ParseStatementData() ([]E, error) {
	var e E
	var err error
	var exps []E
	if len(Tokens) > 0 {
		if e, err = ParseExpression(1); err != nil {
			return exps, err
		}
		exps = append(exps, e)
	}
	for PopString(",") != nil {
		if e, err = ParseExpression(1); err != nil {
			return exps, err
		}
		exps = append(exps, e)
	}
	return exps, nil
}

// ParseStatementPrint parses a PRINT statement.
func ParseStatementPrint() ([]E, error) {
	var e E
	var err error
	var exps []E
	// a PRINT is a sequence of
	for range Tokens {
		switch {
		// labels like "HELLO WORLD"
		case MatchLabel(Peek()):
			exps = append(exps, E{str: *Pop()})
		// commas
		case PopString(",") != nil:
			exps = append(exps, E{str: ","})
		// semicolons
		case PopString(";") != nil:
			exps = append(exps, E{str: ";"})
		// and expressions
		default:
			if len(Tokens) > 0 {
				if e, err = ParseExpression(1); err != nil {
					return exps, err
				}
				exps = append(exps, e)
			}
		}
	}
	return exps, nil
}

// MatchLabel matches a PRINT statement label.
func MatchLabel(s *string) bool { return s != nil && reMatchLabel(*s) }

var reMatchLabel = regexp.MustCompile(`^".*"$`).MatchString

// ParseStatementGoto parses GOTO and GOSUB statements.
func ParseStatementGotoOrGosub() ([]E, error) {
	var err error
	var exps []E
	var n int
	if n, err = ParseLineNumber(); err != nil {
		return exps, err
	}
	exps = append(exps, E{num: float64(n)})
	return exps, nil
}

// ParseStatementIf parses an IF statement.
func ParseStatementIf() ([]E, error) {
	var e E
	var err error
	var exps []E
	var n int
	var r string
	// an IF statement is an expression
	if e, err = ParseExpression(1); err != nil {
		return exps, err
	}
	exps = append(exps, e)
	// followed by a relational operator
	if r, err = ParseRelational(); err != nil {
		return exps, err
	}
	exps = append(exps, E{str: r})
	// followed by another expression
	if e, err = ParseExpression(1); err != nil {
		return exps, err
	}
	exps = append(exps, e)
	// followed by THEN
	if PopString("THEN") == nil {
		return exps, fmt.Errorf(`expecting "THEN"`)
	}
	// and a line number
	if n, err = ParseLineNumber(); err != nil {
		return exps, err
	}
	exps = append(exps, E{num: float64(n)})
	return exps, nil
}

// ParseRelational parses a relational operator.
func ParseRelational() (string, error) {
	if t := Pop(); t != nil && Relationals[*t] {
		return *t, nil
	}
	return "", fmt.Errorf("expected a relational operator")
}

// Relationals is a map of relational operators.
var Relationals = map[string]bool{
	"<":  true,
	"=":  true,
	">":  true,
	"<=": true,
	"<>": true,
	">=": true,
}

// ParseStatementFor parses a FOR statement.
func ParseStatementFor() ([]E, error) {
	var e E
	var err error
	var exps []E
	var n string
	// a FOR statement is a variable name
	if n, err = ParseVariableName(); err != nil {
		return exps, err
	}
	exps = append(exps, E{str: n})
	// followed by =
	if PopString("=") == nil {
		return exps, fmt.Errorf(`expecting "="`)
	}
	// followed by an expression
	if e, err = ParseExpression(1); err != nil {
		return exps, err
	}
	exps = append(exps, e)
	// followed by TO
	if PopString("TO") == nil {
		return exps, fmt.Errorf(`expecting "TO"`)
	}
	// followed by another expression
	if e, err = ParseExpression(1); err != nil {
		return exps, err
	}
	exps = append(exps, e)
	// followed by the an optional STEP which defaults to 1.0
	if PopString("STEP") != nil {
		if e, err = ParseExpression(1); err != nil {
			return exps, fmt.Errorf("invalid STEP value")
		}
		exps = append(exps, e)
	} else {
		exps = append(exps, E{num: 1.0})
	}
	return exps, nil
}

// ParseStatementNext parses a NEXT statement.
func ParseStatementNext() ([]E, error) {
	var err error
	var exps []E
	var n string
	if n, err = ParseVariableName(); err != nil {
		return exps, err
	}
	exps = append(exps, E{str: n})
	return exps, nil
}

// ParseStatementEmpty returns an empty E slice.
func ParseStatementEmpty() ([]E, error) {
	var e []E
	return e, nil
}

// ParseStatementDef parses a DEF statement.
func ParseStatementDef() ([]E, error) {
	var e E
	var err error
	var exps []E
	var n string
	// a DEF is a function name
	if n, err = ParseFunctionName(); err != nil {
		return exps, err
	}
	exps = append(exps, E{str: n})
	// followed by (
	if PopString("(") == nil {
		return exps, fmt.Errorf(`expecting "("`)
	}
	// followed by a single variable name
	if n, err = ParseVariableName(); err != nil {
		return exps, err
	}
	exps = append(exps, E{str: n})
	// followed by )
	if PopString(")") == nil {
		return exps, fmt.Errorf(`expecting ")"`)
	}
	// followed by =
	if PopString("=") == nil {
		return exps, fmt.Errorf(`expecting "="`)
	}
	// followed by an expression
	if e, err = ParseExpression(1); err != nil {
		return exps, err
	}
	exps = append(exps, e)
	return exps, nil
}

// ParseFunctionName parses a function name.
func ParseFunctionName() (string, error) {
	if MatchFuncName(Peek()) {
		return *Pop(), nil
	}
	return "", fmt.Errorf("expected a function name")
}

// ParseStatementRem parses a REM statement.
func ParseStatementRem() ([]E, error) {
	var exps []E
	// clear Tokens
	Tokens = []string{}
	return exps, nil
}

type byLine []S

func (x byLine) Len() int           { return len(x) }
func (x byLine) Less(i, j int) bool { return x[i].line < x[j].line }
func (x byLine) Swap(i, j int)      { x[i], x[j] = x[j], x[i] }

// SortStatements sorts a slice of statements S
func SortStatements(s []S) { sort.Sort(byLine(s)) }

// Run runs the program p.
func Run(p string) {
	// program counter, GOSUB return value
	var pc, ret int
	// maps variable names to ForState
	fors := make(map[string]ForState)

	statements := ParseProgram(p)
	if len(statements) == 0 {
		return
	}

	// maps line numbers to statement indices
	gotos := make(map[int]int)
	for i, s := range statements {
		gotos[s.line] = i
	}

	PreProcessStatements(statements)

	for pc < len(statements) {
		s := statements[pc]
		if *debug {
			fmt.Printf(
				"debug:\n\tline: %d\n\tpc = %d\n\tstatement: %s\n\tdata: %v\n\tvariables: %v\n\tfors: %v\n\tgotos: %v\n", s.line, pc, s.kind, data, variables, fors, gotos)
			os.Stdin.Read(make([]byte, 1))
		}
		pc += 1
		switch s.kind {
		case "END", "STOP":
			return
		case "LET":
			variables[s.exps[0].vbl.Name()] = Eval(s.exps[1])
		case "READ":
			if len(data) > 0 {
				for _, e := range s.exps {
					variables[e.vbl.Name()] = PopData()
				}
			} else {
				return
			}
		case "PRINT":
			PrintE(s.exps)
		case "GOTO":
			pc = gotos[int(s.exps[0].num)]
		case "IF":
			if functions[s.exps[1].str].CallB(
				Eval(s.exps[0]),
				Eval(s.exps[2])) {
				pc = gotos[int(s.exps[3].num)]
			}
		case "FOR":
			variables[s.exps[0].str] = Eval(s.exps[1])
			fors[s.exps[0].str] = ForState{
				cont: pc,
				end:  Eval(s.exps[2]),
				step: Eval(s.exps[3]),
			}
		case "NEXT":
			n := s.exps[0].str
			f := fors[n]
			incr := variables[n] + f.step
			if (f.step >= 0 && incr <= f.end) || (f.step < 0 && incr >= f.end) {
				variables[n] = incr
				pc = f.cont
			}
		case "GOSUB":
			ret = pc
			pc = gotos[int(s.exps[0].num)]
		case "RETURN":
			pc = ret
		}
	}

}

// ForState represents the runtime state of for statements.
type ForState struct {
	cont int
	end  float64
	step float64
}

// variables maps variables to their float64 values.
var variables = map[string]float64{}

// keeps track of columns
var column = 0

// PreProcessStatements preprocesses statements s, setting up user defined
// functions and data values.
func PreProcessStatements(s []S) {
	for _, statement := range s {
		exps := statement.exps
		switch statement.kind {
		case "DEF":
			functions[exps[0].str] = Function{param: exps[1].str, body: exps[2]}
		case "DATA":
			for i := range exps {
				data = append(data, Eval(exps[i]))
			}
		}
	}
}

// data is a slice of data available for READ statements.
var data []float64

// functions maps function names to predefined Function values.
var functions = map[string]Function{
	"-": Function{
		ocall2: func(x, y float64) float64 { return x - y },
	},
	"*": Function{
		ocall2: func(x, y float64) float64 { return x * y },
	},
	"/": Function{
		ocall2: func(x, y float64) float64 { return x / y },
	},
	"%": Function{
		ocall2: func(x, y float64) float64 { return math.Mod(x, y) },
	},
	"^": Function{
		ocall2: func(x, y float64) float64 { return math.Pow(x, y) },
	},
	"+": Function{
		ocall2: func(x, y float64) float64 { return x + y },
	},
	"<": Function{
		ocallb: func(x, y float64) bool { return x < y },
	},
	"<=": Function{
		ocallb: func(x, y float64) bool { return x <= y },
	},
	"<>": Function{
		ocallb: func(x, y float64) bool { return x != y },
	},
	"=": Function{
		ocallb: func(x, y float64) bool { return x == y },
	},
	">": Function{
		ocallb: func(x, y float64) bool { return x > y },
	},
	">=": Function{
		ocallb: func(x, y float64) bool { return x >= y },
	},
	"ABS": Function{
		fcall1: func(x float64) float64 { return math.Abs(x) },
	},
	"ATN": Function{
		fcall1: func(x float64) float64 { return math.Atan(x) },
	},
	"COS": Function{
		fcall1: func(x float64) float64 { return math.Cos(x) },
	},
	"EXP": Function{
		fcall1: func(x float64) float64 { return math.Exp(x) },
	},
	// really cheating here...
	"INT": Function{
		fcall1: func(x float64) float64 { return float64(int(x)) },
	},
	"LOG": Function{
		fcall1: func(x float64) float64 { return math.Log(x) },
	},
	"NEG": Function{
		fcall1: func(x float64) float64 { return -x },
	},
	"RND": Function{
		fcall1: func(_ float64) float64 { return rand.Float64() },
	},
	"SIN": Function{
		fcall1: func(x float64) float64 { return math.Sin(x) },
	},
	"SQR": Function{
		fcall1: func(x float64) float64 { return math.Sqrt(x) },
	},
	"TAN": Function{
		fcall1: func(x float64) float64 { return math.Tan(x) },
	},
}

// Function represents a function.
type Function struct {
	fcall1 func(float64) float64
	ocall2 func(float64, float64) float64
	ocallb func(float64, float64) bool
	param  string
	body   E
}

// Call1 is a function call with one parameter returning a float.
func (f Function) Call1(x float64) float64 {
	variables[f.param] = x
	if f.fcall1 != nil {
		return f.fcall1(x)
	}
	return Eval(f.body)
}

// Eval evaluates an expression E returning a float64.
func Eval(e E) float64 {
	switch {
	case e.opc != nil:
		return functions[e.opc.op].Call2(Eval(e.opc.x), Eval(e.opc.y))
	case e.fnc != nil:
		return functions[e.fnc.fn].Call1(Eval(e.fnc.x))
	case e.vbl != nil:
		return variables[e.vbl.Name()]
	case MatchVarName(&e.str):
		return variables[e.str]
	default:
		return e.num
	}
}

// Name returns a name for variable v. It takes into account subscripts.
func (v V) Name() string {
	if len(v.sub) > 0 {
		i := []int{}
		for _, s := range v.sub {
			i = append(i, int(Eval(s)))
		}
		return fmt.Sprintf("%s%v", v.val, i)
	} else {
		return v.val
	}
}

// Call2 is an operation with two parameters returning a float.
func (f Function) Call2(x, y float64) float64 {
	if f.ocall2 != nil {
		return f.ocall2(x, y)
	}
	return Eval(f.body)
}

// CallB is an operation with two parameters returning a boolean.
func (f Function) CallB(x, y float64) bool {
	if f.ocallb != nil {
		return f.ocallb(x, y)
	}
	return false
}

// PopData removes and returns the leftmost value in data, if available.
func PopData() float64 {
	var d float64
	if len(data) > 0 {
		d = data[0]
		data = data[1:]
	}
	return d
}

// PrintE prints labels, ","s, ";"s, and expressions in appropriate columns
func PrintE(e []E) {
	var last string
	for _, exp := range e {
		switch {
		case exp.str == ",":
			Pad(15)
		case exp.str == ";":
			Pad(3)
		case MatchLabel(&exp.str):
			PrintS(strings.Replace(exp.str, `"`, "", -1))
		default:
			n := Eval(exp)
			if IsWhole(n) {
				PrintS(fmt.Sprintf("%d ", int(math.Round(n))))
			} else {
				PrintS(fmt.Sprintf("%f ", n))
			}
		}
		last = exp.str
	}
	if *debug || len(e) == 0 || (last != "," && last != ";") {
		PrintNewLine()
	}
}

// IsWhole takes a float f and returns whether it is a whole number.
func IsWhole(f float64) bool {
	frac := math.Abs(f - math.Trunc(f))
	return frac < 1e-9 || frac > 1-1e-9
}

// eps is the margin of error we are allowing in float conversion.
const eps = 1e-9

// Pad prints empty spaces up to the column that is the next multiple of n
func Pad(n int) {
	for column%n != 0 {
		PrintS(" ")
	}
}

// PrintS prints string s while keeping track of columns. Advances to a new
// line if column is greater than or equal to 100.
func PrintS(s string) {
	fmt.Print(s)
	column += len(s)
	if column > 100 {
		PrintNewLine()
	}
}

// PrintNewLine prints a new line and keeps track of column.
func PrintNewLine() {
	fmt.Println()
	column = 0
}

const usage = "usage: basic2 [-debug] filename"

var debug = flag.Bool("debug", false, "activate debugging")
var filename string

func main() {
	flag.Usage = func() { fmt.Println(usage) }
	flag.Parse()
	if len(os.Args) < 2 {
		fmt.Fprintf(os.Stderr, "basic2: no filename provided.\n%s\n", usage)
		os.Exit(2)
	}
	if len(os.Args) == 3 {
		filename = os.Args[2]
	} else {
		filename = os.Args[1]
	}
	f, err := os.Open(filename)
	defer f.Close()
	if err != nil {
		fmt.Fprintf(os.Stderr, "open file %s failed: %v\n", filename, err)
		os.Exit(1)
	}
	b, err := ioutil.ReadAll(f)
	if err != nil {
		fmt.Fprintf(os.Stderr, "reading file %s failed: %v\n", filename, err)
		os.Exit(1)
	}
	Run(string(b))
}
