package main

import (
	"reflect"
	"runtime"
	"testing"
)

func FunctionName() string {
	pcs := make([]uintptr, 10)
	n := runtime.Callers(2, pcs)
	frame, _ := runtime.CallersFrames(pcs[:n]).Next()
	return frame.Function
}

func TestGetTokens(t *testing.T) {
	var tests = []struct {
		input string
		want  []string
	}{
		{"X-1", []string{"X", "-", "1"}},
		{`PRINT "HELLO WORLD"`, []string{"PRINT", `"HELLO WORLD"`}},
		{"10 GOTO 99", []string{"10", "GOTO", "99"}},
		{"10GOTO99", []string{"10", "GOTO", "99"}},
		{"10 GO TO 99", []string{"10", "GOTO", "99"}},
		{"SIN(X) ^ 2", []string{"SIN", "(", "X", ")", "^", "2"}},
		{"100IFX1+123.4", []string{"100", "IF", "X1", "+", "123.4"}},
		{"+E1-12.3E4", []string{"+", "E1", "-", "12.3E4"}},
		{`+1+"HI" THEN99`, []string{"+", "1", "+", `"HI"`, "THEN", "99"}},
		{"<> 1.2E-34*-12E34", []string{"<>", "1.2E-34", "*", "-", "12E34"}},
	}
	for i := range tests {
		got := GetTokens(tests[i].input)
		want := tests[i].want
		if !reflect.DeepEqual(got, want) {
			t.Errorf("GetTokens(%q) = %q, want %q", tests[i].input, got, want)
		}
	}
}

func PtrString(s string) *string { return &s }

func TestPeekAndPop(t *testing.T) {
	Tokens = GetTokens("10 GO TO 99")
	var tests = []struct {
		name string
		op   func() *string
		want *string
	}{
		{"Peek()", Peek, PtrString("10")},
		{"Pop()", Pop, PtrString("10")},
		{"Peek()", Peek, PtrString("GOTO")},
		{"Pop()", Pop, PtrString("GOTO")},
		{"Peek()", Peek, PtrString("99")},
		{"Pop()", Pop, PtrString("99")},
		{"Peek()", Peek, nil},
	}
	for i := range tests {
		got := tests[i].op()
		want := tests[i].want
		if !reflect.DeepEqual(got, want) {
			t.Errorf("%s = %v, want %v", tests[i].name, got, want)
		}
	}
}

func TestSplitLines(t *testing.T) {
	var tests = []struct {
		input string
		want  []string
	}{
		{"\n\n", []string{}},
		{"10 REM COUNT", []string{"10 REM COUNT"}},
		{"10 REM COUNT\n20 END", []string{"10 REM COUNT", "20 END"}},
	}
	for i := range tests {
		got := SplitLines(tests[i].input)
		want := tests[i].want
		if !reflect.DeepEqual(got, want) {
			t.Errorf("SplitLines(%q): got %q, want %q", tests[i].input, got, want)
		}
	}
}

func TestParseLet1(t *testing.T) {
	program := "10 LET A = 1"
	want := []S{
		S{line: 10, kind: "LET", exps: []E{
			E{vbl: &V{val: "A"}},
			E{num: float64(1)},
		}},
	}
	got := ParseProgram(program)
	for i := range got {
		if !reflect.DeepEqual(got[i], want[i]) {
			t.Errorf("got %v, want %v", got[i], want[i])
		}
	}
}

func TestParseLet2(t *testing.T) {
	program := "20 LET A(1, 2) = 2"
	want := []S{
		S{line: 20, kind: "LET", exps: []E{
			E{vbl: &V{val: "A", sub: []E{
				E{num: float64(1)},
				E{num: float64(2)},
			}}},
			E{num: float64(2)},
		}},
	}
	got := ParseProgram(program)
	for i := range got {
		if !reflect.DeepEqual(got[i], want[i]) {
			t.Errorf("got %v, want %v", got[i], want[i])
		}
	}
}

func TestParseLet3(t *testing.T) {
	program := "30 LET A = 1 + 3 * 5"
	want := []S{
		S{line: 30, kind: "LET", exps: []E{
			E{vbl: &V{val: "A"}},
			E{opc: &O{
				x:  E{num: float64(1)},
				op: "+",
				y: E{opc: &O{
					x:  E{num: float64(3)},
					op: "*",
					y:  E{num: float64(5)},
				}},
			}},
		}},
	}
	got := ParseProgram(program)
	for i := range got {
		if !reflect.DeepEqual(got[i], want[i]) {
			t.Errorf("got %v, want %v", got[i], want[i])
		}
	}
}

func TestParseRead(t *testing.T) {
	program := "10 READ N0, P0"
	want := []S{
		S{line: 10, kind: "READ", exps: []E{
			E{vbl: &V{val: "N0"}},
			E{vbl: &V{val: "P0"}},
		}},
	}
	got := ParseProgram(program)
	for i := range got {
		if !reflect.DeepEqual(got[i], want[i]) {
			t.Errorf("got %v, want %v", got[i], want[i])
		}
	}
}

func TestParseDim(t *testing.T) {
	program := "10 DIM S(3, 5)\n20 DIM A, B"
	want := []S{
		S{line: 10, kind: "DIM", exps: []E{
			E{vbl: &V{
				val: "S",
				sub: []E{
					E{num: float64(3)},
					E{num: float64(5)},
				},
			}},
		}},
		S{line: 20, kind: "DIM", exps: []E{
			E{vbl: &V{val: "A"}},
			E{vbl: &V{val: "B"}},
		}},
	}
	got := ParseProgram(program)
	SortStatements(want)
	for i := range got {
		if !reflect.DeepEqual(got[i], want[i]) {
			t.Errorf("got %v, want %v", got[i], want[i])
		}
	}
}

func TestParseData(t *testing.T) {
	program := "11 DATA 8, 4"
	want := []S{
		S{line: 11, kind: "DATA", exps: []E{
			E{num: float64(8)},
			E{num: float64(4)},
		}},
	}
	got := ParseProgram(program)
	for i := range got {
		if !reflect.DeepEqual(got[i], want[i]) {
			t.Errorf("got %v, want %v", got[i], want[i])
		}
	}
}

func TestParsePrint(t *testing.T) {
	program := `20 PRINT "N",N0;`
	want := []S{
		S{line: 20, kind: "PRINT", exps: []E{
			E{str: `"N"`},
			E{str: ","},
			E{vbl: &V{val: "N0"}},
			E{str: ";"},
		}},
	}
	got := ParseProgram(program)
	for i := range got {
		if !reflect.DeepEqual(got[i], want[i]) {
			t.Errorf("got %v, want %v", got[i], want[i])
		}
	}
}

func TestParseGotoOrGosub(t *testing.T) {
	program := "10 GOTO 99\n20 GOSUB 10"
	want := []S{
		S{line: 10, kind: "GOTO", exps: []E{
			E{num: float64(99)},
		}},
		S{line: 20, kind: "GOSUB", exps: []E{
			E{num: float64(10)},
		}},
	}
	got := ParseProgram(program)
	for i := range got {
		if !reflect.DeepEqual(got[i], want[i]) {
			t.Errorf("got %v, want %v", got[i], want[i])
		}
	}
}

func TestParseIf(t *testing.T) {
	program := "10 IF A = 0 THEN 99"
	want := []S{
		S{line: 10, kind: "IF", exps: []E{
			E{vbl: &V{val: "A"}},
			E{str: "="},
			E{num: float64(0)},
			E{num: float64(99)},
		}},
	}
	got := ParseProgram(program)
	for i := range got {
		if !reflect.DeepEqual(got[i], want[i]) {
			t.Errorf("got %v, want %v", got[i], want[i])
		}
	}
}

func TestParseRemEndStopReturn(t *testing.T) {
	program := "10 REM\n20 END\n30 STOP\n40 RETURN"
	want := []S{
		S{line: 10, kind: "REM"},
		S{line: 20, kind: "END"},
		S{line: 30, kind: "STOP"},
		S{line: 40, kind: "RETURN"},
	}
	got := ParseProgram(program)
	SortStatements(want)
	for i := range got {
		if !reflect.DeepEqual(got[i], want[i]) {
			t.Errorf("got %v, want %v", got[i], want[i])
		}
	}
}

func TestParseFor(t *testing.T) {
	program := "10 FOR I = 1 TO 10\n20 FOR I = 5 TO 11 STEP 2"
	want := []S{
		S{line: 10, kind: "FOR", exps: []E{
			E{str: "I"},
			E{num: float64(1)},
			E{num: float64(10)},
			E{num: float64(1)},
		}},
		S{line: 20, kind: "FOR", exps: []E{
			E{str: "I"},
			E{num: float64(5)},
			E{num: float64(11)},
			E{num: float64(2)},
		}},
	}
	got := ParseProgram(program)
	SortStatements(want)
	for i := range got {
		if !reflect.DeepEqual(got[i], want[i]) {
			t.Errorf("got %v, want %v", got[i], want[i])
		}
	}
}

func TestParseNext(t *testing.T) {
	program := "10 NEXT I"
	want := []S{
		S{line: 10, kind: "NEXT", exps: []E{
			E{str: "I"},
		}},
	}
	got := ParseProgram(program)
	for i := range got {
		if !reflect.DeepEqual(got[i], want[i]) {
			t.Errorf("got %v, want %v", got[i], want[i])
		}
	}
}

func TestParseDef(t *testing.T) {
	program := "10 DEF SQR(X) = X * X"
	want := []S{
		S{line: 10, kind: "DEF", exps: []E{
			E{str: "SQR"},
			E{str: "X"},
			E{opc: &O{
				x:  E{vbl: &V{val: "X"}},
				op: "*",
				y:  E{vbl: &V{val: "X"}},
			}},
		}},
	}
	got := ParseProgram(program)
	for i := range got {
		if !reflect.DeepEqual(got[i], want[i]) {
			t.Errorf("got %v, want %v", got[i], want[i])
		}
	}
}

func TestExpressions(t *testing.T) {
	var tests = []struct {
		input string
		want  E
	}{
		{"A + B * X + C", E{opc: &O{
			x: E{opc: &O{
				x:  E{vbl: &V{val: "A"}},
				op: "+",
				y: E{opc: &O{
					x:  E{vbl: &V{val: "B"}},
					op: "*",
					y:  E{vbl: &V{val: "X"}},
				}},
			}},
			op: "+",
			y:  E{vbl: &V{val: "C"}},
		}}},
		{"A + B + X + C", E{opc: &O{
			x: E{opc: &O{
				x: E{opc: &O{
					x:  E{vbl: &V{val: "A"}},
					op: "+",
					y:  E{vbl: &V{val: "B"}},
				}},
				op: "+",
				y:  E{vbl: &V{val: "X"}},
			}},
			op: "+",
			y:  E{vbl: &V{val: "C"}},
		}}},
		{"SIN(X)^2", E{opc: &O{
			x: E{fnc: &F{
				fn: "SIN",
				x:  E{vbl: &V{val: "X"}},
			}},
			op: "^",
			y:  E{num: float64(2)},
		}}},
		// right associativity
		{"10 ^ 2 ^ 3", E{opc: &O{
			x:  E{num: float64(10)},
			op: "^",
			y: E{opc: &O{
				x:  E{num: float64(2)},
				op: "^",
				y:  E{num: float64(3)},
			}},
		}}},
		// left associativity
		{"10 - 2 - 3", E{opc: &O{
			x: E{opc: &O{
				x:  E{num: float64(10)},
				op: "-",
				y:  E{num: float64(2)},
			}},
			op: "-",
			y:  E{num: float64(3)},
		}}},
		{"A(I)+M(I, J)", E{opc: &O{
			x: E{vbl: &V{
				val: "A",
				sub: []E{
					E{vbl: &V{val: "I"}},
				},
			}},
			op: "+",
			y: E{vbl: &V{
				val: "M",
				sub: []E{
					E{vbl: &V{val: "I"}},
					E{vbl: &V{val: "J"}},
				},
			}},
		}}},
		{"X * -1", E{opc: &O{
			x:  E{vbl: &V{val: "X"}},
			op: "*",
			y: E{fnc: &F{
				fn: "NEG",
				x:  E{num: float64(1)},
			}},
		}}},
		{"X--Y--Z", E{opc: &O{
			x: E{opc: &O{
				x:  E{vbl: &V{val: "X"}},
				op: "-",
				y: E{fnc: &F{
					fn: "NEG",
					x:  E{vbl: &V{val: "Y"}},
				}},
			}},
			op: "-",
			y: E{fnc: &F{
				fn: "NEG",
				x:  E{vbl: &V{val: "Z"}},
			}},
		}}},
		{"((((X))))", E{vbl: &V{val: "X"}}},
	}
	for _, test := range tests {
		Tokens = GetTokens(test.input)
		got, err := ParseExpression(1)
		if err != nil {
			t.Errorf("cannot parse input %s", test.input)
		}
		if !reflect.DeepEqual(got, test.want) {
			t.Errorf("input %s, got %v, want %v", test.input, got, test.want)
		}
	}
}

func TestParseProgram(t *testing.T) {
	program := `10 REM POWER TABLE
	11 DATA 8, 4
	15 READ N0, P0
	20 PRINT "N",
	25 FOR P = 2 TO P0
	30   PRINT "N ^" P,
	35 NEXT P
	40 PRINT "SUM"
	45 LET S = 0
	50 FOR N = 2 TO N0
	55   PRINT N,
	60   FOR P = 2 TO P0
	65     LET S = S + N ^ P
	70     PRINT N ^ P,
	75   NEXT P
	80   PRINT S
	85 NEXT N
	99 END`
	ParseProgram(program)
	if Pop() != nil {
		t.Errorf("Program not successfully parsed")
	}
}
