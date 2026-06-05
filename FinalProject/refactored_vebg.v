import math

pub type ExprC = NumC | StrC | IdC | IfC | LamC | AppC
pub type Value = BoolV | NumV | ClosV | PrimOpV | StringV | VoidV
pub type Env = []Binding

pub struct Binding {
    name string
    val  &Value
}

pub struct NumC  { n f64 }
pub struct StrC  { s string }
pub struct IdC   { name string }
pub struct IfC   { test ExprC then ExprC else_br ExprC }
pub struct LamC  { params []string body ExprC }
pub struct AppC  { func ExprC args []ExprC }

pub struct BoolV   { b bool }
pub struct NumV    { n f64 }
pub struct ClosV   { args []string body ExprC env Env }
pub struct PrimOpV { op string }
pub struct StringV { s string }
pub struct VoidV   {}


fn extend_env(binds []Binding, env []Binding) []Binding {
	mut new_env := binds.clone()
	new_env << env
	return new_env
}

fn lookup(name string, env []Binding) &Value {
	for b in env {
		if b.name == name {
			return b.val
		}
	}
	panic('lookup: name not found: ${name} (VEBG)')
}

const dni_table = ['->','fn','if','given','=']

fn is_banned(s string) bool {
	return s in dni_table
}


pub fn op_to_meaning(op string, args []&Value) &Value {
    match op {
        '+' {
            if args.len == 2 && args[0] is NumV && args[1] is NumV {
                return &NumV{n: (args[0] as NumV).n + (args[1] as NumV).n}
            } else {
                panic('opToMeaning: invalid args for + (VEBG)')
            }
        }
        '-' {
            if args.len == 2 && args[0] is NumV && args[1] is NumV {
                return &NumV{n: (args[0] as NumV).n - (args[1] as NumV).n}
            } else {
                panic('opToMeaning: invalid args for - (VEBG)')
            }
        }
        '*' {
            if args.len == 2 && args[0] is NumV && args[1] is NumV {
                return &NumV{n: (args[0] as NumV).n * (args[1] as NumV).n}
            } else {
                panic('opToMeaning: invalid args for * (VEBG)')
            }
        }
        '/' {
            if args.len == 2 && args[0] is NumV && args[1] is NumV {
                b := (args[1] as NumV).n
                if b == 0 {
                    panic('opToMeaning: division by zero (VEBG)')
                }
                return &NumV{n: (args[0] as NumV).n / b}
            } else {
                panic('opToMeaning: invalid args for / (VEBG)')
            }
        }
        '<=' {
            if args.len == 2 && args[0] is NumV && args[1] is NumV {
                return &BoolV{b: (args[0] as NumV).n <= (args[1] as NumV).n}
            } else {
                panic('opToMeaning: invalid args for <= (VEBG)')
            }
        }
        'equal?' {
            if args.len != 2 { panic('opToMeaning: invalid args for equal? (VEBG)') }
            if args[0] is NumV && args[1] is NumV {
                return &BoolV{b: (args[0] as NumV).n == (args[1] as NumV).n}
            } else if args[0] is StringV && args[1] is StringV {
                return &BoolV{b: (args[0] as StringV).s == (args[1] as StringV).s}
            } else if args[0] is BoolV && args[1] is BoolV {
                return &BoolV{b: (args[0] as BoolV).b == (args[1] as BoolV).b}
            } else {
                return &BoolV{b: false}
            }
        }
        'substring' {
            if args.len == 3 && args[0] is StringV && args[1] is NumV && args[2] is NumV {
                s     := (args[0] as StringV).s
                start := int((args[1] as NumV).n)
                stop  := int((args[2] as NumV).n)
                if start < 0 || stop < 0 || start >= s.len || stop > s.len || start > stop {
                    panic('opToMeaning: substring index out of range (VEBG)')
                }
                return &StringV{s: s[start..stop]}
            } else {
                panic('opToMeaning: invalid args for substring (VEBG)')
            }
        }
        'strlen' {
            if args.len == 1 && args[0] is StringV {
                return &NumV{n: f64((args[0] as StringV).s.len)}
            } else {
                panic('opToMeaning: invalid args for strlen (VEBG)')
            }
        }
        'error' {
            if args.len == 1 {
                msg := if args[0] is StringV { (args[0] as StringV).s } else { serialize(args[0]) }
                panic('user-error: ${msg}')
            }
            panic('opToMeaning: invalid args for error (VEBG)')
        }
        'crying' {
            if args.len == 1 && args[0] is BoolV {
                if (args[0] as BoolV).b {
                    println('🥹')
                } else {
                    println('🥺')
                }
                return &VoidV{}
            } else {
                panic('opToMeaning: invalid args for crying (VEBG)')
            }
        }
        else {
            panic('opToMeaning: invalid operation: ${op} (VEBG)')
        }
    }
}


fn serialize(v &Value) string {
	match v {
		NumV    {
			n := v.n
			if n == math.floor(n) && !math.is_inf(n, 0) {
				return i64(n).str()
			}
			return n.str()
		}
		BoolV   { return if v.b { 'true' } else { 'false' } }
		ClosV   { return '#<procedure>' }
		PrimOpV{ return '#<primop>' }
		StringV { return '"${v.s}"' }
		VoidV   { return '' }
	}
}


 fn interp(expr ExprC, envi []Binding) &Value {
  	match expr {
  		NumC {
  			return &NumV{n : (expr as NumC).n}
  		}
  		StrC {
  			return &StringV{s: (expr as StrC).s}
  		}
  		IdC {
  			return lookup((expr as IdC).name, envi)
  		}
  		LamC {
  			return &ClosV{args: (expr as LamC).params, body: (expr as LamC).body, env: envi}
  		}
  		IfC {
  			cond_val := interp((expr as IfC).test, envi)
  			if cond_val is BoolV {
  				if cond_val.b {
  					return interp((expr as IfC).then, envi)
  				}
  				return interp((expr as IfC).else_br, envi)
  			}
  			panic('interp: condition not a boolean expression (VEBG)')
  		}
  		AppC {
  			fd := interp((expr as AppC).func, envi)
			mut evaluated_args := []&Value{}
			for a in (expr as AppC).args {
				evaluated_args << interp(a, envi)
			}
			match fd {
				PrimOpV {
					return op_to_meaning((fd as PrimOpV).op, evaluated_args)
				}
				ClosV {
					if (fd as ClosV).args.len != evaluated_args.len {
						panic('interp: wrong number of arguments applied to function (VEBG)')
					}
					mut new_binds := []Binding{}
					for i, p in (fd as ClosV).args {
						new_binds << Binding{ name: p, val: evaluated_args[i] }
					}
					new_env := extend_env(new_binds, (fd as ClosV).env)
					return interp((fd as ClosV).body, new_env)
				}
				else {
					panic('interp: poorly formatted function call (VEBG)')
				}
			}
  		}
  	}
  }


// helpers to reduce boilerplate
fn top_env() []Binding {
    mut env := []Binding{}
    env << Binding{ name: '+',        val: &PrimOpV{op: '+'} }
    env << Binding{ name: '-',        val: &PrimOpV{op: '-'} }
    env << Binding{ name: '*',        val: &PrimOpV{op: '*'} }
    env << Binding{ name: '/',        val: &PrimOpV{op: '/'} }
    env << Binding{ name: '<=',       val: &PrimOpV{op: '<='} }
    env << Binding{ name: 'equal?',   val: &PrimOpV{op: 'equal?'} }
    env << Binding{ name: 'substring',val: &PrimOpV{op: 'substring'} }
    env << Binding{ name: 'strlen',   val: &PrimOpV{op: 'strlen'} }
    env << Binding{ name: 'crying',   val: &PrimOpV{op: 'crying'} }
    return env
}

fn interp_top(expr ExprC) &Value {
    return interp(expr, top_env())
}

// ── arithmetic ────────────────────────────────────────────────────────────────

fn test_add() {
    // (+ 3 4)  =>  7
    result := interp_top(AppC{
        func: IdC{ name: '+' }
        args: [ExprC(NumC{n: 3}), ExprC(NumC{n: 4})]
    })
    assert result is NumV
    assert (result as NumV).n == 7
}

fn test_sub() {
    // (- 10 3)  =>  7
    result := interp_top(AppC{
        func: IdC{ name: '-' }
        args: [ExprC(NumC{n: 10}), ExprC(NumC{n: 3})]
    })
    assert (result as NumV).n == 7
}

fn test_mul() {
    result := interp_top(AppC{
        func: IdC{ name: '*' }
        args: [ExprC(NumC{n: 6}), ExprC(NumC{n: 7})]
    })
    assert (result as NumV).n == 42
}

fn test_div() {
    result := interp_top(AppC{
        func: IdC{ name: '/' }
        args: [ExprC(NumC{n: 10}), ExprC(NumC{n: 4})]
    })
    assert (result as NumV).n == 2.5
}

// ── comparisons ───────────────────────────────────────────────────────────────

fn test_lte_true() {
    result := interp_top(AppC{
        func: IdC{ name: '<=' }
        args: [ExprC(NumC{n: 3}), ExprC(NumC{n: 3})]
    })
    assert (result as BoolV).b == true
}

fn test_lte_false() {
    result := interp_top(AppC{
        func: IdC{ name: '<=' }
        args: [ExprC(NumC{n: 5}), ExprC(NumC{n: 3})]
    })
    assert (result as BoolV).b == false
}

fn test_equal_nums() {
    result := interp_top(AppC{
        func: IdC{ name: 'equal?' }
        args: [ExprC(NumC{n: 4}), ExprC(NumC{n: 4})]
    })
    assert (result as BoolV).b == true
}

fn test_equal_strings() {
    result := interp_top(AppC{
        func: IdC{ name: 'equal?' }
        args: [ExprC(StrC{s: 'hello'}), ExprC(StrC{s: 'hello'})]
    })
    assert (result as BoolV).b == true
}

fn test_equal_type_mismatch() {
    // num vs string => false, not a panic
    result := interp_top(AppC{
        func: IdC{ name: 'equal?' }
        args: [ExprC(NumC{n: 1}), ExprC(StrC{s: '1'})]
    })
    assert (result as BoolV).b == false
}

// ── strings ───────────────────────────────────────────────────────────────────

fn test_strlen() {
    result := interp_top(AppC{
        func: IdC{ name: 'strlen' }
        args: [ExprC(StrC{s: 'hello'})]
    })
    assert (result as NumV).n == 5
}

fn test_substring() {
    // (substring "hello" 1 3)  =>  "el"
    result := interp_top(AppC{
        func: IdC{ name: 'substring' }
        args: [ExprC(StrC{s: 'hello'}), ExprC(NumC{n: 1}), ExprC(NumC{n: 3})]
    })
    assert (result as StringV).s == 'el'
}

// ── if ────────────────────────────────────────────────────────────────────────

fn test_if_true_branch() {
    // (if true 1 2)  =>  1
    result := interp_top(IfC{
        test:    IdC{ name: 'true' }
        then:    NumC{n: 1}
        else_br: NumC{n: 2}
    })
    // easier: build the condition as an equal? call
    _ := result // placeholder; see cleaner version below
}

fn test_if_picks_then() {
    // (if (<= 1 2) 42 99)  =>  42
    result := interp_top(IfC{
        test: AppC{
            func: IdC{ name: '<=' }
            args: [ExprC(NumC{n: 1}), ExprC(NumC{n: 2})]
        }
        then:    NumC{n: 42}
        else_br: NumC{n: 99}
    })
    assert (result as NumV).n == 42
}

fn test_if_picks_else() {
    // (if (<= 5 2) 42 99)  =>  99
    result := interp_top(IfC{
        test: AppC{
            func: IdC{ name: '<=' }
            args: [ExprC(NumC{n: 5}), ExprC(NumC{n: 2})]
        }
        then:    NumC{n: 42}
        else_br: NumC{n: 99}
    })
    assert (result as NumV).n == 99
}

// ── lambda + closure ──────────────────────────────────────────────────────────

fn test_identity_lambda() {
    // ((fn (x) x) 7)  =>  7
    result := interp_top(AppC{
        func: LamC{ params: ['x'], body: IdC{ name: 'x' } }
        args: [ExprC(NumC{n: 7})]
    })
    assert (result as NumV).n == 7
}

fn test_lambda_add() {
    // ((fn (x y) (+ x y)) 3 4)  =>  7
    result := interp_top(AppC{
        func: LamC{
            params: ['x', 'y']
            body: AppC{
                func: IdC{ name: '+' }
                args: [ExprC(IdC{name: 'x'}), ExprC(IdC{name: 'y'})]
            }
        }
        args: [ExprC(NumC{n: 3}), ExprC(NumC{n: 4})]
    })
    assert (result as NumV).n == 7
}

fn test_closure_captures_env() {
    // ((fn (x) (fn (y) (+ x y))) 10) applied to 5  =>  15
    outer := interp_top(AppC{
        func: LamC{
            params: ['x']
            body: LamC{
                params: ['y']
                body: AppC{
                    func: IdC{ name: '+' }
                    args: [ExprC(IdC{name: 'x'}), ExprC(IdC{name: 'y'})]
                }
            }
        }
        args: [ExprC(NumC{n: 10})]
    })
    // outer is now a ClosV; apply it to 5
    result := interp_top(AppC{
        func: LamC{  // wrap in another lambda so we can pass the closure as a value
            params: ['f']
            body: AppC{
                func: IdC{ name: 'f' }
                args: [ExprC(NumC{n: 5})]
            }
        }
        args: [ExprC(NumC{n: 10})]  // placeholder — see note below
    })
    // The cleanest way to test this end-to-end is one expression:
    // (((fn (x) (fn (y) (+ x y))) 10) 5)
    result2 := interp_top(AppC{
        func: AppC{
            func: LamC{
                params: ['x']
                body: LamC{
                    params: ['y']
                    body: AppC{
                        func: IdC{ name: '+' }
                        args: [ExprC(IdC{name: 'x'}), ExprC(IdC{name: 'y'})]
                    }
                }
            }
            args: [ExprC(NumC{n: 10})]
        }
        args: [ExprC(NumC{n: 5})]
    })
    assert (result2 as NumV).n == 15
}

// ── serialize ─────────────────────────────────────────────────────────────────

fn test_serialize_int() {
    assert serialize(&NumV{n: 5.0})  == '5'
}

fn test_serialize_float() {
    assert serialize(&NumV{n: 2.5})  == '2.5'
}

fn test_serialize_bool() {
    assert serialize(&BoolV{b: true})  == 'true'
    assert serialize(&BoolV{b: false}) == 'false'
}

fn test_serialize_string() {
    assert serialize(&StringV{s: 'hi'}) == '"hi"'
}

fn test_serialize_closure() {
    assert serialize(&ClosV{args: [], body: NumC{n: 0}, env: []Binding{}}) == '#<procedure>'
}


//*********************************************RUN TESTS*********************************************
fn main() {
    mut passed := 0
    mut failed := 0
    test_add()
    test_sub()
    test_mul()
    test_div()
    test_lte_true()
    test_lte_false()
    test_equal_nums()
    test_equal_strings()
    test_equal_type_mismatch()
    test_strlen()
    test_substring()
    test_if_true_branch()
    test_if_picks_then()
    test_if_picks_else()
    test_identity_lambda()
    test_lambda_add()
    test_closure_captures_env()
    test_serialize_int()
    test_serialize_float()
    test_serialize_bool()
    test_serialize_string()
    test_serialize_closure()
    println('All tests passed!')

}
