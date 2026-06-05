// struct Dog {}
// struct Cat {}
// struct Veasel {}
// type Animal = Dog | Cat | Veasel
// a := Animal(Veasel{})
// match a {
//     Dog { println('Bay') }
//     Cat { println('Meow') }
//     Veasel { println('Vrrrrr-eeee') } // see: https://www.youtube.com/watch?v=qTJEDyj2N0Q
// }




// (struct NumC ([n : Real]) #:transparent)
// (struct StrC ([s : String]) #:transparent)
// (struct IdC ([name : Symbol]) #:transparent)
// (struct IfC ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
// (struct LamC ([params : (Listof Symbol)] [body : ExprC]) #:transparent)
// (struct AppC ([fun : ExprC] [args : (Listof ExprC)]) #:transparent)

// (define-type ExprC (U NumC StrC IdC IfC LamC AppC))

// (define-type Value (U boolV numV closV primOpV stringV))
// (struct boolV ([b : Boolean]) #:transparent)
// (struct numV ([n : Real]) #:transparent)
// (struct closV ([args : (Listof idC)] [body : ExprC] [env : Env]) #:transparent)
// (struct primOpV ([op : Symbol]) #:transparent)
// (struct stringV ([s : String]))

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


 fn interp(expr ExprC, env []Binding) Value {
  	match expr {
  		NumC {
  			return num_v(expr.n)
  		}
  		StrC {
  			return string_v(expr.s)
  		}
  		IdC {
  			return lookup(expr.name, env)
  		}
  		LamC {
  			return clos_v(expr.params, expr.body, env)
  		}
  		IfC {
  			cond_val := interp(expr.test, env)
  			if cond_val is BoolV {
  				if cond_val.b {
  					return interp(expr.then, env)
  				}
  				return interp(expr.else_br, env)
  			}
  			panic('interp: condition not a boolean expression (VEBG)')
  		}
  		AppC {
  			// handle application here
  		}
  	}
  }

