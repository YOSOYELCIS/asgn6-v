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

