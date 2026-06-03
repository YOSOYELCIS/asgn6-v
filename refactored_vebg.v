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

struct Binding {name: string, val: Value}
type Env = []Binding

struct NumC {n: f64}
struct StrC {s: string}
struct IdC {name: string}
struct IfC {test: ExprC, then: ExprC, else: ExprC}
struct LamC {params: []string, body: ExprC}
struct AppC {fun: ExprC, args: []ExprC}
type ExprC = NumC | StrC | IdC | IfC | LamC | AppC

struct boolV {b: bool}
struct numV {n: f64}
struct closV {args: []string, body: ExprC, env: Env}
struct primOpV {op: string}
struct stringV {s: string}
type Value = boolV | numV | closV | primOpV | stringV


