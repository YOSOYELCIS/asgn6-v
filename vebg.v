module main

import os
import math
import rand

// ─── AST / ExprC ────────────────────────────────────────────────────────────

enum ExprKind {
	num_c
	id_c
	string_c
	app_c
	ifc
	lam_c
}

@[heap]
struct ExprC {
mut:
	kind    ExprKind
	// numC
	num     f64
	// idC / lamC body symbol
	sym     string
	// stringC
	str     string
	// appC  – fun + args
	fun     &ExprC = unsafe { nil }
	args    []&ExprC
	// lamC  – param names + body
	params  []string
	body    &ExprC = unsafe { nil }
	// if?C
	cond_e  &ExprC = unsafe { nil }
	then_e  &ExprC = unsafe { nil }
	else_e  &ExprC = unsafe { nil }
}

fn num_c(n f64) &ExprC {
	return &ExprC{ kind: .num_c, num: n }
}

fn id_c(s string) &ExprC {
	return &ExprC{ kind: .id_c, sym: s }
}

fn string_c(s string) &ExprC {
	return &ExprC{ kind: .string_c, str: s }
}

fn app_c(fun &ExprC, args []&ExprC) &ExprC {
	return &ExprC{ kind: .app_c, fun: fun, args: args }
}

fn ifc(cond &ExprC, then &ExprC, els &ExprC) &ExprC {
	return &ExprC{ kind: .ifc, cond_e: cond, then_e: then, else_e: els }
}

fn lam_c(params []string, body &ExprC) &ExprC {
	return &ExprC{ kind: .lam_c, params: params, body: body }
}

// ─── Values ──────────────────────────────────────────────────────────────────

enum ValueKind {
	bool_v
	num_v
	clos_v
	prim_op_v
	string_v
	void_v
}

@[heap]
struct Value {
mut:
	kind     ValueKind
	b        bool
	n        f64
	s        string
	op       string
	// closV
	params   []string
	body     &ExprC = unsafe { nil }
	env      []Binding
}

fn bool_v(b bool) &Value {
	return &Value{ kind: .bool_v, b: b }
}

fn num_v(n f64) &Value {
	return &Value{ kind: .num_v, n: n }
}

fn string_v(s string) &Value {
	return &Value{ kind: .string_v, s: s }
}

fn prim_op_v(op string) &Value {
	return &Value{ kind: .prim_op_v, op: op }
}

fn void_v() &Value {
	return &Value{ kind: .void_v }
}

fn clos_v(params []string, body &ExprC, env []Binding) &Value {
	return &Value{ kind: .clos_v, params: params, body: body, env: env }
}

// ─── Environment ─────────────────────────────────────────────────────────────

struct Binding {
	name string
	val  &Value
}

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

// ─── Reserved / banned symbols ───────────────────────────────────────────────

const dni_table = ['->','fn','if','given','=']

fn is_banned(s string) bool {
	return s in dni_table
}

// ─── S-expression token type ─────────────────────────────────────────────────

// We represent S-expressions as a simple tagged union.
enum SexpKind {
	atom   // number literal stored as string, or symbol/string token
	list
	num
	str_atom
}

struct Sexp {
mut:
	kind     SexpKind
	atom_val string   // symbol or raw string text
	num_val  f64
	is_str   bool     // true when atom came from a quoted string
	children []&Sexp
}

fn sexp_num(n f64) &Sexp {
	return &Sexp{ kind: .num, num_val: n }
}

fn sexp_sym(s string) &Sexp {
	return &Sexp{ kind: .atom, atom_val: s }
}

fn sexp_str(s string) &Sexp {
	return &Sexp{ kind: .atom, atom_val: s, is_str: true }
}

fn sexp_list(children []&Sexp) &Sexp {
	return &Sexp{ kind: .list, children: children }
}

// ─── Tokeniser ───────────────────────────────────────────────────────────────

fn tokenize(src string) []string {
	mut tokens := []string{}
	mut i := 0
	for i < src.len {
		c := src[i]
		if c == `(` || c == `)` {
			tokens << src[i..i+1]
			i++
		} else if c == `"` {
			// read quoted string
			mut j := i + 1
			for j < src.len && src[j] != `"` {
				if src[j] == `\\` { j++ }
				j++
			}
			tokens << src[i..j+1]
			i = j + 1
		} else if c == `;` {
			// line comment
			for i < src.len && src[i] != `\n` { i++ }
		} else if c == ` ` || c == `\t` || c == `\n` || c == `\r` {
			i++
		} else {
			mut j := i
			for j < src.len {
				d := src[j]
				if d == `(` || d == `)` || d == ` ` || d == `\t` || d == `\n` || d == `\r` { break }
				j++
			}
			tokens << src[i..j]
			i = j
		}
	}
	return tokens
}

// ─── S-expression parser ──────────────────────────────────────────────────────

struct TokenStream {
mut:
	tokens []string
	pos    int
}

fn (mut ts TokenStream) peek() string {
	if ts.pos >= ts.tokens.len { return '' }
	return ts.tokens[ts.pos]
}

fn (mut ts TokenStream) next() string {
	t := ts.peek()
	ts.pos++
	return t
}

fn (mut ts TokenStream) parse_sexp() &Sexp {
	tok := ts.peek()
	if tok == '(' {
		ts.next()
		mut children := []&Sexp{}
		for ts.peek() != ')' && ts.peek() != '' {
			children << ts.parse_sexp()
		}
		ts.next() // consume ')'
		return sexp_list(children)
	} else {
		ts.next()
		// quoted string?
		if tok.len >= 2 && tok[0] == `"` {
			inner := tok[1..tok.len-1]
			return sexp_str(inner)
		}
		// number?
		if n := f64_opt(tok) {
			return sexp_num(n)
		}
		return sexp_sym(tok)
	}
}

fn parse_source(src string) &Sexp {
	tokens := tokenize(src)
	mut ts := TokenStream{ tokens: tokens }
	s := ts.parse_sexp()
	return s
}

// f64_opt helper (V doesn't have it built-in in all versions)
fn f64_opt(s string) ?f64 {
	if s.len == 0 { return none }
	// allow leading minus
	start := if s[0] == `-` { 1 } else { 0 }
	mut has_digit := false
	mut has_dot   := false
	for i := start; i < s.len; i++ {
		c := s[i]
		if c >= `0` && c <= `9` { has_digit = true }
		else if c == `.` {
			if has_dot { return none }
			has_dot = true
		} else { return none }
	}
	if !has_digit { return none }
	return s.f64()
}

// ─── VEBG parse ──────────────────────────────────────────────────────────────

fn check_dupes(args []string) {
	mut seen := []string{}
	for a in args {
		if a in seen {
			panic('check_dupes: bad syntax: duplicate args ${args} (VEBG)')
		}
		seen << a
	}
}

fn parse_expr(s &Sexp) &ExprC {
	match s.kind {
		.num {
			return num_c(s.num_val)
		}
		.atom {
			if s.is_str {
				return string_c(s.atom_val)
			}
			// plain symbol
			if is_banned(s.atom_val) {
				panic('parse: invalid syntax: reserved symbol ${s.atom_val} (VEBG)')
			}
			return id_c(s.atom_val)
		}
		.list {
			ch := s.children
			if ch.len == 0 {
				panic('parse: invalid syntax: empty list (VEBG)')
			}
			head := ch[0]

			// (if cond then else)
			if head.kind == .atom && !head.is_str && head.atom_val == 'if' {
				if ch.len != 4 {
					panic('parse: if requires 3 sub-expressions (VEBG)')
				}
				return ifc(parse_expr(ch[1]), parse_expr(ch[2]), parse_expr(ch[3]))
			}

			// (given (bindings ...) do body)
			if head.kind == .atom && !head.is_str && head.atom_val == 'given' {
				if ch.len != 4 || ch[2].kind != .atom || ch[2].atom_val != 'do' {
					panic('parse: malformed given expression (VEBG)')
				}
				bindings_sexp := ch[1]
				if bindings_sexp.kind != .list {
					panic('parse: given bindings must be a list (VEBG)')
				}
				mut param_names := []string{}
				mut param_exprs := []&ExprC{}
				for b in bindings_sexp.children {
					if b.kind != .list || b.children.len != 3 {
						panic('parse: poorly formatted binding (VEBG)')
					}
					bname_sexp := b.children[0]
					eq_sexp    := b.children[1]
					bval_sexp  := b.children[2]
					if bname_sexp.kind != .atom || bname_sexp.is_str {
						panic('parse: binding name must be a symbol (VEBG)')
					}
					if is_banned(bname_sexp.atom_val) {
						panic('parse: reserved symbol in binding ${bname_sexp.atom_val} (VEBG)')
					}
					if eq_sexp.kind != .atom || eq_sexp.atom_val != '=' {
						panic('parse: expected = in binding (VEBG)')
					}
					param_names << bname_sexp.atom_val
					param_exprs << parse_expr(bval_sexp)
				}
				check_dupes(param_names)
				mut param_id_exprs := []&ExprC{}
				for p in param_names {
					param_id_exprs << id_c(p)
				}
				body_expr := parse_expr(ch[3])
				return app_c(lam_c(param_names, body_expr), param_exprs)
			}

			// (fn (args ...) -> body)
			if head.kind == .atom && !head.is_str && head.atom_val == 'fn' {
				if ch.len != 4 || ch[2].kind != .atom || ch[2].atom_val != '->' {
					panic('parse: malformed fn expression (VEBG)')
				}
				args_sexp := ch[1]
				if args_sexp.kind != .list {
					panic('parse: fn args must be a list (VEBG)')
				}
				mut param_names := []string{}
				for a in args_sexp.children {
					if a.kind != .atom || a.is_str {
						panic('parse: expected symbol in fn args (VEBG)')
					}
					param_names << a.atom_val
				}
				check_dupes(param_names)
				body_expr := parse_expr(ch[3])
				return lam_c(param_names, body_expr)
			}

			// (fun arg1 arg2 ...)  – application
			if head.kind == .atom && !head.is_str && is_banned(head.atom_val) {
				panic('parse: invalid syntax: reserved symbol as function ${head.atom_val} (VEBG)')
			}
			fun_expr := parse_expr(head)
			mut arg_exprs := []&ExprC{}
			for i := 1; i < ch.len; i++ {
				arg_exprs << parse_expr(ch[i])
			}
			return app_c(fun_expr, arg_exprs)
		}
		else {
			panic('parse: invalid syntax (VEBG)')
		}
	}
}

// ─── Serialize ───────────────────────────────────────────────────────────────

fn serialize(v &Value) string {
	match v.kind {
		.num_v    {
			n := v.n
			if n == math.floor(n) && !math.is_inf(n, 0) {
				return i64(n).str()
			}
			return n.str()
		}
		.bool_v   { return if v.b { 'true' } else { 'false' } }
		.clos_v   { return '#<procedure>' }
		.prim_op_v{ return '#<primop>' }
		.string_v { return '"${v.s}"' }
		.void_v   { return '' }
	}
}

// ─── Primitive operations ────────────────────────────────────────────────────

fn op_to_meaning(op string, args []&Value) &Value {
	match op {
		'+' {
			if args.len == 2 && args[0].kind == .num_v && args[1].kind == .num_v {
				return num_v(args[0].n + args[1].n)
			}
			panic('opToMeaning: invalid args for + (VEBG)')
		}
		'-' {
			if args.len == 2 && args[0].kind == .num_v && args[1].kind == .num_v {
				return num_v(args[0].n - args[1].n)
			}
			panic('opToMeaning: invalid args for - (VEBG)')
		}
		'*' {
			if args.len == 2 && args[0].kind == .num_v && args[1].kind == .num_v {
				return num_v(args[0].n * args[1].n)
			}
			panic('opToMeaning: invalid args for * (VEBG)')
		}
		'/' {
			if args.len == 2 && args[0].kind == .num_v && args[1].kind == .num_v {
				if args[1].n == 0 { panic('interp: divide by zero error (VEBG)') }
				return num_v(args[0].n / args[1].n)
			}
			panic('opToMeaning: invalid args for / (VEBG)')
		}
		'<=' {
			if args.len == 2 && args[0].kind == .num_v && args[1].kind == .num_v {
				return bool_v(args[0].n <= args[1].n)
			}
			panic('opToMeaning: invalid args for <= (VEBG)')
		}
		'equal?' {
			if args.len != 2 { panic('opToMeaning: invalid args for equal? (VEBG)') }
			a0 := args[0]
			a1 := args[1]
			if a0.kind == .num_v    && a1.kind == .num_v    { return bool_v(a0.n == a1.n) }
			if a0.kind == .string_v && a1.kind == .string_v { return bool_v(a0.s == a1.s) }
			if a0.kind == .bool_v   && a1.kind == .bool_v   { return bool_v(a0.b == a1.b) }
			return bool_v(false)
		}
		'substring' {
			if args.len == 3 && args[0].kind == .string_v
				&& args[1].kind == .num_v && args[2].kind == .num_v {
				s     := args[0].s
				start := int(args[1].n)
				stop  := int(args[2].n)
				if start < 0 || stop > s.len || start > stop {
					panic('opToMeaning: substring index out of range (VEBG)')
				}
				return string_v(s[start..stop])
			}
			panic('opToMeaning: invalid args for substring (VEBG)')
		}
		'strlen' {
			if args.len == 1 && args[0].kind == .string_v {
				return num_v(f64(args[0].s.len))
			}
			panic('opToMeaning: invalid args for strlen (VEBG)')
		}
		'println' {
			if args.len == 1 && args[0].kind == .string_v {
				println(args[0].s)
				return void_v()
			}
			panic('opToMeaning: invalid args for println (VEBG)')
		}
		'error' {
			if args.len == 1 {
				msg := if args[0].kind == .string_v { args[0].s } else { serialize(args[0]) }
				panic('user-error: ${msg}')
			}
			panic('opToMeaning: invalid args for error (VEBG)')
		}
		'read-num' {
			if args.len == 0 {
				print('>')
				line := os.input('')
				n := line.f64()
				return num_v(n)
			}
			panic('opToMeaning: invalid args for read-num (VEBG)')
		}
		'read-str' {
			if args.len == 0 {
				print('>')
				line := os.input('')
				return string_v(line)
			}
			panic('opToMeaning: invalid args for read-str (VEBG)')
		}
		'chain' {
			if args.len >= 1 {
				return chain_helper(args)
			}
			panic('opToMeaning: invalid args for chain (VEBG)')
		}
		'++' {
			return string_v(concat_helper(args))
		}
		'randomInt' {
			if args.len == 1 && args[0].kind == .num_v {
				mx := int(args[0].n)
				return num_v(f64(rand.int_in_range(0, mx) or { 0 }))
			}
			if args.len == 2 && args[0].kind == .num_v && args[1].kind == .num_v {
				mn := int(args[0].n)
				mx := int(args[1].n)
				return num_v(f64(rand.int_in_range(mn, mx) or { 0 }))
			}
			panic('opToMeaning: invalid args for randomInt (VEBG)')
		}
		'crying' {
			if args.len == 1 && args[0].kind == .bool_v {
				if args[0] == bool_v(true){
					println('🥹')
				}
				else{
					println('🥺')
				}
			return void_v()
			}
			else {
				panic('opToMeaning: invalid args for crying (VEBG)')
			}
		}
		else {
			panic('opToMeaning: invalid operation: ${op} (VEBG)')
		}
	}
}

fn chain_helper(args []&Value) &Value {
	if args.len == 0 { panic('chain_helper: chain has no arguments (VEBG)') }
	if args.len == 1 { return args[0] }
	return chain_helper(args[1..])
}

fn concat_helper(args []&Value) string {
	mut result := ''
	for a in args {
		match a.kind {
			.string_v { result += a.s }
			.num_v {
				n := a.n
				if n == math.floor(n) {
					result += i64(n).str()
				} else {
					result += n.str()
				}
			}
			else { panic('++: invalid argument type (VEBG)') }
		}
	}
	return result
}

// ─── Interpreter ─────────────────────────────────────────────────────────────

fn interp(expr &ExprC, env []Binding) &Value {
	match expr.kind {
		.num_c    { return num_v(expr.num) }
		.string_c { return string_v(expr.str) }
		.id_c     { return lookup(expr.sym, env) }
		.lam_c    { return clos_v(expr.params, expr.body, env) }
		.ifc {
			cond_val := interp(expr.cond_e, env)
			if cond_val.kind != .bool_v {
				panic('interp: condition not a boolean expression (VEBG)')
			}
			if cond_val.b {
				return interp(expr.then_e, env)
			} else {
				return interp(expr.else_e, env)
			}
		}
		.app_c {
			fd := interp(expr.fun, env)
			mut evaluated_args := []&Value{}
			for a in expr.args {
				evaluated_args << interp(a, env)
			}
			match fd.kind {
				.prim_op_v {
					return op_to_meaning(fd.op, evaluated_args)
				}
				.clos_v {
					if fd.params.len != evaluated_args.len {
						panic('interp: wrong number of arguments applied to function (VEBG)')
					}
					mut new_binds := []Binding{}
					for i, p in fd.params {
						new_binds << Binding{ name: p, val: evaluated_args[i] }
					}
					new_env := extend_env(new_binds, fd.env)
					return interp(fd.body, new_env)
				}
				else {
					panic('interp: poorly formatted function call (VEBG)')
				}
			}
		}
	}
}

// ─── Top-level environment ───────────────────────────────────────────────────

fn make_top_env() []Binding {
	return [
		Binding{ name: 'true',      val: bool_v(true) },
		Binding{ name: 'false',     val: bool_v(false) },
		Binding{ name: '+',         val: prim_op_v('+') },
		Binding{ name: '-',         val: prim_op_v('-') },
		Binding{ name: '*',         val: prim_op_v('*') },
		Binding{ name: '/',         val: prim_op_v('/') },
		Binding{ name: '<=',        val: prim_op_v('<=') },
		Binding{ name: 'equal?',    val: prim_op_v('equal?') },
		Binding{ name: 'substring', val: prim_op_v('substring') },
		Binding{ name: 'strlen',    val: prim_op_v('strlen') },
		Binding{ name: 'error',     val: prim_op_v('error') },
		Binding{ name: 'println',   val: prim_op_v('println') },
		Binding{ name: 'read-num',  val: prim_op_v('read-num') },
		Binding{ name: 'read-str',  val: prim_op_v('read-str') },
		Binding{ name: 'chain',     val: prim_op_v('chain') },
		Binding{ name: '++',        val: prim_op_v('++') },
		Binding{ name: 'randomInt', val: prim_op_v('randomInt') },
		Binding{ name: 'crying', val: prim_op_v('crying') },

	]
}

// ─── top-interp ──────────────────────────────────────────────────────────────

fn top_interp(src string) string {
	sexp := parse_source(src)
	expr := parse_expr(sexp)
	val  := interp(expr, make_top_env())
	return serialize(val)
}

// ─── Main / REPL ─────────────────────────────────────────────────────────────

fn main() {
	args := os.args
	if args.len > 1 {
		// File mode: vebg <file.vebg>
		src := os.read_file(args[1]) or {
			eprintln('Error reading file: ${args[1]}')
			exit(1)
		}
		result := top_interp(src)
		if result != '' { println(result) }
	} else {
		// REPL mode
		println('VEBG REPL  (Ctrl-D to quit)')
		for {
			print('vebg> ')
			line := os.input('') 
			if line == '' { break }
			result := top_interp(line)
			if result != '' { println(result) }
		}
	}
}