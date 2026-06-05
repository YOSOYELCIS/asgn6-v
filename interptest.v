module main

fn test_interp_num_literal() {
	env := []Binding{}
	val := interp(num_c(7), env)

	assert val.kind == .num_v
	assert val.n == 7
}

fn test_interp_string_literal() {
	env := []Binding{}
	val := interp(string_c('hello'), env)

	assert val.kind == .string_v
	assert val.s == 'hello'
}

fn test_interp_identifier_from_env() {
	env := [Binding{
		name: 'x'
		val:  num_v(42)
	}]
	val := interp(id_c('x'), env)

	assert val.kind == .num_v
	assert val.n == 42
}

fn test_interp_if_expression() {
	env := make_top_env()
	expr := ifc(id_c('true'), num_c(1), num_c(2))
	val := interp(expr, env)

	assert val.kind == .num_v
	assert val.n == 1
}

fn test_interp_simple_lambda_application() {
	env := make_top_env()
	add_one := lam_c(['x'], app_c(id_c('+'), [id_c('x'), num_c(1)]))
	expr := app_c(add_one, [num_c(9)])
	val := interp(expr, env)

	assert val.kind == .num_v
	assert val.n == 10
}

fn test_interp_closure_captures_environment() {
	env := make_top_env() + [Binding{
		name: 'y'
		val:  num_v(3)
	}]
	use_outer := lam_c(['x'], app_c(id_c('+'), [id_c('x'), id_c('y')]))
	expr := app_c(use_outer, [num_c(4)])
	val := interp(expr, env)

	assert val.kind == .num_v
	assert val.n == 7
}
