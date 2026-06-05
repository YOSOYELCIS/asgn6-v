module main

fn main() {
		// Test cases for op_to_meaning
	assert op_to_meaning('+', [&NumV{n: 2}, &NumV{n: 3}]).n == 5
	assert op_to_meaning('-', [&NumV{n: 5}, &NumV{n: 2}]).n == 3
	assert op_to_meaning('*', [&NumV{n: 4}, &NumV{n: 6}]).n == 24
	assert op_to_meaning('/', [&NumV{n: 10}, &NumV{n: 2}]).n == 5
	assert op_to_meaning('<=', [&NumV{n: 3}, &NumV{n: 5}]).b == true
	assert op_to_meaning('<=', [&NumV{n: 5}, &NumV{n: 3}]).b == false
	assert op_to_meaning('equal?', [&NumV{n: 4}, &NumV{n: 4}]).b == true
	assert op_to_meaning('equal?', [&NumV{n: 4}, &NumV{n: 5}]).b == false
	assert op_to_meaning('equal?', [&StringV{s: 'hello'}, &StringV{s: 'hello'}]).b == true
	assert op_to_meaning('equal?', [&StringV{s: 'hello'}, &StringV{s: 'world'}]).b == false
	assert op_to_meaning('equal?', [&BoolV{b: true}, &BoolV{b: true}]).b == true
	assert op_to_meaning('equal?', [&BoolV{b: true}, &BoolV{b: false}]).b == false
	assert op_to_meaning('strlen', [&StringV{s: 'hello'}]).n == 5

}
