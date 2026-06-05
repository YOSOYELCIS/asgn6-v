module main

fn main() {
    assert (op_to_meaning('+', [&NumV{n: 2}, &NumV{n: 3}] ) as NumV).n == 5
    assert (op_to_meaning('-', [&NumV{n: 5}, &NumV{n: 2}] ) as NumV).n == 3
    assert (op_to_meaning('*', [&NumV{n: 4}, &NumV{n: 6}] ) as NumV).n == 24
    assert (op_to_meaning('/', [&NumV{n: 10}, &NumV{n: 2}]) as NumV).n == 5
    assert (op_to_meaning('<=',    [&NumV{n: 3}, &NumV{n: 5}]) as BoolV).b == true
    assert (op_to_meaning('<=',    [&NumV{n: 5}, &NumV{n: 3}]) as BoolV).b == false
    assert (op_to_meaning('equal?', [&NumV{n: 4},              &NumV{n: 4}]             ) as BoolV).b == true
    assert (op_to_meaning('equal?', [&NumV{n: 4},              &NumV{n: 5}]             ) as BoolV).b == false
    assert (op_to_meaning('equal?', [&StringV{s: 'hello'},     &StringV{s: 'hello'}]    ) as BoolV).b == true
    assert (op_to_meaning('equal?', [&StringV{s: 'hello'},     &StringV{s: 'world'}]    ) as BoolV).b == false
    assert (op_to_meaning('equal?', [&BoolV{b: true},          &BoolV{b: true}]         ) as BoolV).b == true
    assert (op_to_meaning('equal?', [&BoolV{b: true},          &BoolV{b: false}]        ) as BoolV).b == false
    assert (op_to_meaning('strlen', [&StringV{s: 'hello'}]     ) as NumV).n == 5
}