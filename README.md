# Lox

"jlox" scripting language from [Crafting Interpreters](https://www.craftinginterpreters.com)

## Differences with jlox

- Integers and floats are separate types
- Floats support scientific notation ( `3.4e3` )
- Supports block comments ( `/* ... */` )
- Supports modulo operator ( `%` )
- Supports comma operator ( `expr1, expr2` )
- Supports conditional operator ( `cond ? then : else` )
- Handles division by zero
- Supports `break` statements
- Supports `continue` statements
- Supports anonymous functions ( `fun() {}` )
- Supports arithmentic assignment operators ( `+=`, `-=`, `*=`, `/=`, `%=` )
- Supports prefix and postfix increment and decrement operators ( `++`, `--` )
- Supports arrays ( `[1, "2", true]` ) and some methods in the `Array` class
- Classes support native methods
- Classes support static methods via Metaclasses ( `MyClass.method()` )
- Classes support getter methods ( `class MyClass { getter { return this.field } }` )
- Classes support calling methods as static methods ( `arr.push(1)` => `Array.push(arr, 1)` )

- Error messages show line and column
- Reports warnings when a local variable is never used
- The REPL supports printing the value of expressions
- The REPL prints values in different colors depending on their type
