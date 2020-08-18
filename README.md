# A (currently) unnamed programming language

This is a programming language prototype that I intend to use for
experimentation with some language features I have not seen anywhere else.

## Interesting features

### Step-by-step execution

If a language is implemented as a virtual machine, and such a virtual machine
is implemented in a way that allows having more than one instance at a runtime
(meaning, no global variables are used), it should be possible to allow
programs to spawn child VMs and drive their execution. In order to instantiate
a child VM to evaluate, prefix it with `spawn` keyword, that will evaluate to a
VM handle. The following functions can be applied to such a handle:

* `step` makes an execution step, returning a boolean
  flag indicating whether the execution has finished.
* `extract` returns the result value if VM has finished
  its execution, halting an interpreter otherwise.
* `stack` returns a copy of argument's stack.

For example, we can use this feature to trace stack's contents the VM
evaluates an expression:

```
fun main(): int {
    stepwise(spawn 2 * (3 + 4));
    return 0;
}

fun stepwise(v: vm<int>): int {
    val stopped = step(v);
    debug stack(v);
    if (stopped) {
        return extract(v);
    } else {
        return stepwise(v);
    }
}
```

Output:

```
[2]
  v => vm<addr=0>
  stack(v) => [2]
[2, 3]
  v => vm<addr=0>
  stack(v) => [2, 3]
[2, 3, 4]
  v => vm<addr=0>
  stack(v) => [2, 3, 4]
[2, 7]
  v => vm<addr=0>
  stack(v) => [2, 7]
[14]
  v => vm<addr=0>
  stack(v) => [14]
```

### Debug expressions

Debug output is probably the most frequent tool used to understand a program's
behaviour. Debug expressions in this prototype is super-powered `print`
function, the superpower being that its evaluation will print out every single
intermediate result as well.

For example, consider you have a function like this:

```
fun factorial(x: int): int {
    if (x == 0 || x == 1) {
        return 1;
    }
    return debug x * factorial(x - 1);
}
```

The debug expression here would produce the following output:

```
2
  x => 2
  x - 1 => 1
  factorial(x - 1) => 1
  x * factorial(x - 1) => 2
6
  x => 3
  x - 1 => 2
  factorial(x - 1) => 2
  x * factorial(x - 1) => 6
24
  x => 4
  x - 1 => 3
  factorial(x - 1) => 6
  x * factorial(x - 1) => 24
120
  x => 5
  x - 1 => 4
  factorial(x - 1) => 24
  x * factorial(x - 1) => 120
```

## Boring features

* Assignments
* If-statements
* Arithmetic and boolean expressions
* Local variables
* Recursive functions
* Lists

## License

Apache 2