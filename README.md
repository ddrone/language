# A (currently) unnamed programming language

This is a programming language prototype that I intend to use for
experimentation with some language features I have not seen anywhere else.

Because coming up with a catchy name is difficult and takes mental energy, I'm
going to postpone naming this thing for as long as possible.

## Interesting features

### Step-by-step execution

If a language is implemented as a virtual machine, and such a virtual machine
is implemented in a way that allows having more than one instance at a runtime
(meaning, no global variables are used), it should be possible to allow
programs to spawn child VMs and drive their execution.

In the following example:

* `spawn` is a kind of expression which evaluation will instantiate a child VM
  evaluating given expression and pass a handle to the program.
* `step` takes such a handle and makes an execution step, returning a boolean
  flag indicating whether the execution has finished.
* `extract` takes a VM handle and return the result value if VM has finished
  its execution, halting an interpreter otherwise.
* `stack` takes a VM handle and return a copy of its stack.

```
fun factorial(x: int): int {
    if (x <= 1) {
        return 1;
    }
    return x * factorial(x - 1);
}

fun stepwise(v: vm<int>): int {
    if (step(v)) {
        return extract(v);
    } else {
        debug stack(v);
        return stepwise(v);
    }
}

fun main(): int {
    val v = spawn factorial(3);
    debug stepwise(v);
    return 0;
}
```

Here is an example output (truncated):

```
[3]
  v => vm<addr=0>
  stack(v) => [3]
[3]
  v => vm<addr=0>
  stack(v) => [3]
[3, 3]
  v => vm<addr=0>
  stack(v) => [3, 3]
[3, 3, 1]
  v => vm<addr=0>
  stack(v) => [3, 3, 1]
[3, 0]
  v => vm<addr=0>
  stack(v) => [3, 0]
[3]
  v => vm<addr=0>
  stack(v) => [3]
[3, 3]
  v => vm<addr=0>
  stack(v) => [3, 3]
...
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
