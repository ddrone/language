# A (currently) unnamed programming language

This is a programming language prototype that I intend to use for
experimentation with some language features I have not seen anywhere else.

Because coming up with a catchy name is difficult and takes mental energy, I'm
going to postpone naming this thing for as long as possible.

## Interesting features

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
  x => 2
  1 => 1
  x - 1 => 1
  factorial(x - 1) => 1
  x * factorial(x - 1) => 2
6
  x => 3
  x => 3
  1 => 1
  x - 1 => 2
  factorial(x - 1) => 2
  x * factorial(x - 1) => 6
24
  x => 4
  x => 4
  1 => 1
  x - 1 => 3
  factorial(x - 1) => 6
  x * factorial(x - 1) => 24
120
  x => 5
  x => 5
  1 => 1
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
