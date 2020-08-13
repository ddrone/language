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

For example, let's say that you have an expression `foo * bar + baz` in your
program, and it does not compute to a desired result. Using your typical
`print` function you would have to write something like

```
print("foo = $foo")
print("bar = $bar")
print("baz = $baz")
```

in order to figure out why the value is not the one that you expect.

In this prototype, you would just stick a word `debug` in front of an
expression, like `debug foo * bar + baz`.

## Boring features

* Assignments
* If-statements
* Arithmetic and boolean expressions
* Local variables
* Recursive functions
* Lists
