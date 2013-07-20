This project is an interpreter for Fixlang, a language of my invention. This
README is an attempt at documentation.


QUICKSTART

You can run the interpreter with `runhaskell main.hs`, which will expect an FL
program from STDIN.


FIXLANG

Fixlang is a language which I no longer have the ability to describe. Thus, I
will simply provide a recipe for FL:

    - Start with Scheme
    - Remove all parentheses
    - Make all functions take FIXed numbers of arguments (non-variadic)
    - Add a dash of Haskell
    - Try to make a language out of the resulting mess

At this point in the recipe, you will be left with a relatively pretty language
that uses almost no punctuation. Indeed, the language may look a lot like
natural language, in that it is just a bunch of alphanumeric tokens separated
by whitespace. So now it's time to create a bunch of ugly macros with the
punctuation that isn't being used. Once you've done this, you may have the
disgusting, concise mess that is FL.


OKAY BUT HOW DO I DO STUFF IN FIXLANG

To begin, we'll ignore the last step of the fixlang recipe, and pretend fixlang
doesn't have a bunch of ugly hacks in it. Instead, fixlang is a beautiful
language, whose only punctuation is '.', '{', '}', and '$'. To begin, we'll examine a
fundamental part of most languages: variable assignment.

    let myvar "Hello World!" myvar          -> "Hello World!"

Note that the -> isn't actually part of the program. I'll be using it in this
tutorial to denote the value of an expression.  This "let" is similar to those
found in functional programming languages like LISP and Haskell. It takes 3
arguments, and evaluates the 3rd argument, after binding the value of the 2nd
argument to the first argument. Note that let itself is simply an expression,
and we can put as much whitespace as we want between token, allowing us to do
something like this:

    let oranges 4
    let apples 3

    let fruits + apples oranges

    fruits                                  -> 7

Go ahead and put this through the interpreter, and it will spit out "7". This
also introduces our first function: +. + takes two arguments, adds them
together, and returns the result. To apply any function in FL, we check how many
arguments the function needs, evaluate that many arguments right of the
function, and evaluate the function with the values we obtained. In this
example, the interpreter saw that + needs 2 arguments, so it read 2 more values
from STDIN (apples and oranges). It then added the two values together,
obtaining 7, and bound 7 to fruits.

When we "evaluate" a value from STDIN, we first check if it's a function. If it
isn't a funciton, we simply return it. In the above example, neither apples nor
oranges were functions, so we merely returned them. However, if the next value
is a function, we evaluate it, using the same rules as above.

    + * 2 3 / 8 - 3 1                   -> 10

Thus, this expression adds * 2 3 and / 8 - 3 1. * 2 3 evaluates to 6, and / 8 - 3 1 
evaluates to 4; the entire expression evaluates to 10.


There are four types of values in FL: Ints, Strings, Empty, and Pairs. We've
already seen and worked with Ints, and we saw a String briefly in our hello
world example. Empty seems like a weird datatype, but we use it for lists, as
we'll see shortly.

Pairs are a unique data type, and simply represent two FL values. You can have a
Pair of Ints, a Pair of String, a Pair of an Int and a String, a Pair of two
Emptys, even Pairs of Pairs. To construct a Pair, we use cons, a function that
takes two arguments, and returns a Pair of those arguments. All of the following
are valid Pairs:

    cons 1 4
    cons "Hello" "world!"
    cons 5 cons 3 empty

The last expression is a special kind of Pair; it is a list. Formally, we say a
list is either empty, or cons val lst, where val is any FL value, and lst is any
list. So, empty is a list, therefore cons 3 empty is a list is a list, therefore
cons 5 cons 3 empty is a list. cons empty empty, and cons cons 2 empty 3 empty,
are also examples of lists.

I sort of lied when I said that there are only four types of values in FL; there
are only four base types in FL. However, there is a special fifth type in FL,
called a Lambda. Lambdas are special in that they can be applied like functions.
Any given Lambda takes some fixed number of arguments, and produces a result.
Here, we create a simple function that adds 1 to its argument:

    let myFirstLambda {1 + .1 1}
    myFirstLambda 41                    -> 42

This introduces a number of new concepts, so let's break it into parts. First,
we're being introduced to curly braces ({}). Curly braces are used to denote
Lambdas; { starts a Lambda, and } closes a Lambda. When you have a { to define a
Lambda, it must be immediately followed with a non-negative integer. This number
specifies the number of arguments the Lambda takes.

In this case, the Lambda takes one argument. Then, we have what almost seems
like a regular addition expression, except for that weird . in front of the
first argument. When you have a . preceding an integer, it means that in fact is
not an integer, but an argument. .1 represent the first argument to the Lambda.
So, when the Lambda is later evaluated, .1 is replaced with 41, and then the
expression + 41 1 evaluates to 42.

    let sumSquares {2, + * .1 .1 * .2 .2}
    sumSquares 4 5                              -> 41

This Lambda takes two arguments, and adds their squares. We're getting to the
point where our code is a bit difficult to read, so let's look at how we can
properly format our code:

    let sumSquares {2,
        + * .1 .1
          * .2 .2
    }

    sumSquares 4 5                  -> 41

This code is much easier to read. The fact that whitespace is insignificant in
FL, that is, wherever you have a space, you can have as many tabs, spaces, and
newlines as you want, means you have a lot of freedom in formatting your code.
You should exercise this freedom often, as the lack of inherit structure in
written FL code makes it very difficult to read without good formatting.

Another function is the if function. if takes 3 arguments. If its first argument
is 0, "" (the empty string), or empty, then it returns its 3rd argument. If its
first argument is any other value, it returns its 2nd argument.

    if 1 1 0                        -> 1

    if "Hello World!" 42 123        -> 42

    if * 0 5 0 1                    -> 1

    if empty 1 87                   -> 87

    if cons empty empty "foo" "bar" -> foo

    if 0
        "that value was true"
    else
        "that value was false"      -> "that value was false"

The last example looks different, but we've simply formatted it differently, and
used a new function, "else". else is a trivial function, which takes one
argument and returns it. We could just have easily have ommitted it and our code
would have the same effect, but this helps to make our code more readable.

We refer to 0, "", and empty as "false" values, and all other values as "true"
values. There are 3 function for producing true or false values: eq (equal), gt
(greater than), and lt (less than). These functions produce boolean values,
suitable for input to if; specifically, they return 1 or 0. Integers are
compared numerically, Strings are compared alphabetically, and Pairs are
compared lexicographically; the left values are compared, and if they're equal,
the right values are used. Lambdas cannot be used with these functions. Note
that the two things you're comparing must be of the same type; you cannot
compare a String to an Int, for instance.

    gt 34 2                                 -> 1
    eq 34 "34"                              -> 0
    gt cons 1 cons 999 empty
       cons 2 empty                         -> 1

Now that we know if, we can make use of another property of Lambdas. In
addition to all the arguments passed directly, every Lambda also has a zeroth
argument, denoted ".0" (without quotes). This argument represents the Lambda
itself. This means the Lambda can call itself within itself. If this seems
confusing, consider this example:

    # this is a comment describing the function- it's the factorial function
    let factorial {1,
        if .1
            * .1
              .0 - .1 1
        else
            1
    }

    factorial 5                             -> 120

As a quick note, this introduces the comment; if you have the # character
anywhere outside of a string, then the rest of that line will be ignored.

Here, we have recursively defined the factorial function. If you step through
the code, you will find that the logic is "if n > 0, then factorial(n) =
n * factorial(n-1); otherwise, factorial(n) is 1". Recursion makes Lambdas in FL
very powerful.

There is one more thing to be learned in FL; explicit function application. When
you write a Lambda, it's evaluated as an expression, not directly as a function.
That's why, if you have `let f {1, + .1 1} 4', the program will return 4,
instead of passing 4 as an argument to the Lambda and binding the result to f.
However, sometimes you may want to apply your lambdas. You use the function
application operator, $, for this:

    $ {2, + .1 .2} 3 4                      -> 7

We also have an opposite to this function. Sometimes, you may want to pass a
function as an argument to another function, without evaluating it immediately.
In this case, you append a . to the function's name:

    # function takes a function and an argument, and applies the function to the
    # argument twice
    let double  {2, .1 .1 .2}
    let add1    {1, + .1 1}
    double add1. 4                          -> 6

Because we have a . after add1, add1 doesn't try to take 4 as its own argument;
instead, it is passed as an argument to double, which then applies it twice to
4, yielding 6.


UGLY STUFF

You now have all the tools needed to write FL code. The rest of this README will
cover additional language features that serve to make code more concise.


PAIRS/LISTS

We have various symbols for dealing with pairs and lists. , is an alias for
cons, < for car, > for cdr, and ; for empty. Thus, ,1,2,3,4; is a list of the
numbers from 1 to 4. <1,2,3,4; is 1, and >1,2,3,4; is 2,3,4;. This allows for a
much more concise definition of lists and pairs.


LAMBDAS

Instead of using {}s to define Lambdas, you can also use (). The difference is
that you don't need to tell () how many arguments you need; it will infer it by
finding the "highest" argument referenced in it, and call that the number of
arguments it needs. Thus, {2, + .1 .2} and (+ .1 .2) are equivalent expressions.
This is convenient, though it is slightly weaker; it is impossible to define the
Lambda {2, .1} with (), because () would create a Lambda of only one argument.
For brevity and readability, () should be used instead of {} wherever possible.

You can also use []s to define Lambdas. []s are like [], except the way you
refer to arguments and Ints gets reversed. That is, .3 now refers to the number
3, and 3 now refers to the third argument. This can be very convenient when
defining a function that makes heavy use of its arguments, and not-so-heavy use
of numbers. As an example, here is the definition of a classic higher-order
function:

    # right fold on list
    let foldr [
        if 3            1 <3
                          0 1. 2 >3
        else            2
    ]

    foldr +. 0 ,1,2,3,4;                    -> 10

Similarly, ? is an alias for if; you can say ?1 2 3, instead of if 1 2 3. With
all of these tricks, you can define foldr in 18 characters!

    # 18-character foldr
    [?3 1<3 0 1.2>3 2]

This should simultaneously show how powerful FL can be, and also how
horrifyingly ugly it can be.
