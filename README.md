# Tidy Programming Language

## Overview

Tidy is a statically typed, purely object-oriented programming language with comprehensive support for functional programming and immutability.

Its main goal is to make it easy to, at the same time:

- Write transparent functional code wherever possible

- Preserve the ability to structure it on a high level in a clean, organised object-oriented fashion

- Preserve the ability to store state, in an easy but highly controlled and visible way so as not to compromise that transparency

That last point is the main difference between Tidy and other languages that stitch object-oriented and functional programming closely together, like Scala. Scala provides many wonderful features to help with this but to do it well you must know what you're doing, and you can just as easily end up with ugly imperative code with scattered state and side-effects all over. In Tidy it is much easier to structure the code, encourage good practices and see where mutability and side effects are, as one must declare them explicity.



## Learn more

- To read more about Tidy, see the [quick-start guide](https://github.com/gerardd33/Tidy/blob/main/docs/guide.md) (docs/guide.md).

- To see the language's implementation (in Haskell), see the files in the *parser* and *interpreter* packages.

- LBNF grammar of the language is available in the [parser/Tidy.cf file](https://github.com/gerardd33/Tidy/blob/main/parser/Tidy.cf). 

- You can play with the parser to get to know the syntax in [parser/parser_tests](https://github.com/gerardd33/Tidy/tree/main/parser/parser_tests). There you can run a suite of automatic tests with *run_all_tests.sh* or input your own example into *Test.ty* and run *build_and_run_single_test.sh*.

- **TODO Further instructions to run it etc.**

- If you just want a snippet of what Tidy looks like, here's one:

```
singleton class Main {

    actions: {
        helloWorld: () -> Void = System#printLine("Hello world!")
    }
}

```

And here's a bigger one:

```
mutable class Student {
    
    values: {
        id: Int,
        firstName: String,
        lastName: String,
        birthDate: Date,
        gender: Gender
    }
    
    variables: {
        login: String,
        private passwordHash: String,
        classes: List[UniversityClass] = List()
    }
    
    functions: {
        fullName: () -> String = this.firstName ++ " " ++ this.lastName
        
        authorize: (enteredPassword: String) -> Bool = {
            PasswordUtils.hash(enteredPassword) == this.passwordHash
        }
    }
    
    actions: {
        changePassword: (newPassword: String) -> Void = {
            Logger#log("Changing password for user " ++ this)
            value hashedPassword: String = PasswordUtils.hash(newPassword)
            this#passwordHash(hashedPassword)
        }
    
        addClass: (newClass: UniversityClass) -> Void = {
            if (not this.classes.contains(newClass)) {
                this#classes(this.classes.add(newClass))
            }
        }
    }
}
```
