# Tidy Programming Language

## Overview

Tidy is a statically typed, purely object-oriented programming language with comprehensive support for functional programming and immutability.

Its main goal is to make it easy to, at the same time:

- Write transparent functional code wherever possible.

- Preserve the ability to structure it on a high level in a clean, organized object-oriented fashion.

- Preserve the ability to store state, in an easy but highly controlled and visible way so as not to compromise that transparency.

That last point is the main difference between Tidy and other languages that stitch object-oriented and functional programming closely together, like Scala. Scala provides many wonderful features to help with this, but to do it well you must know what you're doing and how to organize your code properly, or you can just as easily end up with ugly imperative code with scattered state and side-effects all over. In Tidy it is much easier to structure the code, encourage good practices and see where mutability and side effects are, as one must declare them explicity. All of this is made easy and convenient through Tidy's multiple features visible right from the syntax to its extensive compile-time verification adjusted specificially with these goals in mind.


## Learn more

- To read more about Tidy, see the [quick-start guide](https://github.com/gerardd33/Tidy/blob/main/docs/Guide.md) (docs/Guide.md).

- To see the language's implementation (in Haskell), see the files in the [project](https://github.com/gerardd33/Tidy/tree/main/project) package. The heart of the implementation is in the *project/src/Interpreter* package.

- LBNF grammar of the language is available in the [project/src/Parser/Tidy.cf file](https://github.com/gerardd33/Tidy/blob/main/project/src/Parser/Tidy.cf).

- The [project/test/semantics](https://github.com/gerardd33/Tidy/tree/main/project/test/semantics) package contains examples of good and bad Tidy programs, serving as unit tests for the proper interpreter.

- Note that the interpreter is still in development, so while almost all of the features described in these documents already work properly, there are several that are not yet fully supported by the interpreter (notably lambdas, pattern matching, built-in methods for class List etc.). If you want reliable code to play with, go to [project/test/semantics](https://github.com/gerardd33/Tidy/tree/main/project/test/semantics) – all the examples there are fully functional.
 
- You can also play with the parser to get to know the syntax in [project/test/syntax](https://github.com/gerardd33/Tidy/tree/main/project/test/syntax). There you can see more examples of good and bad syntax, run a suite of automatic tests over them with ``run_all_tests.sh`` or input your own example into ``Test.ty`` and run ``single_test.sh``.

- To run the Tidy interpreter, clone this repository, go to the root directory of the project and run ``./build_tidy.sh``. Then you can simply type ``./tidy YourSourceFile.ty`` to build and execute any source file.

- If you just want a snippet of what Tidy looks like, here's one:

```
singleton class Main {

    actions: {
        main: () -> Void = System#printLine("Hello world!")
    }
}
```

And here's a bigger one:

```
mutable class Student {
    
    values: {
        id: Int;
        firstName: String;
        lastName: String;
        gender: Gender;
    }
    
    variables: {
        login: String;
        private passwordHash: String;
        classes: List[UniversityClass] = List[UniversityClass]();
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
            val hashedPassword: String = PasswordUtils.hash(newPassword);
            this#passwordHash(hashedPassword)
        }
    
        addClass: (newClass: UniversityClass) -> Void = {
            if (not this.classes.contains(newClass)) {
                this#classes(this.classes.add(newClass))
            }
        }
    }
}

singleton class MainClass {
    
    actions: {
        
        main: () -> Void = {
            val student: Student = Student(12, "John", "Kowalski", Male, 
                "jkowalski34", "dj3$%ex&@#jw_3d");
            System#printLine(student.fullName)
        }
    }
}
```
