# Quick start guide

The best place to start talking about Tidy is at the top, with classes. A Tidy source file (*.ty*) is always composed of a list of one or more class definitions. Everything in Tidy is an object, i.e. an instance of a class, and everything inside a program lies somewhere inside some object. There are three types of classes: *immutable*, *mutable* and *singleton* classes. Let's go over each one of them.


## Mutable classes

We start with mutable classes because they're the most general ones. The basic template for a mutable looks like this:

```
mutable class ClassName {
    
    values: {
    }
    
    variables: {
    }
    
    functions: {
    }
    
    actions: {
    }
}
```

As you can see, there are four possible types of class members:

- Values: constant, immutable attributes.

- Variables: variable, mutable attributes.

- Functions: methods without side effects (so *actions* cannot be invoked inside them). Their syntax is very similar to standard Haskell functions. They're technically not purely functional because they can use current values of other mutable members, but if we call a function several times with no *actions* (so no side effects and the state unchanged) between the invocations, they're guaranteed to yield the same result and not change the state.

- Actions: methods that can have side effects.

The order of the sections is important but you can skip the sections you don't need.

Mutable classes are a tool to store state and work with side effects without a need for things like monads, but you should use them carefully. A good pattern is to build the core of your program, and in general all the components that don't need state and side effets, using clear, transparent flows with immutable classes and functions, using mutable classes on the edges of your flows (e.g. as places for storage, external interactions or entry points) or in other well-selected isolated places, reasonably decoupled from other components. When in doubt, make your class immutable, you can easily change it to mutable later if you decide this is a place where you need mutable state or side effects.


## Immutable classes

This is the template for an immutable class:

```
immutable class ClassName {
    
    values: {
    }
    
    functions: {
    }
}
```

It's the same one as for a mutable class but without *variables* and *actions*. Immutable class is essentially Tidy's equivalent of Scala *case classes* or Java 15 *records*. As mentioned above, you should prefer this kind of objects in your code.


## Singleton classes

This is the template for a singleton class:

```
singleton class ClassName {
    
    values: {
    }
    
    functions: {
    }
    
    actions: {
    }
}
```

It's the same as for an immutable class but it can also have actions. This means they don't have their own internal state but can produce side effects in other places. Singleton classes cannot be instantiated by the user and they have a singleton instance that can be accessed using the name of the class directly. They're similar to static classes in other languages or to *singleton objects* in Scala. They can be used as utility classes or in a way similar to Scala *companion objects*, so it's generally a good place to put all the methods you would mark as *static* in other languages. 

The entrypoint of a program must also be a singleton class. When you execute a *.ty* file, the first declared action called *main* in some singleton class will be executed. You can, however, have multiple classes of different types in one file. This is also a good place to mention that Tidy has a strong static type and error checking system, meaning you will get information about the vast majority of the possible errors during compilation rather than during runtime (for example referenced class/object not in scope, method/constructor arguments not matching the signature, duplicate declarations, illegal side effects in purely functional expressions or functions, bad return type, non-existing method and much more).


## Abstract classes

Each of the above class types can be marked as *abstract*. This is the equivalent of Java *interfaces* or Scala *traits* (not of Java *abstract classes*). Abstract classes cannot be instantiated. The rationale for having abstract singleton classes is to use them as templates for other more specific singleton classes.


## Attributes

Let's build a sample mutable class. We'll start by adding some fields.

```
mutable class Student {
    
    values: {
        id: Int;
        firstName: String;
        lastName: String;
        birthDate: Date;
        gender: Gender;
    }
    
    variables: {
        login: String;
        private passwordHash: String;
        classes: List[UniversityClass] = List[UniversityClass]();
    }
}
```

There are several new things here:

- Syntax of type declaration.

- Private members. The ones without a modifier are public.

- Attributes initialized with a value (``List[UniversityClass]()``).


Note that in Tidy you don't have to write a lot of semicolons, unlike for example in Java. However, there is one important place where this is necessary, namely after each value/variable declaration. They're also used (to avoid syntax conflicts) in very few situations when you want to write one-liners without curly brackets: in expressions like *if-then-else* or *lambdas*, for example:

```
if (predicate) then {
    value1
} else {
    value2
}
```

can be written as:

```
if (predicate) then value1; else value2;

```

This might be a bit of a corner case but remember that this doesn't mean you can skip the values definition semicolon. If you declare a value storing a lambda, you must use two (one for skipping lambda curly brackets and one for finishing the value declaration):

```

// BAD
values: {
    function1: get () -> Int = get () -> 3;
}

// BOTH GOOD
values: {
    function1: get () -> Int = get () -> {
        3
    };
    
    function2: get () -> Int = get () -> 3;;
}


```


It's important to mention that in Tidy you don't need to write most of the classic OOP boiler-plate code. The language will automatically generate for you:

- A constructor, that takes as parameters all of the fields that you don't initialize explicitly, in the order of their declaration (starting from the top superclass if any is present). There is no *null* reference in Tidy, so the object must be complete with all the fields initialized from the moment it's born.

- Public getters for all public (not marked as *private*) attributes (values and variables).

- Private getters for all private attributes.

- Public setters for all public variables.

- Private setters for all private variables.

- A ``toString`` method that can be used for printing the object.

- An equality operator that performs deep recursive comparison of the values of attributes (and returns false for all pairs of objects of different type).


## Functions

Now let's see what Tidy's functions look like:

```
mutable class Student {
    
    values: {
        id: Int;
        firstName: String;
        lastName: String;
        birthDate: Date;
        gender: Gender;
    }
    
    variables: {
        login: String;
        private passwordHash: String;
        classes: List[UniversityClass] = List();
    }
    
    functions: {
        fullName: () -> String = this.firstName ++ " " ++ this.lastName
        
        authorize: (enteredPassword: String) -> Bool = {
            PasswordUtils.hash(enteredPassword) == this.passwordHash
        }
    }
}
```

The syntax here is very similar to functions in Haskell, that is, the body must be a single expression. You can also use locally declared helper values, like with Haskell's *where*, for example:

```
someValueForADummyStudent: () -> Student = {
    ValueGenerator.value(student)
} with values: {
    student = Student("Some student", 23);
}
```

Also, keep in mind that the *this* keyword is required, not optional as in e.g. Java. Code constistency, clarity, avoiding shadowing etc. are big enough reasons for this, but there are even bigger ones – we'll talk about them later.


## Actions

Let's first talk for a moment about expressions. All of the Tidy's constructs that appear inside methods are expressions, e.g.: arithmetic expressions, *ifs*, *foreach*, an action call, a local value definition etc. That means they all return some value (similarly to Scala) and there are no purely imperative statements. There exists, however, a division into purely functional and non-pure expressions. The latter cannot be used inside functions, inside purely functional expressions and in a few other places that expect pure expressions (to avoid confusing and unpredictible code).

Actions can be composed of a list of multiple expressions. They behave very much like normal Scala methods, that is, each expression is evaluated separately and the evaluation result of the last expression is the value returned by the method. Let's add some actions to our example:

```
mutable class Student {
    
    values: {
        id: Int;
        firstName: String;
        lastName: String;
        birthDate: Date;
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
```

Both of these actions return *Void* but you could of course return something more meaningful like this:

```

addClassAndCountTotal: (newClass: UniversityClass) -> Int = {
    if (not this.classes.contains(newClass)) {
        this#classes(this.classes.add(newClass))
    }
    val totalClasses: Int = this.classes.size;
    totalClasses
}
```

By the way, *Void* is a perfectly normal type, unlike in Java or C++ (it is equivalent to Scala's *Unit*). You can for example create values of type *Void*, just like of type *Int*. It has, however, only one possible value: *Pass*.


## Method invocation: *get* and *do* expressions

One thing to elaborate on here are those *.* and *#* expressions. They're extremely important. The dot is read as *get* and the hash is read as *do*. This is a feature very characteristic of Tidy. As mentioned before, one of the most important ideas behind Tidy is making it clear when and where mutable state or side effects exist. The clear division into values/variables and functions/actions is one step in this direction. Another one is making it explicit whether you're calling a *function* or an *action*. Some examples:

Functions:

```
student.fullName // read "student get fullName"

book.chapters // read "book get chapters"

Greetings.greet("Peter") // read "Greetings get greet Peter"

list.count(5) // returns the number of occurences of *5*, read "list get count 5"

```

Actions:

```
student#addClass(newClass) // read "student do addClass newClass"

student#changePassword(newPassword) // read "student do changePassword newPassword"

System#printLine("Hi!") // read "System do printLine Hi!"

user#login // read "user do login"

mail#send(message) // read "mail do send message"

```

There are several very neat things about this system. First, notice that when you invoke a method with no arguments, you can skip the parentheses. All of this is especially significant when we notice that we can use this mechanism to define getters and setters. Automatically generated ones work like this (assume we have an object *student* of type *Student* with a variable field *age*):

- Getter: ``student.age; // returns the value of field age, read "student get age"``
- Setter: ``student#age(23) // assigns 23 to student.age, read "student do age 23"``

So you can achieve the same effect by writing ``student.grades.summary#finalMark(5)`` instead of ``student.getGrades().getSummary().setFinalMark(5)``. You can skip such boiler-plate code and have your code much more readable because it's clear that *.* calls a pure *function*, just returning some value (without changing anything) and *#* calls *an action* that does/changes something.

One of the greatest benefits of this system is that it allows us to have beautifully consistent syntax without any assignment operator in our language whatsoever! That's because every assignment (outside of the initial attribute declaration) can happen as a call to a setter method of some field on some object. Even the locally declared objects are attributes of a special built-in object called *local*, so we can do things like:

```
val x: Int = 5;
var y: Int = 6;
x // returns 5, alias for local.x, both are correct

local#y(8)
y // returns 8

```

It gives us very good encapsulation and uniformity in syntax. We avoid having multiple things like ``student.age``,``student.getAge()`` or ``age = 3``, ``this.age = 3`` and ``student.setAge(3)`` doing essentially the same thing and being used interchangeably and inconsistently. Getting a value is always ``student.age``, changing a value is always ``student#age(23)``, no matter where you are in the code. If the attribute/getter/setter is private, you may just not have access to it from outside the class, but everywhere you use the same clean and convenient syntax that calls exactly the same methods.


## Other features

Let's look at one last piece of code:

```
mutable class PhoneMessageReceiver extends MessageReceiver {
    
    values: {
        messageReceived: String = "You got a new message!";
        messageInvalid: String = "Invalid message";
        maxMessageLength: Int;
        emptyString: String = "";
        
        notificationChannels: List[NotificationChannel];
    }
    
    variables: {
        messages: List[Message] = List[Message]();
    }
    
    functions: {
        private parseMessage: (message: Message) -> String = {
            if (this.validateMessage(message)) then {
                match message {
                    case Email -> "One new email"
                    case Sms -> "One new SMS"
                    case Notification -> "One new notification"
                }
            } elif (message.privileged) then {
                this.emptyString
            } else {
                this.messageInvalid
            }
        }
        
        private validateMessage: (message: Message) -> Bool = {
            not message.empty and message.length <= this.maxMessageLength
        }
    }
    
    actions: {
        override receive: (message: Message) -> Bool = {    
            System#printLine(this.messageReceived)
            
            val response: String = this.parseMessage(message);
            System#printLine(response)
            
            if (response != this.messageInvalid) {
                super#receive(message)
                this#messages(this.messages.add(message))
                
                for (channel: NotificationChannel; in this.notificationChannels) {
                    channel#notify(response)
                }
                
                True
            } else {
                False
            }
        }
        
        // a bit more artificial example to show how some more advanced features work
        override transformMessages: (messageTransformer: do (complete: Bool) -> Void) -> Void = {
            val handleMessage: do (message: Message) -> Void = do (message: Message) -> {
                System#printLine("Transforming message " ++ message)
                message#transform(messageTransformer#call(False))
            };
            
            for (message: Message; in this.messages) {
                handleMessage#call(message)
            }
        }
    }
}

singleton class MainClass {
    
    actions: {
        
        main: () -> Void = {
            val channels: List[NotificationChannel] = List[NotificationChannel](NotificationChannel("link1"), 
                NotificationChannel("link2"));
            val receiver: PhoneMessageReceiver = PhoneMessageReceiver(100, channels);
            
            if (receiver#receive(Message("Hi! What's up?"))) {
                System#printLine("Success")
            } else {
                System#printLine("Failure")
            }
        }
    }
}

```

We can see a lot of more new features here:

- Inheritance, method overriding and pattern matching syntax. *Elif* syntax.

- There is a *super* keyword equivalent to Java's *super* keyword.

- Logical operators: unary *not*, binary *and*, *or* along with literals *True* and *False*.

- *Foreach* loop. There is also a *while* loop with standard syntax: ``while (condition) { }``. Note that these loops are imperative non-pure constructs and can only be used inside actions, not inside functions (but they are still expressions, returning *Void*). Also don't forget the semicolon. It is necessary because of the value declaration.

- *If* expressions. Note the important distinction between the imperative *if* and the functional *if-then-else* expression. Both return the value of the last expression in the evaluated branch (like in Scala), however, a branch in the latter can only consist of a single purely functional expression (just like a function body).

- Higher-order functions and lambdas. The syntax for function types and lambda expressions is the same. Note that because of Tidy's strict dichotomy between *functions* and *actions*, you always need to explicitly declare whether or not your method can have side effects. As in the paragraph on *get* and *do* expressions, *get* is for *functions* and *do* is for *actions*. If you declare your method parameter or local-value lambda with the *get* keyword, it will be run like this: ``fun.call(x)``, if with *do* – like this: ``fun#call(x)``. This is also true for lambdas stored as fields because they are objects like any other and ``object.attributeLambda`` and ``object#atttributeLambda(...)`` are just getters and setters, hence the need for the special *call* method that an object of each functional type has.

- *Pass*: literal of type *Void* (*NOP*). Can be used when you need to explicitly return something in an action or expression that expects to return *Void* or as a placeholder as with *pass* in Python (expression lists in actions or *ifs* must contain at least one expression).


## Style

- Tidy is quite serious about coding style. The style used in the snippets above is encouraged. It closely resembles Java/Scala style conventions, so when in doubt, use those. You can of course adapt some details to your own taste but stay reasonable and consistent, treat whitespaces with respect, use curly brackets in multi-line expressions, don't put multiple separate expressions (e.g. in action bodies) on one line etc.

- In imperative expressions (*while*, *foreach*, imperative *if*) curly brackets are obligatory. In general, curly braces should be placed at the end of the same line as the preceding expression (Java-style).

- Case conventions are very important. Most of them are syntactically enforced. Most of them are the same as in Java. All class names must be in *UpperCamelCase*. Method, parameter and local value/variable names must be in *lowerCamelCase*. Every name can also start with an underscore *_*. Because automatically generated getters by default have the same name as the field (and getters as methods must start with a lowercase letter), all fields with constants also need to be in *lowerCamelCase*, unlike in Java or Scala.

- Naming conventions are similar to those in Scala, that is, try to use reasonably descriptive Java-style names in most places, but in places where the code is largely functional, it's okay to use less verbose, even one-letter identifiers, for example: ``add: (x: Int, y: Int) -> Int = x + y``.

