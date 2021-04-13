# Quick start guide

The best place to start talking about Tidy is at the top, with classes. A Tidy source file (*.ty*) is always composed of a list of one or more class definitions. Everything in Tidy is an object, i.e. an instance of a class, and everything inside a program lies somewhere inside some object. There are three types of classes: *immutable*, *mutable* and *singleton* classes. Let's go over each one of them.


## Mutable classes

We start with mutable classes because they're the most general ones. The basic template for a mutable looks like this:

```
mutable class ClassName {
    
    values:
    
    variables:
    
    functions:
    
    actions:
}
```

As you can see, there are four possible types of class members:

- Values: constant, immutable attributes. They can't include mutable types.

- Variables: variable, mutable attributes.

- Functions: methods without side effects (so *actions* cannot be invoked inside them). Their syntax is very similar to standard Haskell functions. They're technically not purely functional because they can use current values of other mutable members, but if we call a function several times with no *actions* (so no side effects) between the invocations, they're guaranteed to yield the same result.

- Actions: methods that can have side effects.

The order of the sections is significant but you can skip the sections you don't need.

Mutable classes are a tool to store state and work with side effects without a need for, for example, monads, but you should use them carefully. The idea is to build the core of your program using clear, transparent flows with immutable classes and functions and use mutable classes only on the edges of your flows (e.g. as places for storage, interactions or entry points), reasonably decoupled from that core and from other mutable components. When in doubt, make your class immutable, you can easily change it to mutable later if you decide this is the place where you need mutable state or side effects.


## Immutable classes

This is the template for an immutable class:

```
immutable class ClassName {
    
    values:
    
    functions:
}
```

It's the same one as for a mutable class but without *variables* and *actions*. Immutable class is essentially Tidy's equivalent of Scala *case classes* or Java 15 *records*. As mentioned above, you should prefer this kind of objects in your code.

One more important use case of immutable classes is in pattern matching. This, by the way, makes it possible to do without constructs for *enums*:

```
abstract immutable class Gender
immutable class Male extends Gender
immutable class Female extends Gender

greetProperly: (gender: Gender) -> String = {
    match gender {
        Male -> "Hello Sir!"
        Female -> "Hello Madam!"
    }
}
```


## Singleton classes

This is the template for a singleton class:

```
singleton class ClassName {
    
    values:
    
    functions:
    
    actions:
}
```

It's the same as for an immutable class but it can also have actions. The other major difference between the two is that singleton classes cannot be instantiated and you access their members using the name of the class, not the name of the instance. They're the equivalent of static classes in other languages or of *singleton objects* in Scala. They can be used as utility classes or in a way similar to Scala *companion objects*, so it's generally a good place to put all of your static methods.


## Abstract classes

Each of the above class types can be marked as *abstract*. This is the equivalent of Java *interfaces* or Scala *traits* (not of Java *abstract classes*). Abstract classes cannot be instantiated. The rationale for having abstract singleton classes is just to use them as templates, as their implementations cannot be instantiated anyway.


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
        classes: List[UniversityClass] = List();
    }
}
```

There are several new things here:

- Syntax of type declaration.

- Private members.

- Fields initialised with a value (``List()``).


Note that in Tidy you don't have to write a lot of semicolons, like e.g. in Java. However, there is one important place where this is necessary, namely after each value/variable definition. They're also used (to avoid syntax conflicts) in very few situations when you want to write one-liners without curly brackets: in expressions like *if-then-else* or *lambdas*, for example:

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

This might be a corner case but remember that this doesn't mean you can skip the values definition semicolon. In this case you must use two (one for skipping lambda curly brackets and one for ending value declaration:

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


It's important to mention that in Tidy you don't need to write the classic OOP boiler-plate code. It will automatically generate for you:

- A constructor, that takes as parameters all of the fields that you don't initialise explicitly. There is no *null* reference in Tidy, so the object must be complete with all the fields initialised from the moment it's born.

- Public getters for all public (not marked as *private*) attributes (values and variables).

- Private getters for all private attributes.

- Public setters for all public variables.

- Private setters for all private variables.

- A ``toString`` method that can be used for printing the object.


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

The syntax here is very similar to functions in Haskell, that is, their body must be a single expression. It can also use locally declared helper values, like with Haskell's *where*, for example:

```
someValueForADummyStudent: () -> Student = {
    ValueGenerator.value(student)
} with values: {
    student = Student("Some student", 23);
}
```

Also, keep in mind that the *this* keyword is required, not optional as in e.g. Java. Code constistency, clarity, avoiding shadowing etc. are big enough reasons for this, but there are even better ones – we'll talk about them later.


## Actions

Actions behave more or less like normal Scala methods in Scala. They can be composed of several expressions. Each expression is evaluated separately and the evaluation result of the last expression is the value returned by the method. The expression can for example be an arithmetic expression, an *if*, *foreach* etc., an action call or a local value definition. Let's add some actions to our example:

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
    
    actions: {
        changePassword: (newPassword: String) -> Void = {
            Logger#log("Changing password for user " ++ this)
            value hashedPassword: String = PasswordUtils.hash(newPassword);
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

Both of these actions return *Void* but you could of course do something like this:

```

addClassAndCountTotal: (newClass: UniversityClass) -> Int = {
    if (not this.classes.contains(newClass)) {
        this#classes(this.classes.add(newClass))
    }
    value totalClasses: Int = this.classes.size;
    totalClasses
}
```

## Method invocation: *get* and *do* expressions

One thing to elaborate on here are those *.* and *#* expressions. They're very important. The dot is read as *get* and the hash is read as *do*. This is a feature very specific to Tidy. As mentioned before, one of the most important ideas behind Tidy is making it clear when and where there is mutable state or side effects. The clear division into values/variables and functions/actions is one step in this direction. Another one is making it visible whether you call a *function* or an *action*. Some examples (notice too that methods with no arguments can be invoked without parentheses):

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

There are several very neat things about this system. You can use shorter method names as there is no need to call your method *getFullName*. You can have your code just as readable because it's clear that *.* is a more-or-less pure function, just returning some value (without changing anything at all) and *#* is *an action*, doing/changing something.

This is especially significant when we notice that we can use this to define getters and setters. Automatically generated ones work like this (assume we have an object *student* of type *Student* with a variable field *age*):

- Getter: ``value age: Int = student.age // read "student get age"``
- Setter: ``student#age(23) // assigns 23 to student.age, read "student do age 23"``

One of the greatest benefits of this system is that it allows us to have beautifully consistent syntax without any assignment operator in our language whatsoever! That's because every assignment can happen as a call to a setter method of some field on some object (all parameters and local variables are constant, like in Haskell, *this* is obligatory etc., so everything comes together perfectly). This also means that parameters and local values can't shadow attributes as well as that we can call functions with no arguments without parentheses and not worry about conflicts.

It gives us very good encapsulation and uniformity in syntax. We avoid having multiple things like ``student.age``,``student.getAge()`` or ``age = 3``, ``this.age = 3`` and ``student.setAge(3)`` doing exactly the same things and being used interchangeably and inconsistently. Getting a value is always ``student.age``, changing a value is always ``student#age(23)``, no matter where you are in the code. If the attribute/getter/setter is private, you may just not have access to it from outside the class, but everywhere you use the same clean and convenient syntax.


## Other features

Let's look at one last piece of code:

```
mutable class PhoneMessageReceiver extends MessageReceiver {
    
    values: {
        messageReceived: String = "You got a new message!";
        messageInvalid: String = "Invalid message";
        maxMessageLength: Int = 100;
        emptyString: String = "";
        
        notificationChannels: List[NotificationChannel];
    }
    
    variables: {
        messages: List[Message] = List();
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
            
            value response: String = this.parseMessage(message);
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
            value handleMessage: do (message: Message) -> Void = do (message: Message) -> {
                System#printLine("Transforming message " ++ message)
                message#transform(local#messageTransformer(False))
            };
            
            for (message: Message; in this.messages) {
                local#handleMessage(message)
            }
        }
    }
}

```

We can see a lot of more new features here:

- Inheritance, method overriding and pattern matching syntax. *Elif* syntax.

- There is a *super* keyword equivalent to Java's *super* keyword.

- Logical operators: unary *not*, binary *and*, *or* along with literals *True* and *False*.

- *Foreach* loop. There is also a *while* loop with standard syntax: ``while (condition) { }``. Note that these loops are imperative constructs and can only be used inside actions, not inside functions. Also don't forget the semicolon. It's necessary because of the value declaration.

- *If* statements. Note the very important distinction between the imperative *if* (it can have an optional *else* branch) and the functional *if-then-else* expression. The former is like an *if* in C and returns a *Void*, the latter is like Java ternary operator and returns the value from either the *then* or the *else* branch.

- Higher-order functions and lambdas. The syntax for function types and lambda expressions is the same. Note that because of Tidy's strict dichotomy between *functions* and *actions*, you always need to explicitly declare whether or not your function can have side effects. As in the paragraph on *get* and *do* expressions, *get* is for *functions* and *do* is for *actions*. If you declare your function parameter or lambda with the *get* keyword, it will be run like this: ``local.fun(x)``, if with *do* – like this: ``local#fun(x)``.

- Note that the syntax doesn't even allow calls like ``fun(x)``. There are two reasons for this and we've already talked about both of them: first, each function invocation must be an invocation of some method on some object, and second, you need a way to specify whether this is a function or an action by prefixing it with a dot or a hash. If you have a function as your attribute this is not a problem – you just call ``objectName.fun(x)``. But what if you have a function ``fun`` as your parameter or stored in a local value? In this case, you should use the special keyword ``local`` which is analogous to ``this`` – it references the scope of the method (that is, values local to the method as well as the method parameters).

- *Pass*: literal of type *Void* (*NOP*). Can be used when you need to explicitly return something in an action or expression that expects to return *Void* or as a placeholder as with *pass* in Python.


## Style

- Tidy is pretty serious about style. It doesn't force you to keep most of the conventions through syntax errors but it doesn't guarantee sensible interpretation of weird syntactic corner cases. The style used in the snippets above is encouraged. It closely resembles Java/Scala style conventions, so when in doubt, use those. Treat whitespaces with respect. Use curly brackets in multi-line expressions. And don't put multiple separate expressions (e.g. in action bodies) on one line. 

- In imperative expressions (*while*, *foreach*, imperative *if*) curly brackets are obligatory. In general, curly braces should be placed at the end of the same line as the preceding expression (Java-style).

- Case conventions are very important. Some of them are syntactically significant. Vast majority of them is the same as in Java. All class names must be in *UpperCamelCase*. Method, parameter and variable names must be in *lowerCamelCase*. Because automatically generated getters by default have the same name as the field (and they must start with a lowercase letter), fields with constants also need to be in *lowerCamelCase*, unlike in Java or Scala.

- Naming conventions are similar to those in Scala, that is, try to use reasonably descriptive Java-style names in most places, but in places where the code is largely functional, it's okay to use less verbose, even one-letter identifiers, for example: ``add: (x: Int, y: Int) -> Int = x + y``.

