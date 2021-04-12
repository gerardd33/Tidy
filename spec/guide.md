# Quick start guide

The best place to start talking about Tidy is at the top, with classes. Everything in Tidy is an object, i.e. an instance of a class, and everything lies somewhere inside some object. There are three types of classes: *immutable*, *mutable* and *singleton* classes. Let's go over each one of them.

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

Let's build a sample mutable class. We'll start by adding some fields. First, note that for member lists, as well as for all other constructs (for example an *if* or a method body), there are two possible syntax variants:

If you manage to squeeze it into one line:

```
values: firstName: String, lastName: String
```

If you don't:

```
values: {
    id: Int,
    firstName: String,
    lastName: String,
    age: Int
}
```

So here's our class: 

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
}
```

There are several new things here:

- Syntax of type declaration

- Private members

- Initialisation of a field with a value (``List()``)

It's important to mention that you don't need to write the classic OOP boiler-plate code. Tidy will automatically generate for you:

- A constructor, that takes as arguments all of the fields that you don't initialise explicitly. There is no *null* reference in Tidy, so the object must be complete with all the fields initialised from the moment it's born.

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
}
```

The syntax here is very similar to functions in Haskell, that is, their body must be a single expression (it can be also an *if* or a *match* expression (we'll see more examples later). It can also declare local values, like with Haskell's *where*, for example:

```
someValueForDummyStudent: () -> Student = {
    ValueGenerator.value(student)
} with values: student = Student("Some student", 23)
```

or of course:

```
someValueForDummyStudent: () -> Student = {
    ValueGenerator.value(student)
} with values: {
    studentName: String = NameGenerator.name,
    student: Student = Student(studentName, 23)
}
```

Also, keep in mind that the *this* keyword is required, not optional as in e.g. Java. Code constistency, clarity, avoiding shadowing etc. are big enough reasons for this, but there are even better ones – we'll talk about them later.

## Actions

Actions behave more or less like methods in Scala. They can be composed of several expressions (usually lines, but also *if* statements etc.). Each expression is evaluated separately and the evaluation result of the last expression/line is the return value of the method. The expression can be an *if*, *foreach* etc., an action call or a local value definition. Let's add some actions to our example:

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
            this#passwordHash(hashedPassword)
        } with values: hashedPassword: String = PasswordUtils.hash(newPassword)
    
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
    value totalClasses: Int = this.classes.size
    totalClasses
}
```

## Method invocation: *get* and *do* expressions

One thing to elaborate on here are those *.* and *#* expressions. They're very important. The dot is read as *get* and the hash is read as *do* (actually in the first version of the language this was the syntax, but it was replaced with the symbols for brevity).

This is a feature very specific to Tidy. As mentioned before, one of the most important ideas behind Tidy is making it clear when and where there is mutable state or side effects. The clear division into values/variables and functions/actions is one step in this direction. Another one is making it visible whether you call a *function* or an *action*. Some examples (notice too that methods with no arguments can be invoked without parentheses):

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

There are several very neat things about this system. There is no need to name your methods like *getFullName*, you can use shorter names and have your code just as readable because it's clear that *.* is a more-or-less pure *function*, getting some value (without changing or doing anything) and *#* is *an action*, doing/changing something.

This is especially significant when we notice that we can use this to define getters and setters. Automatically generated ones work like this (assume we have an object *student* of type *Student* with a variable field *age*):

- Getter: ``value age: Int = student.age // read "student get age"``
- Setter: ``student#age(23) // assigns 23 to student.age, read "student do age 23"``

One of its greatest benefits is that it allows us to have beautifully consistent syntax without any assignment operator in our language whatsoever! That's because every assignment can happen as a call to a setter method of some field on some object (all parameters and local variables are constant, like in Haskell, *this* is obligatory etc., so everything comes together perfectly).

This also gives us very good encapsulation and uniformity in syntax. We avoid having multiple things like ``student.age``,``student.getAge()`` or ``age = 3``, ``this.age = 3`` and ``student.setAge(3)`` doing exactly the same things and being used interchangeably and inconsistently. Getting a value is always ``student.age``, changing a value is always ``student#age(23)``, no matter where you are in the code. If the attribute/getter/setter is private, you may just not have access to it from outside the class, but everywhere you use the same clean and convenient syntax.


## Other features

Let's look at one last piece of code:

```
mutable class PhoneMessageReceiver extends MessageReceiver {
    
    values: {
        MESSAGE_RECEIVED: String = "You got a new message!",
        MESSAGE_INVALID: String = "Invalid message",
        MAX_MESSAGE_LENGTH: Int = 100,
        
        notificationChannels: List[NotificationChannel]
    }
    
    variables: {
        messages: List[Message] = List()
    }
    
    functions: {
        private parseMessage: (message: Message) -> String = {
            if (this.validateMessage(message)) {
                match message {
                    case Email -> "One new email"
                    case Sms -> "One new SMS"
                    case Notification -> "One new notification"
                }
            } else {
                this.MESSAGE_INVALID
            }
        }
        
        private validateMessage: (message: Message) -> Bool = {
            not message.empty and message.length <= this.MAX_MESSAGE_LENGTH
        }
    }
    
    actions: {
        override receive: (message: Message) -> Bool = {    
            System#printLine(this.MESSAGE_RECEIVED)
            
            value response: String = this.parseMessage(message)
            System#printLine(response)
            
            if (response != this.MESSAGE_INVALID) {
                super#receive(message)
                this#messages(this.messages.add(message))
                
                for (channel in this.notificationChannels) {
                    channel#notify(response)
                }
                
                True
            } else {
                False
            }
        }
    }
}

```

We can see some more new features here:

- Pattern matching syntax.

- Inheritance syntax.

- Method overriding syntax.

- There is a *super* keyword equivalent to Java *super* keyword.

- Logical operators: unary *not*, binary *and*, *or*. Logical literals are *True* and *False*.

- *Foreach* loop. There is also a *while* loop with standard syntax: ``while (condition) { }``. Note that these loops are imperative constructs and can only be used inside actions, not inside functions.


Other interesting features include:

- Higher-order functions and lambdas, for example: ``list.map((x) -> 2 * x)``

## Style

- Tidy is pretty serious about style conventions. It doesn't force you to keep most of them through syntax errors but it doesn't guarantee sensible interpretation of weird syntactic corner cases. In particular you should treat whitespaces seriously. The style used in the snippets above is highly encouraged. It closely resembles Java/Scala style conventions, so when in doubt, use those. Also, use curly brackets in multi-line expressions and don't put multiple expressions separate expressions (e.g. in action bodies) on one line.

- Case conventions are very important. Vast majority of them is the same as in Java. All class names must be in *UpperCamelCase*, method, parameter and local variable names in *lowerCamelCase*, except for constants that should be in *SCREAMING_SNAKE_CASE*.

- Naming conventions are similar to those in Scala, that is, try to use reasonably descriptive Java-style names, but in places where the code is mostly functional, it's okay to use less verbose, even one-letter identifiers, for example: ``add: (x: Int, y: Int) -> Int = x + y``.

- Curly braces should be at the end of the same line as the preceding expression (Java-style). Two alternative syntax styles (one-liner without curly braces and multi-line with curly braces) were described above and are important to keep in mind. If you need to wrap expressions across two or more lines in places where curly braces are not used (e.g. in a method body), remember to leave the operator at the end of the line, especially in actions, otherwise it might cause confusion in the action body evaluation, for example:

```
// GOOD
    value someText: String = "a" ++ "veeeeery" ++ 
        "long" ++ "text" ++ "indeed"

// BAD
    value someText: String = "a" ++ "veeeeery"
        ++ "long" ++ "text" ++ "indeed"

```

