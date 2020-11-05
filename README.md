## Ideas for a statically typed dialect of Scheme

Name: I'll use Steme here, but it might be Statics or something else.

What: A statically typed pure eager language.

Code: Steme looks and feels like Scheme, but behaves more like ML:
a statically typed eager language

Immutable: All Steme types are immutable,
though at the Scheme level they may be mutable.

Persistent:  Some but perhaps not all types have persistent variants.

Procedures: Accept multiple arguments and return multiple values,
unlike Haskell or ML.  Therefore no currying needed
(see [SRFI 36](http://srfi.schemers.org/srfi-36/srfi-36.html).

Macros: Same in Steme as in Scheme (unclear exactly what macros
R7RS-large will be required to support).
Unclear whether low-level macros can be written in Steme.

Tuples: Multiple values.

Static typing: Code must typecheck using Hindley-Milner.

Parametric polymorphism: As in ML.
Examples: list, vector, dictionary.

Ad hoc polymorphism: Type classes as in Haskell.
(ML has exactly one type class corresponding to Haskell Eq.)
Support multi-parameter classes, as GHC does but Haskell 98 does not.

Simple numeric types: Exact integer, exact rational,
inexact real, inexact complex.

Characters: Not a type, just 1-character strings,
but can be represented separately at the Scheme level.

Strings: Iterable but not indexable.
Iteration can be by codepoint, default and extended grapheme cluster, line,
or language-specific levels like word and sentence.

Simple types:  Defined by Scheme predicates.

Intersection types: Scheme record types.

Union types: Non-reified types whose predicate
is an OR of the member types.

FFI: Scheme is the FFI of Steme.
In order to call a Scheme procedure,
the programmer must certify that it is pure
and what type it returns.
Such certifications can be global or local.

Compilation targets:  R7RS Scheme,
a particular Scheme with its type annotations
(hoping that the Scheme compiler will remove type checks)
C code with stubs for a particular Scheme's FFI
(minimal type checks to separate fixnums and bignums, etc.).
Not a goal to compile to other random languages.

Calling Steme: If Scheme calls Steme with arguments of the wrong type,
it is an error, but Steme may or may not catch the error,
depending on the implementation.

Exceptions:  Steme can raise an exception.
Steme can also catch an exception and replace it
with an eagerly computed value,
but it does not know what the exception is.

Logging: Steme can log anything,
but it is an error to attempt to reread the log.

Comment in the issues or at [*cowan@ccil.org*](mailto:cowan@ccil.org).
