Chapter 1
============
* Any two types with the same cardinality will always be isomorphic to one another
* An isomorphism between types s and t is defined as a pair of functions `to` and `from`:

```haskell
to ::s->t 
from::t->s
```

* For types with cardinality `n` there are `n!` isomorphisms between them.

Sum, and Product Types
------------------------
* You can use isomorphisms to prove type theories
 - For instance, you can prove that *a × 1 = a* by showing an isomorphism between `(a, ()) and a`

 ```haskell
prodUnitTo :: a -> (a, ())
prodUnitTo a = (a, ()) prodUnitFrom :: (a, ()) -> a
prodUnitFrom (a, ()) = a
```

* Just like with 1 and 0, `()` and `Void` are the identity values for sum and
  product respectively
  - You prove this by using isomorphisms for `a -> (a, ())` and `a -> Either a Void`

Curry Howard
-------------
* This is the table describing the relationship between types and cardinalities
* take a^1 = a
  - express this with curry howard
  - |1| -> a
  - () -> a
  - what this shows is that a program from unit to a is the same as the bound
    value a
    - this describes purity

Canonical Repr
---------------
* every representation of a type is equivalently useful as another other
  representation
  - the conventional form of your type is called the *sum of products*
  - basically, something is in its canonical repr if all sum types are:
    * on the outside
    * represented as `Either`
  - and all product types are:
    * on the inside
    * represented by (,)
  - examples

```haskell
Either (a,b) (c,d)
()
a -> b
```

Chapter 2: Kinds
====================
* for regular programming, the building blocks are terms and types
* for type level programming, the building blocks are types and kinds

* concrete types are sometimes called *value types*
* Type constraints also have kinds

```haskell
show :: Show a => a -> String
```

* `Show a` has kind `CONSTRAINT`

### DataKinds
* Lifts Data Constructor -> Type Constructors & Type -> Kind
* This is called "promotion"

```haskell
{-# LANGUAGE DataKinds #-}

data Bool = True | False
-- kind Bool = 'True | 'False
```

* `Bool` is now a kind, and `'True/'False` are types of kind `Bool`
  - `'True` and `'False` are called *promoted data constructors*
  - the `'` *tick* is standard to signal that this thing is a promoted data
    constructor

* You need `TypeLits` extension if you're going to promote stdlib types to kinds
  (like `String, Num`, etc)

#### Type Level Functions (*closed type families*)
- these type families are functions at a type level
- you cannot just promote term-level functions like you can with data
  constructors

Chapter 3: Variance
--------------------
* Here is the basic understanding you need to have:

> variables in positive position are *produced* or *owned*, while those in
negative position are *consumed*. Products, sums and the right-side of an arrow
are all pieces of data that already exist or are produced, but the type on the
left-side of an arrow is indeed consumed.

* A type `T` can only be a functor if it is covariant

* Here's an example of variance arithmetic:

 - normally a tuple `(a, b)` is `+, +`
 - also, a function `a -> b` is `- -> +`
 - in the function `(a, Bool) -> Int` the variance isn't `(+, +) -> +`
 - the tuples variance gets multiplied just like regular algebra, and + * - is a
   negative, so the variance is `(-, -) -> +`.
 - this function is contravariant in `a` (negative position)

 Chapter 4: Working with Types
 -----------------------------
 * `ScopedTypeVariables` is for referencing bound type variables, but you need
   to use `forall` for it to work

```haskell
broken :: (a -> b) -> a -> b
broken f a = apply where
  apply :: c
  apply = f a

working :: forall a b. (a -> b) -> a -> b
working f a = apply where
  apply :: b
  apply = f a
```
