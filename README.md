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
* The reason that invariant maps that you've seen before have both
  transformation functions, is that invariant mapping requires `a` and `b` to be
  isomorphisms.

```haskell
class Invariant f where
  invmap :: (a -> b) -> (b -> a) -> f a -> f b
```

* *positive position*: this is based off of the canonical representation, and
  there is a chart for this.

* Here's an example of variance arithmetic:

 - normally a tuple `(a, b)` is `+, +`
 - also, a function `a -> b` is `- -> +`
 - in the function `(a, Bool) -> Int` the variance isn't `(+, +) -> +`
 - the tuples variance gets multiplied just like regular algebra, and + * - is a
   negative, so the variance is `(-, -) -> +`.
 - this function is contravariant in `a` (negative position)

 Chapter 4: Working with Types
 -----------------------------
 > `-XTypeApplications` and `-XScopedTypeVariables` are the two most
fundamental extensions in a type-programmer’s toolbox. They go
together hand in hand.

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

### TypeApplications
* Allows you to explictly fill in type variables
  - this is just like applying a value to a function, you're applying a type to
    a type-level function

```haskell
*Main Lib One Two> :set -XTypeApplications
*Main Lib One Two> :t fmap
fmap :: Functor f => (a -> b) -> f a -> f b
*Main Lib One Two> :t fmap @Maybe
fmap @Maybe :: (a -> b) -> Maybe a -> Maybe b
```

* You an also leave the functor polymorphic, and fill in the other type holes

```haskell
fmap @_ :: Functor w => (a -> b) -> w a -> w b
*Main Lib One Two> :t fmap @_ @Int @Bool
fmap @_ @Int @Bool :: Functor w => (Int -> Bool) -> w Int -> w Bool
```

### GADTs
* you need `GADT` to use type equalities (like `a ~ Int`)
  - GADTs are really just syntactic sugar over type equalities
* They also allow you to write DSLs for your application

```haskell
data Expr_ a
= (a ∼ Int ) => LitInt_ Int
| (a ∼ Bool ) => LitBool_ Bool
| (a ∼ Int ) => Add_ ( Expr_ Int ) ( Expr_ Int )
| If_ ( Expr_ Bool ) ( Expr_ a) ( Expr_ a)

-- this is equivalent to

{-# LANGUAGE GADTs #-}

data Expr a where
  LitInt :: Int -> Expr Int
  LitBool :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
```

> `(:#) :: t -> HList ts -> HList (t ': ts)` if you have a t thing, and an HList
> that contains ts things, you can make an HList that contains (t : ts) things
> if it were just -> HList ts at the end, then it wouldn't change the type first
> of all you could prepend arbitrarily many elements of any type to any HList
> without changing the type, if that were the case in the same way consing a
> regular list doesn't change the type second you'd never be able to make a
> (t:ts) thing (barring undefined), since HList ts -> HList ts would only
> preserve what you've already got, and the only other thing is []

 RankN
 ------
 * Remember that `a -> a` gets quantified to `forall a. a -> a`

 > The intuition behind higher-rank types is that they are functions which take
 > callbacks . The rank of a function is how often control gets “handed off”. A
 > rank-2 function will call a polymorphic function for you, while a rank-3
 > function will run a callback which itself runs a callback.

> More precisely, a function gains higher rank every time a forall quantifier
> exists on the left-side of a function arrow.

Continuation Monad
------------------
> The type `forall r. (a -> r) -> r` is known as being in continuation-passing
> style or more tersely as CPS .

> Since we know that `Identity a ~= a` and that `a ∼= forall r. (a -> r) -> r` ,
> we should expect the transitive isomorphism between `Identity a` and CPS.
> Since we know that `Identity a` is a Monad and that isomorphisms preserve
> typeclasses, we should expect that CPS also forms a Monad .
