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

Chapter 2
==========
* for regular programming, the building blocks are terms and types
* for type level programming, the building blocks are types and kinds
