# R6 class with generic reusable functionality

[`R6`](https://r6.r-lib.org/reference/R6Class.html) class with generic
(abstract) new cloning functionality.

## Public fields

- `object_generator`:

  Class object generator used to create new clones, particularly for
  user inheritance.

- `attached`:

  A list of dynamically attached attributes (name-value pairs).

## Methods

### Public methods

- [`GenericClass$new()`](#method-GenericClass-new)

- [`GenericClass$new_clone()`](#method-GenericClass-new_clone)

- [`GenericClass$clone()`](#method-GenericClass-clone)

------------------------------------------------------------------------

### Method `new()`

Initialization method saves an object generator for new cloning.

#### Usage

    GenericClass$new(object_generator = NULL, ...)

#### Arguments

- `object_generator`:

  Class object generator used to create new clones, particularly for
  user inheritance.

- `...`:

  Parameters passed individually (ignored).

------------------------------------------------------------------------

### Method `new_clone()`

Creates a new (re-initialized) object of the current (inherited) object
class with optionally passed parameters.

#### Usage

    GenericClass$new_clone(...)

#### Arguments

- `...`:

  Parameters passed via the inherited class constructor (defined in
  initialize and run via new).

#### Returns

New object of the current (inherited) class.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    GenericClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
object1 <- GenericClass$new()
class(object1)
#> [1] "GenericClass" "R6"          
# Referencing
object_ref <- object1
object_ref$attached$a <- 1
object1$attached
#> $a
#> [1] 1
#> 
# Cloning
object2 <- object1$clone()
object2$attached$b <- 2
object1$attached
#> $a
#> [1] 1
#> 
object2$attached
#> $a
#> [1] 1
#> 
#> $b
#> [1] 2
#> 
# New cloning
object3 <- object1$new_clone()
object3$attached$c <- 3
object1$attached
#> $a
#> [1] 1
#> 
object3$attached
#> $c
#> [1] 3
#> 
```
