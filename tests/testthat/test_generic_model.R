context("Generic Model")

test_that("inheritance", {
  TestInheritedModel <- R6::R6Class("TestInheritedModel",
    inherit = GenericModel,
    public = list(attr1 = NULL),
    private = list(.model_attributes = c("attr1", "attr2", "attr3"), .active_attributes = c("attr2"), .attr2 = NULL, attr3 = NULL),
    active = list(attr2 = function(value) {
      if (missing(value)) {
        private$.attr2
      } else {
        private$.attr2 <- value
      }
    })
  )
  test_model <- TestInheritedModel$new(object_generator = TestInheritedModel) # for check only
  expect_is(test_model, "TestInheritedModel")
  expect_is(test_model, "GenericModel")
  expect_is(test_model, "GenericClass")
  expect_is(test_model, "R6")
})

test_that("attribute get and set methods", {
  TestInheritedModel <- R6::R6Class("TestInheritedModel",
    inherit = GenericModel,
    public = list(attr1 = NULL),
    private = list(.model_attributes = c("attr1", "attr2", "attr3"), .active_attributes = c("attr2"), .attr2 = NULL, attr3 = NULL),
    active = list(attr2 = function(value) {
      if (missing(value)) {
        private$.attr2
      } else {
        private$.attr2 <- value
      }
    })
  )
  test_model <- TestInheritedModel$new(
    object_generator = TestInheritedModel, # for check only
    params = list(attr1 = "test1", cat = "meow"),
    attr2 = c(11, 22, 33), attr3 = 99, dog = "woof"
  )
  expect_equal(
    test_model$get_attribute_names(),
    c("attr1", "attr2", "attr3", "dog", "cat", "error_messages", "warning_messages")
  )
  expect_equal(test_model$attr1, "test1")
  expect_equal(test_model$attr2, c(11, 22, 33))
  expect_null(test_model$attr3) # not public or active
  expect_equal(test_model$get_attribute("attr3"), 99)
  expect_null(test_model$get_attribute("dummy"))
  expect_null(test_model$get_attribute(123))
  expect_null(test_model$get_attribute("123"))
  expect_null(test_model$cat) # within attached
  expect_named(test_model$attached, c("dog", "cat"))
  expect_equal(test_model$get_attributes(c("dog", "cat")), list(dog = "woof", cat = "meow"))
  test_model$set_attributes(attr1 = "test2", attr2 = c(44, 55), attr3 = 100, dog = "bark")
  expect_equal(test_model$get_attributes(), list(
    attr1 = "test2", attr2 = c(44, 55),
    attr3 = 100, dog = "bark", cat = "meow"
  ))
})

test_that("attribute aliases", {
  TestInheritedModel <- R6::R6Class("TestInheritedModel",
    inherit = GenericModel,
    public = list(attr1 = NULL),
    private = list(.model_attributes = c("attr1", "attr2", "attr3"), .active_attributes = c("attr2"), .attr2 = NULL, attr3 = NULL),
    active = list(attr2 = function(value) {
      if (missing(value)) {
        private$.attr2
      } else {
        private$.attr2 <- value
      }
    })
  )
  test_model <- TestInheritedModel$new(
    object_generator = TestInheritedModel, # for check only
    attribute_aliases = list(
      a1 = "attr1", a2_1 = "attr2[1]", a3_a = "attr3$a",
      d_2 = "dog[2]", c_b = "cat$b"
    ),
    params = list(attr1 = "test1", cat = list(a = "meow", b = "purr")),
    attr2 = c(11, 22, 33), attr3 = list(a = 9, b = 10), dog = c("woof", "bark")
  )
  expect_equal(
    test_model$get_attribute_aliases(),
    c(
      "attr1", "attr2", "attr3", "dog", "cat", "error_messages", "warning_messages",
      "a1", "a2_1", "a3_a", "d_2", "c_b"
    )
  )
  expect_equal(test_model$get_attribute("a1"), "test1")
  expect_equal(test_model$get_attributes(c("a2_1", "a3_a", "d_2", "c_b")), list(a2_1 = 11, a3_a = 9, d_2 = "bark", c_b = "purr"))
  test_model$set_attributes(params = list(a2_1 = 12, d_2 = "ruff"), a3_a = 8, c_b = "hiss")
  expect_equal(
    test_model$get_attributes(c("attr2", "attr3", "dog", "cat")),
    list(attr2 = c(12, 22, 33), attr3 = list(a = 8, b = 10), dog = c("woof", "ruff"), cat = list(a = "meow", b = "hiss"))
  )
})

test_that("new cloning", {
  TestInheritedModel <- R6::R6Class("TestInheritedModel",
    inherit = GenericModel,
    public = list(attr1 = NULL),
    private = list(.model_attributes = c("attr1", "attr2", "attr3"), .active_attributes = c("attr2"), .attr2 = NULL, attr3 = NULL),
    active = list(attr2 = function(value) {
      if (missing(value)) {
        private$.attr2
      } else {
        private$.attr2 <- value
      }
    })
  )
  test_model <- TestInheritedModel$new(
    object_generator = TestInheritedModel, # for check only
    model_attributes = c("attr1", "attr2", "attr3", "dog", "cat"),
    attribute_aliases = list(
      a1 = "attr1", a2_1 = "attr2[1]", a3_a = "attr3$a",
      d_2 = "dog[2]", c_b = "cat$b"
    ),
    params = list(attr1 = "test1", cat = list(a = "meow", b = "purr")),
    attr2 = c(11, 22, 33), attr3 = list(a = 9, b = 10), dog = c("woof", "bark")
  )
  expect_silent(test_clone <- test_model$new_clone())
  expect_is(test_clone, "TestInheritedModel")
  expect_equal(test_clone$model_attributes, c("attr1", "attr2", "attr3", "dog", "cat"))
  expect_equal(
    test_clone$get_attribute_names(),
    c("attr1", "attr2", "attr3", "dog", "cat", "error_messages", "warning_messages")
  )
  expect_named(test_clone$attribute_aliases, c("a1", "a2_1", "a3_a", "d_2", "c_b"))
  expect_equal(test_clone$get_attributes(), list())
})
