context("Generic Class")

test_that("inheritance, attributes, and new cloning", {

  # Inheritance
  TestInheritedClass <- R6::R6Class("TestInheritedClass", inherit = GenericClass,
                                    public = list(attr1 = NULL, attr2 = NULL))
  test_class <- TestInheritedClass$new(object_generator = TestInheritedClass) # for check only
  expect_is(test_class, "TestInheritedClass")
  expect_is(test_class, "GenericClass")
  expect_is(test_class, "R6")

  # Attributes
  test_class$attr1 <- "test1"
  test_class$attr2 <- c(11, 22, 33)
  test_class$attached$extra <- "hitchhiker"
  expect_equal(test_class$attr1, "test1")
  expect_equal(test_class$attr2, c(11, 22, 33))
  expect_equal(test_class$attached, list(extra = "hitchhiker"))

  # New cloning
  expect_silent(test_clone <- test_class$new_clone())
  expect_is(test_clone, "TestInheritedClass")
  expect_null(test_clone$attr1)
  expect_null(test_clone$attr2)
  expect_equal(test_clone$attached, list())

})
