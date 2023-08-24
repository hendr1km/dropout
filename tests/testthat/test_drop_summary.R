library(testthat)

library(dropout)

test_that("creates correct dropout summary", {
 df <- data.frame(
  Name = c("Alice", "Bob", "Sam", "David", NA),
  Age = c(NA, 30, NA, 22, 28),
  Hair_color = c("red", "green", NA, "pink", "brown"),
  Likes_dogs = c(TRUE, FALSE, NA, FALSE, NA)
 )

 result <- drop_summary(df)

 expected_result <- data.frame(
  column_name = (c("Name", "Age", "Hair_color", "Likes_dogs")),
  dropout = as.integer(c(0, 1, 0, 1)),
  drop_rate = (c(0.0, 0.2, 0.2, 0.4)),
  drop_na = (c(0, 1, 1, 2)),
  true_na = (c(1, 1, 0, 0)),
  missing = (c(1, 2, 1, 2)),
  completion_rate = (c(0.8, 0.6, 0.8, 0.6))
 )

 expect_identical(result, expected_result)
})


