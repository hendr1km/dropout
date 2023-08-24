library(testthat)
library(dropout)

# detects the correct dropouts with index for every row in the dataframe
test_that("detect dropouts correctly", {
 df <- data.frame(
  Name = c("Alice", "Bob", "Sam", "David", NA),
  Age = c(NA, 30, NA, 22, 28),
  Hair_color = c("red", "green", NA, "pink", "brown"),
  Likes_dogs = c(TRUE, FALSE, NA, FALSE, NA)
 )

 result <- drop_detect(df)

 expected_result <- data.frame(
  dropout_column = as.character(c("NA", "NA", "Age", "NA", "Likes_dogs")),
  dropout = as.logical(c(FALSE, FALSE, TRUE, FALSE, TRUE)),
  dropout_index = as.integer(c(NA, NA, 2, NA, 4))
 )

 expect_identical(result, expected_result)
})


