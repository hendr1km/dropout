#' Dropout Summary
#'
#' This function produces a summary of the occurrence of dropouts and NA for the dataset. This makes it possible to distinguish which parts of the missing values are due to dropouts and which are due to other causes. In this way, questions with high dropout rates or non-dropout NAs can be identified.
#'
#' @param data dataframe or tibble to detect dropouts.
#' @param last_col Index position or column name of the last survey item. This is an optional argument and is needed if there are other columns in your dataframe after the survey items you want to detect the dropout for. Read more using ?drop_detect
#'
#' @return A dataframe or tibble containing the following for each column of your dataset: column in your dataset: the column name (column_name), the number of dropouts occurring in each column (dropout), the proportion of respondents who dropped out of the survey in that column (drop_rate), the sum of na values in the column not due to dropouts (true_na) and due to dropouts (drop_na). As well as the total missing values and the total proportion of people who completed the column (completion_rate).
#' @export
#'
#' @examples
#' # drop_summary (with the index of the last servey item)
#' drop_summary(df, last_col = 50)
#'
#' # columns with most dropouts (descending)
#' df %>%
#'  drop_summary() %>%
#'  arrange(dsc(dropout)) %>%
#'  head(n = 10)

drop_summary <- function(data, last_col) {

  drop_df <- drop_prepare(data, last_col)

  col_index <- drop_df$list_index
  drop_df <- drop_df$list_data[-c(2,3)]
  drop_df$dropout_column <- factor(drop_df$dropout_column, levels = names(data[, 1:col_index]))

  # Group by 'col' and count occurrences
  grouped_counts <- table(drop_df$dropout_column)

  # Create a new data frame with the counts
  result_df <- data.frame(column_name = names(grouped_counts), dropout = as.vector(grouped_counts))
  result_df$drop_rate <- round(cumsum(result_df$dropout) / nrow(data), 2)
  result_df$missing <- colSums(is.na(data[, 1:col_index]))
  result_df$completion_rate <- round((nrow(data) - result_df$missing) / nrow(data), 2)
  result_df$true_na <- result_df$missing - cumsum(result_df$dropout)
  result_df$drop_na <- result_df$missing - result_df$true_na

  # Reorder the columns
  result_df <- result_df[, c("column_name", "dropout", "drop_rate", "drop_na", "true_na", "missing", "completion_rate")]

  # Convert to tibble if tibble is installed, otherwise df
  if (requireNamespace("tibble", quietly = TRUE)) {
    result_df <- tryCatch(
      as_tibble(result_df),
      error = function(e) {
        # Return the data frame directly if tibble package is not available
        return(result_df)
      }
    )
  }

  return(result_df)
}
