#' Dropout Summary
#'
#' @param data dataframe or tibble to detect dropouts.
#' @param last_col Index position or column name of the last survey item. This is an optional argument and is needed if there are other columns in your dataframe after the survey items you want to detect the dropout for.
#'
#' @return A dataframe or tibble containing the following for each row of your dataset: A logical whether a dropout has been detected and, if so, the column name where it occurs and the index of the column.
#' @export
#'
#' @examples

drop_summary <- function(df) {

 drop_df <- drop_detect(df)[-c(2)]
 drop_df$col <- factor(drop_df$col, levels = colnames(df))

 # Group by 'col' and count occurrences
 grouped_counts <- table(drop_df$col)

 # Create a new data frame with the counts
 result_df <- data.frame(col = names(grouped_counts), dropout = as.vector(grouped_counts))
 result_df$drop_rate <- round(cumsum(result_df$dropout) / nrow(df), 2)
 result_df$missing <- colSums(is.na(df))
 result_df$completion_rate <- round((nrow(df) - result_df$missing) / nrow(df), 2)
 result_df$true_na <- result_df$missing - cumsum(result_df$dropout)
 result_df$drop_na <- result_df$missing - result_df$true_na

 # Reorder the columns
 result_df <- result_df[, c("col", "dropout", "drop_rate", "drop_na", "true_na", "missing", "completion_rate")]

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

 options(tibble.width = Inf)
 return(result_df)
}
