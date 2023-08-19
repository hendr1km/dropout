#' Detecting Dropouts
#'
#' The function detects participants who drop out of the survey by recognising NA sequences up to the last question of the survey. The column of the dropout is also determined, as well as the respective index of the column. This forms the basis for various methods of dropout analysis.
#'
#' @param data dataframe or tibble to detect dropouts.
#' @param last_col Index position or column name of the last survey item. This is an optional argument and is needed if there are other columns in your dataframe after the survey items you want to detect the dropout for.
#'
#' @return A dataframe or tibble containing the following for each row of your dataset: A logical whether a dropout has been detected and, if so, the column name where it occurs and the index of the column.
#' @export
#'
#' @examples
#' # detecting dropouts with the index position 50 as the last question column
#' drop_detect(df, last_col = 50)
#'
#'# counting dropouts by gender
#' df %>%
#'  drop_detect(last_col = "question_50") %>%
#'  bind_cols(df,.) %>%
#'  group_by(gender) %>%
#'  count(dropout)
#'
#'# mean age of participants who did not drop out
#' df %>%
#'  drop_detect() %>%
#'  select(dropout) %>%
#'  bind_cols(df,.) %>%
#'  filter(dropout = FALSE) %>%
#'  summarise(mean_age = mean(age))
#'
#'# subsample for the first 8 questions without dropouts
#' subsample <-
#'  df |>
#'  drop_detect() %>%
#'  bind_cols(df,.) %>%
#'  filter(dropout = FALSE | dropout_index > 8) %>%
#'  select(1:8)
#'
#'# comparing the count of dropouts for two sections of the survey
#'df %>%
#' drop_detect(last_col = 35) %>%
#' select(dropout_index) %>%
#' mutate(section = case_when(dropout_index >= 5 & dropout_index <= 20 ~ "Section_1",
#'        dropout_index >=21 & dropout_index <=35 ~ "Section_2")) %>%
#'          count(section) %>%
#'          filter(!is.na(section))
#'
#'
#'  # Read more about possible use cases in the vignette :)

drop_detect <- function(data, last_col) {

  # Check if there is the optional operator last col, otherwise use data
  if (missing(last_col)) {
    data
  } else {
    if (is.character(last_col)) {  # Change 'col' to 'last_col'
      col_index <- which(names(data) == last_col)  # Change 'data_frame' to 'data'
    } else if (is.numeric(last_col)) {
      col_index <- last_col
    } else {
      stop("Column must be specified by name or index.")
    }

    if (length(col_index) != 1) {
      stop("Column not found or ambiguous.")
    }

    data <- data[, 1:col_index]
  }

  data_subset <-
    output <- apply(data, 1, function(row) {
      dropout_col <- character(1)
      for (i in 1:(ncol(data))) {
        if (all(is.na(row[i:ncol(data)]))) {
          dropout_col <- colnames(data)[i]
          break
        }
      }
      if (identical(dropout_col, character(0))) {
        dropout_col <- NA
      }
      return(dropout_col)
    })

  output <- ifelse(output == "", NA, output)

  drop_data <- data.frame(dropout_column = output)

  if (!any(is.na(data[, ncol(data)]))) {
    warning("No Dropouts detected. \n Please set last_col to the the last survey item. Use ?drop_detect to find out more")
  }

  drop_data$dropout <- !is.na(drop_data$dropout_column)

  # experimental (dropout index)

  drop_data$dropout_index <- ifelse(!is.na(drop_data$dropout_column), match(drop_data$dropout_column, colnames(df)), NA)

  if (requireNamespace("tibble", quietly = TRUE)) {
    drop_data <- tryCatch(
      as_tibble(drop_data),
      error = function(e) {
        drop_data$dropout <- as.logical(drop_data$dropout)
        drop_data
      }
    )
  }

  drop_data

}
