# Helper Functions for LQAS Pipeline

fast_binary_convert <- function(x) {
  data.table::fcase(
    tolower(x) %in% c("yes", "y", "1"), 1,
    tolower(x) %in% c("no", "n", "0"), 0,
    default = NA_real_
  )
}

clean_text <- function(x) {
  x <- as.character(x)
  x <- stringr::str_trim(x)
  x <- stringr::str_squish(x)
  x <- toupper(x)
  return(x)
}

performance_category <- function(total_missed) {
  data.table::fcase(
    total_missed < 4, "high",
    total_missed < 9, "moderate",
    total_missed < 20, "poor",
    default = "very poor"
  )
}
