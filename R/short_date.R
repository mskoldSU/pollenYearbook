#' Format dates with Swedish month labels
#'
#' Format dates with Swedish month labels.
#'
#' @param date A vector of Date values.
#' @returns A character vector; missing dates become a dash.
#' @details Uses the bundled swe_month labels without a separator between day and month.
#' @export
#' @examples
#' short_date(as.Date(c("2025-06-01", NA)))
short_date <- function(date){
  ifelse(is.na(date), "-", paste0(day(date), swe_month[month(date)])) # |> str_replace_all(pattern = "NA", replacement = "")
}
