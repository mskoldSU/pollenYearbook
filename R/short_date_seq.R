#' Format date ranges with Swedish month labels
#'
#' Format date ranges with Swedish month labels.
#'
#' @param date1 A vector of start dates.
#' @param date2 A vector of end dates, recycled against date1.
#' @returns A character vector of date ranges; missing start dates become a dash.
#' @export
short_date_seq <- function(date1, date2){
  ifelse(is.na(date1), "-", paste0(short_date(date1), "-", short_date(date2)))
}
