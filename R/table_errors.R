#' Tabulate monitoring periods and technical errors
#'
#' Tabulate monitoring periods and technical errors.
#'
#' @param data A data frame with station and species names, integer year, Date-valued date, and numeric conc columns.
#' @param error_year The year for which to summarize monitoring errors.
#' @returns A flextable object with monitoring dates, error periods, and status bars.
#' @details Requires an additional logical is_error column. The first and last flagged dates per station are excluded from the listed error periods. Status bars omit the first and last monitoring dates.
#' @export
table_errors <- function(data, error_year){
  date_sequence <- function(start_date, end_date){
    if (start_date == end_date)
      return(paste0(day(start_date), swe_month[month(start_date)]))
    if (month(start_date) == month(end_date))
      return(paste0(day(start_date), "-", day(end_date), swe_month[month(start_date)]))
    return(paste0(day(start_date), swe_month[month(start_date)], "-", day(end_date), swe_month[month(end_date)]))
  }
  start_end <- data |>
    filter(year == error_year) |>
    summarise(start = min(date) |> short_date(), end = max(date) |> short_date(), .by = "station") |>
    arrange(station)

  errors <- data |>
    filter(is_error == TRUE, year == error_year) |>
    select(date, station) |>
    mutate(first_or_last = (date %in% c(min(date), max(date))), .by = "station") |>
    filter(!first_or_last) |>
    distinct() |>
    arrange(station, date) |>
    mutate(day = yday(date),
           is_start_day = c(TRUE, !(diff(day) == 1)),
           period = cumsum(is_start_day), .by = c("station")) |>
    summarise(start_date = min(date), end_date = max(date), .by = c("station", "period")) |>
    rowwise() |>
    mutate(days = date_sequence(start_date, end_date)) |>
    ungroup() |>
    summarise(days = paste0(days, collapse = ", "), .by = c("station"))
    # mutate(days = str_remove(days, paste0(start, ", ", end)),
    #        days = str_remove(days, paste0(start, ", ")),
    #        days = str_remove(days, paste0(", ", end

  bars <- data |>
    filter(year == error_year) |>
    select(station, date, is_error) |>
    mutate(status = ifelse(is_error == TRUE, "down", "up")) |>
    distinct() |>
    mutate(first_or_last = (date %in% c(min(date), max(date))), .by = "station") |>
    filter(!first_or_last) |>
    complete(date, station) |>
    mutate(status = ifelse(is.na(status), "off", status)) |>
    nest_by(station)

  start_end |> left_join(errors, by = "station") |>
    left_join(bars, by = "station") |>
    arrange(station) |>
    rowwise() |>
    mutate(bars = list(gg_bars(data))) |>
    select("Mätstation" = station, "Mätstart" = start, "Mätslut" = end, "Driftstörningar" = days, bars) |>
    qflextable() |>
    mk_par(j = c("bars"),
           value = as_paragraph(gg_chunk(value = ., height = .15, width = 1)),
           use_dot = TRUE) |>
    set_header_labels(bars = "Driftstörningar") |>
    set_table_properties(layout = "autofit")  |>
    fontsize(size = 8, part = "all") |>
    padding(padding.top = 0, padding.bottom = 0)
}

#' Draw a monitoring status strip
#'
#' Draw a monitoring status strip.
#'
#' @param bars A data frame with Date-valued date and status (up, down, or off) columns.
#' @returns A ggplot object with green, red, and grey status bars.
#' @export
gg_bars <- function(bars){
    ggplot(bars, aes(x = date, y = 1, fill = status)) +
      geom_col(show.legend = FALSE, width = 1) +
      theme_void() + scale_fill_manual(values = c("up" = "green", "down" = "red", off = "lightgrey"))
}
