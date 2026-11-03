#' Create a compact comparison of pollen seasons
#'
#' Create a compact comparison of pollen seasons.
#'
#' @param data A data frame with station and species names, integer year, Date-valued date, and numeric conc columns.
#' @param species A single taxon name matching the species column.
#' @param year The reporting year as an integer.
#' @param cutoff Fraction of cumulative annual concentration excluded at each end of the season; defaults to 0.025.
#' @returns A data frame with Swedish column names and previous-year values in parentheses.
#' @details Legacy alternative to table_seasons().
#' @export
table_seasons_old <- function(data, species, year, cutoff = .025){
  this_year <- get_seasons(data, species, year, cutoff)
  last_year <- get_seasons(data, species, year - 1, cutoff) |>
    rename_with(~paste0("last_", .x), .cols = -station)
  this_year |> left_join(last_year, by = "station") |>
    mutate(
      season = paste0(short_date_seq(season_start, season_end),
                      " (", short_date_seq(last_season_start, last_season_end), ")"),
      date_max = paste0(short_date(date_max), " (", short_date(last_date_max), ")"),
      max_count = paste0(year_max, " (", last_year_max, ")"),
      total = paste0(total, " (", last_total, ")"),
      start = short_date(start)
    ) |>
    select(station, start, season, date_max, max_count, total) |>
    mutate(across(everything(), ~str_replace(.x, pattern = "NA", replacement = "-"))) |>
    select("Mätstation" = station, "Mätstart" =  start, "Pollensäsong" = season, "Datum för säsongsmaximum" = date_max,
           "Säsongens maximiantal" = max_count, "Årssumma" = total)
}
