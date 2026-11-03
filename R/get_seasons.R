#' Summarize pollen seasons by station
#'
#' Summarize pollen seasons by station.
#'
#' @param data A data frame with station and species names, integer year, Date-valued date, and numeric conc columns.
#' @param species A single taxon name matching the species column.
#' @param year The reporting year as an integer.
#' @param cutoff Fraction of cumulative annual concentration excluded at each end of the season; defaults to 0.025.
#' @returns A data frame with station, season_start, season_end, year_max, date_max, total, start, and end columns.
#' @details Rows with missing concentrations are excluded. Season dates have cumulative concentration strictly between cutoff and 1 - cutoff. Maximum ties use the first date. Error flags are not applied here.
#' @export
#' @examples
#' d <- data.frame(station = "Example", species = "Betula", year = 2025L,
#'                 date = as.Date("2025-04-01") + 0:4, conc = c(1, 4, 10, 4, 1))
#' get_seasons(d, "Betula", 2025)
get_seasons <- function(data, species, year, cutoff = .025){
  filter_year <- year
  filter_species <- species
  data |> filter(year == filter_year, species == filter_species, !is.na(conc)) |>
    arrange(station, date) |>
    mutate(cum_conc = cumsum(conc) / sum(conc),
           season = case_when(cum_conc <= cutoff ~ "pre_season",
                              cum_conc >= (1 - cutoff) ~ "post_season",
                              TRUE ~ "season"),
           year_max = max(conc, na.rm = TRUE),
           date_max = date[conc == year_max][1],
           total = sum(conc, na.rm = TRUE),
           start = min(date, na.rm = TRUE),
           end = max(date, na.rm = TRUE),
           .by = c("station")) |>
    filter(season == "season") |>
    summarise(season_start = min(date),
              season_end = max(date),
              year_max = year_max[1],
              date_max = date_max[1],
              total = total[1],
              start = start[1],
              end = end[1],
              .by = c("station")
    )

}
