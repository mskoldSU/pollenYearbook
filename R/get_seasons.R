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
    mutate(cum_conc = cumsum(conc) / max(sum(conc), 1),
           season = case_when(cum_conc <= cutoff ~ "pre_season",
                              cum_conc >= (1 - cutoff) ~ "post_season",
                              TRUE ~ "season"),
           year_max = max(conc, na.rm = TRUE),
           date_max = date[conc == year_max][1],
           total = sum(conc, na.rm = TRUE),
           start = min(date, na.rm = TRUE),
           end = max(date, na.rm = TRUE),
           .by = c("station")) |>
    #filter(season == "season") |>
    summarise(season_start = dplyr::if_else(sum(conc) > 0, max(date[season == "pre_season"] + 1), NA),
              season_end = dplyr::if_else(sum(conc) > 0, max(min(date[season == "post_season"]) - 1, season_start), NA),
              year_max = year_max[1],
              date_max = dplyr::if_else(sum(conc) > 0, date_max[1], NA),
              total = total[1],
              start = start[1],
              end = end[1],
              .by = c("station")
    )

}
