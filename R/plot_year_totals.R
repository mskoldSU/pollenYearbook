#' Plot annual pollen totals by station
#'
#' Plot annual pollen totals by station.
#'
#' @param data A data frame with station and species names, integer year, Date-valued date, and numeric conc columns.
#' @param species A single taxon name matching the species column.
#' @param start_year First included year; NA uses end_year minus 20.
#' @param end_year Reference year for the default start year and axis breaks. Observations after this year are not removed.
#' @returns A ggplot object.
#' @details Requires an additional logical is_error column. Years with errors on more than 20 percent of the estimated season are shaded.
#' @export
plot_year_totals <- function(data, species, start_year = NA, end_year = 2024){
  filter_species <- species
  if (is.na(start_year))
    start_year <- end_year - 20

  seasons <- data |> filter(species == filter_species, !is.na(conc)) |>
    mutate(day = yday(date)) |>
    summarise(conc = mean(conc, na.rm = TRUE), .by = c("station", "day")) |>
    arrange(station, day) |>
    mutate(conc = cumsum(conc) / sum(conc, na.rm = TRUE), .by = station) |>
    summarise(start = max(c(min(day), day[conc < .025])), end = min(c(max(day), day[conc > .975])), length = end - start + 1, .by = station)

  error_years <- data |>
    filter(species == filter_species, year >= start_year) |>
    left_join(seasons, by = "station") |>
    filter(yday(date) >= start, yday(date) <= end) |>
    summarise(n = sum(is_error), .by = c("station", "year", "length")) |>
    mutate(error = n/length > 0.2)

  start_years <- data |> filter(species == filter_species, !is.na(conc), year >= start_year) |>
    summarise(year = min(year), .by = "station") |>
    filter(year > start_year)

  data |> filter(species == filter_species, year >= start_year) |>
    summarise(conc = sum(conc, na.rm = TRUE), .by = c("station", "year")) |>
    left_join(error_years, by = c("station", "year")) |>
    ggplot() + geom_col(aes(x = year, y = conc, fill = error), width = 1,  color = "steelblue", show.legend = FALSE) +
    facet_wrap(~station, ncol = 2) + theme_bw() +
    scale_y_continuous(expand = expansion(c(0, .05))) +
    scale_x_continuous(breaks = seq(start_year + ((start_year %% 2) != (end_year %% 2)), end_year, by = 2)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    labs(x = "", y = "") +
    theme(plot.margin = margin(t = 4, r = 4, b = -10, l = 4, unit = "pt")) +
    scale_fill_manual(values = c("FALSE" = "steelblue", "TRUE" = "lightgray")) +
    geom_vline(data = start_years, aes(xintercept = year - .5))

}


#' Plot annual spore totals in Stockholm
#'
#' Plot annual spore totals in Stockholm.
#'
#' @param data A data frame with station and species names, integer year, Date-valued date, and numeric conc columns.
#' @param start_year First included year; NA uses end_year minus 20.
#' @param end_year Reference year for the default start year and axis breaks. Observations after this year are not removed.
#' @returns A ggplot object with separate panels for Alternaria and Cladosporium.
#' @export
plot_year_totals_spores <- function(data, start_year = NA, end_year = 2024){
  if (is.na(start_year))
    start_year <- end_year - 20
  data |> filter(species %in% c("Alternaria", "Cladosporium"), year >= start_year, station == "Stockholm") |>
    summarise(conc = sum(conc, na.rm = TRUE), .by = c("species", "year")) |>
    ggplot(aes(x = year, y = conc)) + geom_col(width = 1, fill = "steelblue", color = NA) +
    facet_wrap(~species, ncol = 2, scales = "free_y") + theme_bw() +
    scale_y_continuous(expand = expansion(c(0, .05)), labels = scales::label_number()) +
    scale_x_continuous(breaks = seq(start_year + ((start_year %% 2) != (end_year %% 2)), end_year, by = 2)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    labs(x = "", y = "")+
    theme(plot.margin = margin(t = 4, r = 4, b = -10, l = 4, unit = "pt"))
}
