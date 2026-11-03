#' Compare pollen seasons with the preceding year
#'
#' Compare pollen seasons with the preceding year.
#'
#' @param data A data frame with station and species names, integer year, Date-valued date, and numeric conc columns.
#' @param species A single taxon name matching the species column.
#' @param report_year The reporting year as an integer.
#' @param cutoff Fraction of cumulative annual concentration excluded at each end of the season; defaults to 0.025.
#' @returns A flextable object with Swedish headings and season sparklines.
#' @details Requires an additional logical is_error column. Sparklines show square roots of centered five-day mean concentrations, with errors replaced by missing values. Monitoring start and end columns depend on the taxon.
#' @export
table_seasons <- function(data, species, report_year, cutoff = .025){
  this_year <- get_seasons(data, species, report_year, cutoff) |> mutate(year = year(start))
  last_year <- get_seasons(data, species, report_year - 1, cutoff)  |> mutate(year = year(start))

  first_start <- round(quantile(yday(c(this_year$season_start, last_year$season_start)), .25) * .8)
  last_end <- round(quantile(yday(c(this_year$season_end, last_year$season_end)), .75) * 1.2)

  table_years <- c(report_year -1, report_year)

  table_species <- species

  ts_data <- data |> filter(species == table_species,
                            year %in% table_years,
                            yday(date) %in% first_start:last_end) |>
    mutate(conc = ifelse(is_error == TRUE, NA, conc)) |>
    select(station, date, conc) |>
    complete(station, date) |>
    mutate(year = year(date) |> as.character()) |>
    summarise(conc_ts = list(sqrt(slider::slide_dbl(conc, .f = mean, .before = 2, .after = 2, .complete = TRUE))), .by = c("station", "year"))

  table_data <- bind_rows(this_year, last_year) |>
    arrange(station, -year) |>
    mutate(
      year = as.character(year),
      season = short_date_seq(season_start, season_end),
      date_max = short_date(date_max),
      max_count = year_max,
      total = total,
      start = short_date(start),
      end = short_date(end)
    ) |>
    left_join(ts_data, by = c("station", "year")) |>
    select("Mätstation" = station, "Mätår" = year, "Mätstart" =  start, "Mätslut" = end,
           "Pollensäsong" = season, "Datum för säsongsmaximum" = date_max,
           "Säsongens maximiantal" = max_count, "Årssumma" = total, conc_ts)
  if (table_species %in% c("Alnus", "Corylus"))
    table_data <- select(table_data, -Mätslut)

  if (table_species %in% c("Poaceae", "Artemisia", "Ambrosia"))
    table_data <- select(table_data, -Mätstart)

  if (!(table_species %in% c("Alnus", "Corylus", "Poaceae", "Artemisia", "Ambrosia")))
    table_data <- select(table_data, -Mätstart, -Mätslut)

  hline_rows <- table_data |> count(Mätstation) |> pull(n) |> cumsum()

  last_year_rows <- which(table_data$Mätår == as.character(report_year - 1))

  table_data |>
    qflextable() |>
    mk_par(j = "conc_ts", value = as_paragraph(plot_chunk(value = conc_ts, type = "line", col = "red"))) |>
    hline(i = hline_rows) |>
    set_header_labels(conc_ts = "Säsongsprofil") |>
    colformat_num(
      big.mark = " ", decimal.mark = ",",
      na_str = "") |>
    merge_v(j = ~Mätstation) |>  autofit() |>
    color(i = last_year_rows, color = "grey40") |>
    italic(i = last_year_rows) |>
    set_table_properties(layout = "autofit")  |>
    fontsize(size = 8, part = "all") |>
    padding(padding.top = 0, padding.bottom = 0)
}
