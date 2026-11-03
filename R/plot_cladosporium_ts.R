#' Plot daily Cladosporium concentrations in Stockholm
#'
#' Plot daily Cladosporium concentrations in Stockholm.
#'
#' @param report_year The reporting year as an integer.
#' @param start_date First plotted day as a month-day string (MM-DD).
#' @param end_date Last plotted day as a month-day string (MM-DD).
#' @param text_size Base plot text size in points.
#' @returns A ggplot object.
#' @details Uses bundled daily_conc and swe_month datasets. Daily bars and centered five-day means are plotted on a square-root scale; flagged errors are shaded and excluded from means.
#' @export
plot_cladosporium_ts <- function(report_year,
                                 start_date = "06-01",
                                 end_date = "09-30",
                                 text_size = 8){
  start_date <- paste0(report_year, "-", start_date) |> as.Date()
  end_date <- paste0(report_year, "-", end_date) |> as.Date()

  data <- daily_conc |> filter(date >= start_date - 2, date <= end_date + 2,
                               species == "Cladosporium", station == "Stockholm") |>
    mutate(conc = ifelse(is_error == TRUE, NA, conc),,
           roll = slider::slide_dbl(conc, mean, .before = 2, .after = 2, .complete = TRUE),
           .by = c("station", "species"))|>
    filter(date >= start_date, date <= end_date)

  seasons <- daily_conc |> filter(year == report_year, station == "Stockholm") |>
    select(date, station) |>


    distinct() |>
    summarise(season_start = min(date), season_end = max(date), .by = c("station")) |>
    mutate(facet_label = paste0("Cladosporium ", report_year, " Stockholm"))

  error <- data |> filter(is_error == TRUE) |>
    left_join(seasons, by = "station")

  dates <- tibble(date = seq(as.Date(start_date), as.Date(end_date), by = 1)) |>
    mutate(week = isoweek(date)) |>
    summarise(date = min(date), .by = week) |>
    mutate(day = day(date), month = swe_month[month(date)],
           label = paste0(str_pad(day, "0", width = 2, side = "left"), "-", month))

  date_breaks <- dates$date[-1]
  date_labels <- dates$label[-1]

  data |> left_join(seasons, by = "station") |>
    ggplot() +
    geom_col(data = error, aes(x = date, y = 150), fill = "lightgrey", alpha = .5, width = 1, color = NA) +
    geom_col(aes(y = sqrt(conc), x = date),  fill = "steelblue", width = .5, linewidth = 0, position = "dodge", alpha = .7, show.legend = FALSE) +
    facet_wrap(~facet_label, ncol = 3) +
    theme_bw(base_size = text_size) +
    geom_line(aes(x = date,  y = sqrt(roll)), color = "steelblue", linewidth = 1) +
    labs(x = "", y = "Sporhalt", color = NULL) +
    scale_y_continuous(breaks = sqrt(c(0, 2000, 4000, 10000)),
                       labels = c("0", "2000", "4000", "10000"), expand = expansion(c(0, NA))) +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line(color = "black", linewidth = .1),
          axis.text.x = element_text(angle = 90, hjust = .5, vjust = .5),
          # legend.position = "inside",
          # legend.position.inside = c(.9, .8),
          legend.background = element_rect(color = "black"),
          legend.key.size = unit(0.3, "cm")
    ) +
    geom_text(data = tibble(
      x = as.Date(start_date),
      y = c(22.5, 54, 82, 120),
      label = c("Låga halter", "Måttliga halter", "Höga halter", "Mycket höga halter")),
      aes(x = x, y = y, label = label), size = 3, hjust = 0, vjust = 0) +
    scale_x_date(breaks = date_breaks, labels = date_labels, expand = expansion(0, 0)) +
    coord_cartesian(ylim = c(0, 150))
}
