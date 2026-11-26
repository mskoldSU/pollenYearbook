#' Refresh local daily concentrations from SQLite
#'
#' Rebuild daily reporting concentrations without changing or reinstalling the
#' package. Supporting tables remain bundled with pollenYearbook.
#'
#' @param database Path to an existing SQLite database containing the data,
#'   pollen, station, season, and teknisktfel tables used by the reporting system.
#' @param output File in which to save daily concentrations, defaulting to
#'   daily_conc.rds in the working directory. Its parent directory must exist.
#' @returns Invisibly, the daily concentration data frame saved to output.
#' @details The database is opened read-only and disconnected on exit. Existing
#'   output is overwritten after all transformations finish. In-season dates
#'   without counts become zero; technical-error flags are retained.
#'   Cladosporium conversion factors are 26 through 2011 and 12 thereafter.
#'   Load the saved file with readRDS() in the reporting template.
#' @export
#' @examples
#' \dontrun{
#' update_data("current.sqlite")
#' daily_conc <- readRDS("daily_conc.rds")
#' }
update_data <- function(database, output = "daily_conc.rds") {
  if (!is.character(database) || length(database) != 1L || is.na(database) ||
      !file.exists(database) || dir.exists(database))
    stop("database must be the path to an existing SQLite file.")
  database <- normalizePath(database, mustWork = TRUE)
  if (!is.character(output) || length(output) != 1L || is.na(output) ||
      !nzchar(output) || dir.exists(output) || !dir.exists(dirname(output)))
    stop("output must be a file path in an existing directory.")
  if (normalizePath(output, mustWork = FALSE) == database)
    stop("output must not overwrite the input database.")
  utils::data("stations", package = "pollenYearbook", envir = environment())
  con <- DBI::dbConnect(RSQLite::SQLite(), database, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  required <- list(
    data = c("id", "date", "art_id", "station_id", "data12", "factor"),
    pollen = c("id", "lat_name", "swe_name"),
    station = c("id", "name"),
    season = c("station_id", "start", "end"),
    teknisktfel = c("id", "date", "station_id")
  )
  for (table in names(required)) {
    if (!DBI::dbExistsTable(con, table))
      stop("Database is missing table: ", table)
    missing <- setdiff(required[[table]], DBI::dbListFields(con, table))
    if (length(missing))
      stop("Table ", table, " is missing columns: ", paste(missing, collapse = ", "))
  }
  data <- DBI::dbGetQuery(con, "SELECT * FROM data") |>
    select(-id) |>
    distinct()
  pollen <- DBI::dbGetQuery(con, "SELECT * FROM pollen") |> mutate(lat_name = str_replace(lat_name, "Alternaria ", "Alternaria"))
  db_stations <- DBI::dbGetQuery(con, "SELECT * FROM station")

  season <- DBI::dbGetQuery(con, "SELECT * FROM season") |>
    left_join(db_stations |> select(id, station = name), by = c("station_id" = "id")) |>
    mutate(start = as.Date(start), end = as.Date(end), #station = str_remove(station, "-"),
           year = year(start)) |>
    select(year, station, station_id, season_start = start, season_end = end)
  error <- DBI::dbGetQuery(con, "SELECT * FROM teknisktfel") |>
    mutate(date = as.Date(date)) |>
    left_join(db_stations |> select(id, station = name), by = c("station_id" = "id")) |>
    select(-id)

  station_dates <- season |> summarise(station_start = min(season_start), station_end = max(season_end), .by = c("station", "station_id")) |>
    rowwise() |>
    mutate(dates = list(tibble(date = seq(station_start, station_end, by = 1)))) |>
    tidyr::unnest(dates) |>
    select(station, station_id, date)

  season_dates <- season |> rowwise() |>
    mutate(dates = list(tibble(date = seq(season_start, season_end, by = 1)))) |>
    tidyr::unnest(dates) |> select(-season_start, -season_end, -year) |>
    mutate(in_season = TRUE)

  all_dates <- station_dates |> left_join(season_dates, by = c("station" = "station", "date" = "date", "station_id")) |>
    mutate(in_season = ifelse(is.na(in_season), FALSE, in_season)) |>
    left_join(error |> mutate(is_error = TRUE) |> select(date, station, is_error), by = c("station" = "station", "date" = "date")) |>
    mutate(is_error = ifelse(is.na(is_error), FALSE, is_error))

  all_counts <- data |> left_join(pollen |> select(id, lat_name), by = c("art_id" = "id")) |>
    mutate(date = as.Date(date)) |>
    select(date, species = lat_name, station_id, dplyr::starts_with("data"), factor)

  daily_conc <- purrr::map_dfr(c("Alnus", "Betula", "Ulmus", "Fagus", "Quercus", "Corylus", "Salix",
                        "Artemisia", "Ambrosia", "Poaceae", "Pinus", "Picea", "Fraxinus", "Urtica",
                        "Alternaria", "Cladosporium", "Taxus", "Juniperus", "Populus", "Epicoccum"),
                      ~mutate(all_dates, species = .x)) |>
    left_join(all_counts, by = c("date" = "date", "station_id" = "station_id", "species" = "species")) |>
    left_join(stations, by = "station") |>
    filter(in_season == TRUE, station %in% stations$station) |>
    mutate(year = year(date),
           count = case_when(#is_error ~ NA,
                             !is.na(data12) ~ data12 / factor,
                             in_season  ~ 0,
                             TRUE ~ NA),
           factor = case_when(!(species == "Cladosporium") ~ factor,
                              year > 2011 ~ 12,
                              TRUE ~ 26),
           conc = ifelse(count == 0, 0, count * factor),
           station = fct_reorder(station, lat)) |>
    left_join(pollen, by = c("species" = "lat_name")) |>
    select(station, year, lat, long, date, species, swe_name, conc, is_error) |>
    filter(!((station == "Gävle") & (year == 2026))) |>
    mutate(station = forcats::fct_collapse(station, Gävle = c("Gävle Högskola 2", "Gävle")))



  saveRDS(daily_conc, file = output, compress = "bzip2", version = 2)
  invisible(daily_conc)
}
