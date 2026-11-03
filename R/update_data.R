#' Refresh and reinstall annual report datasets from SQLite
#'
#' Rebuild the five bundled reporting datasets using a SQLite database and
#' optionally reinstall the source package into an R library.
#'
#' @param database Path to an existing SQLite database containing the data,
#'   pollen, station, season, and teknisktfel tables used by the reporting system.
#' @param package_dir Path to the pollenYearbook source package. Defaults to the
#'   current directory; an installed package directory is not a source checkout.
#' @param lib Destination library for installation. Defaults to the first R
#'   library. It must already exist and be writable when reinstall is TRUE.
#' @param reinstall Whether to reinstall after saving the datasets. Set FALSE
#'   to refresh source data without installing.
#' @returns Invisibly, a named list containing stations, daily_conc, pollen,
#'   swe_month, and levels_tab.
#' @details The database is opened read-only and disconnected on exit. All
#'   transformations finish before datasets are saved to package_dir/data.
#'   Existing datasets of those names are overwritten; other datasets are kept.
#'   In-season dates without counts become zero, while technical-error flags
#'   are retained. Cladosporium conversion factors are 26 through 2011 and 12
#'   thereafter, following the original refresh script.
#'
#'   Installation uses a temporary copy of package sources, excluding report
#'   outputs and database dumps. An installation failure raises an error; the
#'   refreshed source datasets remain available for retrying. Restart R after
#'   reinstalling to use the refreshed data in an already loaded package.
#' @export
#' @examples
#' \dontrun{
#' update_data("current.sqlite", package_dir = ".")
#' }
update_data <- function(database, package_dir = ".", lib = .libPaths()[1L],
                        reinstall = TRUE) {
  if (!is.character(database) || length(database) != 1L || is.na(database) ||
      !file.exists(database) || dir.exists(database))
    stop("database must be the path to an existing SQLite file.")
  database <- normalizePath(database, mustWork = TRUE)
  package_dir <- normalizePath(package_dir, mustWork = TRUE)
  description <- file.path(package_dir, "DESCRIPTION")
  if (!file.exists(description) ||
      read.dcf(description, fields = "Package")[1L] != "pollenYearbook" ||
      !dir.exists(file.path(package_dir, "R")))
    stop("package_dir must contain the pollenYearbook source package.")
  if (!is.logical(reinstall) || length(reinstall) != 1L || is.na(reinstall))
    stop("reinstall must be TRUE or FALSE.")
  if (reinstall) {
    lib <- normalizePath(lib, mustWork = TRUE)
    if (length(lib) != 1L || !dir.exists(lib) || file.access(lib, 2L) != 0L)
      stop("lib must be an existing writable R library.")
  }
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
  stations <- tibble(station = c("Abisko", "Borlänge", "Bräkne-Hoby",
                                 "Eskilstuna", "Forshaga", "Gävle", "Göteborg", "Hässleholm",
                                 "Jönköping", "Kiruna", "Kristianstad", "Ljusdal", "Malmö",
                                 "Norrköping", "Nässjö", "Piteå", "Skövde", "Stockholm",
                                 "Sundsvall", "Umeå", "Visby", "Västervik", "Östersund",
                                 "Göteborg Östra", "Storuman", "Gävle Högskola 2"),
                     lat = c(68.3501695,
                             60.4856426, 56.2285608, 59.3717379, 59.533333, 60.6750132, 57.7072326,
                             56.157986, 57.7825634, 67.8480291461374, 56.0293778, 61.8294634,
                             55.6052931, 58.5909124, 57.6530377, 65.3134652, 58.3896329, 59.3632946,
                             62.3907552, 63.8256568, 57.6379361, 57.7594186, 63.1793655, 57.6856, 65.088603, 60.66748),
                     long = c(18.8300059, 15.4234561, 15.1169407, 16.5051474,
                              13.466667, 17.1467019, 11.9670171, 13.7653328, 14.165719,
                              20.3093695730431, 14.1566859, 16.071681, 13.0001566, 16.1903511,
                              14.6981554, 21.4900252, 13.8456344, 18.0560582, 17.3071024,
                              20.2630745, 18.297982, 16.6385035, 14.6357061, 11.9572, 17.128920, 17.11294)) |>
    mutate(station = fct_reorder(station, lat))



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
    select(station, year, lat, long, date, species, swe_name, conc, is_error)



  swe_month <- c("Jan", "Feb", "Mar", "Apr", "Maj", "Jun", "Jul", "Aug", "Sep",
                 "Okt", "Nov", "Dec") |> tolower()



  df <- tibble(
    Grupp = c(
      rep("Träd", 7),
      rep("Gräs och örter", 3),
      rep("Mögelsporer", 3)
    ),
    Taxon = c(
      "Al (Alnus)",
      "Alm (Ulmus)",
      "Björk (Betula)",
      "Bok (Fagus)",
      "Ek (Quercus)",
      "Hassel (Corylus)",
      "Viden (Salix)",
      "Gråbo (Artemisia)",
      "Gräs (Poaceae)",
      "Malörtsambrosia (Ambrosia)",
      "Alternaria",
      "Epicoccum",
      "Cladosporium"
    ),
    Låga = c(rep("1–10", 7), rep("1–10", 3), "1–20","1–20", "1–2000"),
    Måttliga = c(rep("11–100", 7), rep("11–30", 3), "21–100", "21–100", "2001–4000"),
    Höga = c(rep("101–1000", 7), "31–100", "31–80", "31–100", "101–1000","101–1000", "4001–10000"),
    `Mycket höga` = c(rep(">1000", 7), rep(">100", 3), ">1000", ">1000", ">10000")
  )

  df2 <- df |>
    dplyr::group_by(Grupp) |>
    dplyr::group_modify(~ bind_rows(
      tibble(
        Taxon = unique(.y$Grupp),
        Låga = "", Måttliga = "", Höga = "", `Mycket höga` = "",
        is_group = TRUE
      ),
      mutate(.x, is_group = FALSE)
    )) |>
    ungroup() |>
    mutate(n = dplyr::n(), .by = "Grupp") |>
    arrange(-n)


  ft <- flextable::flextable(df2 |> select(-Grupp, -is_group, -n)) |>
    set_header_labels(
      Taxon = "Halter:",
      Låga = "låga",
      Måttliga = "måttliga",
      Höga = "höga",
      `Mycket höga` = "mycket höga"
    ) |>
    flextable::theme_booktabs() |>
    autofit()

  group_rows <- which(df2$is_group)

  ft <- purrr::reduce(
    group_rows,
    .init = ft,
    .f = ~ flextable::merge_at(.x, i = .y, j = 1:5)
  ) |>
    flextable::bold(i = group_rows, bold = TRUE) |>
    flextable::align(i = group_rows, align = "left", part = "body")

  levels_tab <- ft


  datasets <- list(stations = stations, daily_conc = daily_conc, pollen = pollen,
                   swe_month = swe_month, levels_tab = levels_tab)
  data_dir <- file.path(package_dir, "data")
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  for (name in names(datasets)) {
    save(list = name, envir = environment(),
         file = file.path(data_dir, paste0(name, ".rda")), compress = "bzip2",
         version = 2)
  }
  if (reinstall) {
    staging <- tempfile("pollenYearbook-install-")
    dir.create(staging)
    on.exit(unlink(staging, recursive = TRUE), add = TRUE)
    source <- file.path(staging, "pollenYearbook")
    dir.create(source)
    entries <- c("DESCRIPTION", "NAMESPACE", "R", "man", "data", "inst",
                 "src", "configure", "configure.win", "cleanup", "cleanup.win",
                 "LICENSE", "LICENCE", "LICENSE.md")
    paths <- file.path(package_dir, entries)
    paths <- paths[file.exists(paths)]
    if (!all(file.copy(paths, source, recursive = TRUE)))
      stop("Could not stage the source package for installation.")
    r <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R")
    # Pass the working locale to R CMD INSTALL; inherited Unix locale names
    # may be invalid on Windows and prevent parsing Swedish identifiers.
    if (.Platform$OS.type == "windows") {
      old_locale <- Sys.getenv("LC_ALL", unset = NA_character_)
      on.exit({
        if (is.na(old_locale)) Sys.unsetenv("LC_ALL")
        else Sys.setenv(LC_ALL = old_locale)
      }, add = TRUE)
      Sys.setenv(LC_ALL = Sys.getlocale("LC_CTYPE"))
    }
    status <- system2(r, c("CMD", "INSTALL", "--no-multiarch",
                          shQuote(paste0("--library=", lib)), shQuote(source)))
    if (status != 0L)
      stop("Package installation failed (status ", status,
           "). Refreshed datasets are saved in ", data_dir, ".")
  }
  invisible(datasets)
}
