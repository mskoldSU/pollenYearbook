# pollenYearbook

R functions for the Palynological Lab's annual pollen and fungal spore reports. The package prepares monitoring data, summarizes seasons, and creates plots and tables with Swedish labels.

## Installation

Requires R 4.1.0 or later. Install code and bundled supporting tables directly from GitHub:

```r
remotes::install_github("mskoldSU/pollenYearbook", upgrade = "never")
```

## Refresh the report data

`update_data()` reads a compatible SQLite database and saves daily concentrations
in a local RDS file. It does not require package sources or reinstall the package.
The database must contain `data`, `pollen`, `station`, `season`, and `teknisktfel`.

```r
library(pollenYearbook)
update_data("path/to/current.sqlite")
daily_conc <- readRDS("daily_conc.rds")
```

Use `output = "path/to/daily_conc.rds"` to choose another existing directory.
The output is overwritten on refresh. The function also invisibly returns the
daily concentration data frame. The database is opened read-only.

`stations`, `pollen`, `swe_month`, and `levels_tab` are bundled with the package.
Daily concentrations remain local and survive package updates. `template.Rmd`
reads `daily_conc.rds` in its setup chunk.

## Reporting examples

After refreshing the local data:

```r
library(pollenYearbook)
daily_conc <- readRDS("daily_conc.rds")

# Season summaries for a single taxon and year
get_seasons(daily_conc, species = "Betula", year = 2025)

# Formatted comparison with the preceding year
table_seasons(daily_conc, species = "Betula", report_year = 2025)

# Monitoring periods and technical errors
table_errors(daily_conc, error_year = 2025)

# Annual totals by station
plot_year_totals(daily_conc, species = "Betula", end_year = 2025)
```

Use a year and taxon present in your database. Formatted tables are `flextable` objects; plotting functions return `ggplot` objects.

Additional functions provide daily tree, grass, and fungal spore time series, annual spore totals, and Swedish date formatting. Time-series functions are unchanged; their existing `daily_conc` lookup is retained.

For arguments and details, consult the R help pages:

```r
?update_data
?get_seasons
?plot_tree_ts
```

## Development notes

Source functions are in `R/`, generated help pages in `man/`, and installed image assets in `inst/`. Documentation uses roxygen2:

```r
roxygen2::roxygenise()
```

The four supporting datasets are versioned in `data/`. Daily concentrations, database files, and report outputs remain local. Keep database inputs to reproduce a refresh.

## License

This package is licensed under the [MIT License](LICENSE.md).
