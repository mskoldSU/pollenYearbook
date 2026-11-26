# pollenYearbook

R functions for the Palynological Lab's annual pollen and fungal spore reports. The package prepares monitoring data, summarizes seasons, and creates plots and tables with Swedish labels.

## Installation

Requires R 4.1.0 or later. Install code and bundled supporting tables directly from GitHub:

```r
install.packages("remotes") # Once, if needed
remotes::install_github("mskoldSU/pollenYearbook", upgrade = "never")
```

Run the same `install_github()` command to obtain later code and supporting-table
updates. Local daily concentrations are kept separately and do not need to be
regenerated after a code-only update. Restart R after updating a loaded package.

## Refresh the report data

`update_data()` reads a compatible SQLite database and saves daily concentrations
in a local RDS file. It does not require package sources or reinstall the package.
The database must contain `data`, `pollen`, `station`, `season`, and `teknisktfel`.

```r
library(pollenYearbook)
update_data("path/to/current.sqlite")
daily_conc <- readRDS("daily_conc.rds")
```

By default, the file is written to your current working directory. To choose
another existing directory:

```r
update_data("path/to/current.sqlite", output = "report-data/daily_conc.rds")
daily_conc <- readRDS("report-data/daily_conc.rds")
```

The output is overwritten on refresh. The function also invisibly returns the
daily concentration data frame. The database is opened read-only.

`stations`, `pollen`, `swe_month`, and `levels_tab` are bundled with the package.
Daily concentrations remain local and survive package updates. `template.Rmd`
reads `daily_conc.rds` in its setup chunk. Place the file beside the template
for the default workflow, or change the template's `readRDS()` path to match your
chosen output location.

| Data | Storage | Updated by |
| --- | --- | --- |
| `daily_conc` | Local `daily_conc.rds` file | `update_data()` |
| `stations`, `pollen`, `swe_month`, `levels_tab` | Bundled package datasets | Installing a package update |

`update_data()` retains technical-error flags, fills in-season dates without
counts with zero, and applies taxon-specific conversion factors. It uses the
input database's taxon metadata when building daily concentrations; the bundled
`pollen` table is not rewritten.

The previous `package_dir`, `lib`, and `reinstall` arguments have been removed.
Replace calls such as `update_data(database, package_dir = ".")` with
`update_data(database)`. The return value is now the daily concentration data
frame rather than a list of datasets.

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

Additional functions provide daily tree, grass, and fungal spore time series, annual spore totals, and Swedish date formatting. Load `daily_conc` with `readRDS()` before calling the daily time-series functions, as the template does.

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
