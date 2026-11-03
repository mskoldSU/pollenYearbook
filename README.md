# pollenYearbook

R functions for the Palynological Lab's annual pollen and fungal spore reports. The package prepares monitoring data, summarizes seasons, and creates plots and tables with Swedish labels.

## Status

Development version (0.1.0). Report datasets are generated locally from a compatible SQLite database and are excluded from Git. Installing the package alone does not provide the data needed for reporting.

## Installation

Requires R 4.1.0 or later. Clone the repository so that you have a source directory in which to save updated datasets:

```sh
git clone https://github.com/mskoldSU/pollenYearbook.git
cd pollenYearbook
```

From R, with the cloned directory as your working directory:

```r
install.packages("remotes") # Once, if needed
remotes::install_local(".", dependencies = NA, upgrade = "never")
```

## Refresh the report data

Supply an existing SQLite database to `update_data()`. The database must contain the reporting system's `data`, `pollen`, `station`, `season`, and `teknisktfel` tables. An arbitrary SQLite database will not have the required schema.

```r
library(pollenYearbook)

update_data(
  database = "path/to/current.sqlite",
  package_dir = "."
)
```

`package_dir` must point to the cloned source directory. The function opens the database read-only, rebuilds the datasets in `data/`, and reinstalls the package into the first R library. Existing datasets with the same names are overwritten. Restart R after reinstalling to use the refreshed data.

To select a different, existing writable library:

```r
update_data("path/to/current.sqlite", package_dir = ".", lib = "path/to/R/library")
```

To generate the datasets without reinstalling:

```r
datasets <- update_data(
  "path/to/current.sqlite",
  package_dir = ".",
  reinstall = FALSE
)
```

The return value is a named list of the five generated datasets:

| Dataset | Contents |
| --- | --- |
| `daily_conc` | Daily concentrations by station and taxon, with dates, coordinates, Swedish names, and technical-error flags |
| `stations` | Reporting stations and coordinates |
| `pollen` | Taxon metadata from the database |
| `swe_month` | Swedish month abbreviations |
| `levels_tab` | Formatted concentration-category table |

The refresh retains technical-error flags, fills in-season dates without counts with zero, and applies the conversion factors from the original reporting workflow.

## Reporting examples

After generating the datasets, reinstalling, and restarting R:

```r
library(pollenYearbook)
data("daily_conc", package = "pollenYearbook")

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

Additional functions provide daily tree, grass, and fungal spore time series, annual spore totals, and Swedish date formatting. Time-series functions use the locally generated package datasets.

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

Generated datasets, database files, and report outputs are excluded from version control. Keep database inputs locally to reproduce a refresh.

## License

This package is licensed under the [MIT License](LICENSE.md).
