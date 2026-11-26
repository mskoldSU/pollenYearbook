Sys.setlocale('LC_CTYPE', 'Swedish_Sweden.utf8')
work <- tempfile('refresh-test-')
dir.create(work)
pkg <- file.path(work, 'pollenYearbook')
dir.create(pkg)
stopifnot(all(file.copy(c('DESCRIPTION', 'NAMESPACE', 'R', 'man', 'LICENSE'), pkg, recursive = TRUE)))
lib <- file.path(work, 'library')
dir.create(lib)
dir.create(file.path(pkg, 'data'))
stopifnot(all(file.copy(file.path('data', c('stations.rda','pollen.rda','swe_month.rda','levels_tab.rda')), file.path(pkg,'data'))))
r <- file.path(R.home('bin'), 'R.exe')
Sys.setenv(LC_ALL=Sys.getlocale('LC_CTYPE'))
stopifnot(system2(r, c('CMD','INSTALL',shQuote(paste0('--library=',lib)),shQuote(pkg)))==0L)
db <- file.path(work, 'input.sqlite')
con <- DBI::dbConnect(RSQLite::SQLite(), db)
DBI::dbWriteTable(con, 'station', data.frame(id=1L, name='Stockholm'))
DBI::dbWriteTable(con, 'season', data.frame(station_id=1L, start='2025-06-01', end='2025-06-03'))
DBI::dbWriteTable(con, 'pollen', data.frame(id=1:2, lat_name=c('Betula','Cladosporium'), swe_name=c('Björk','Cladosporium')))
DBI::dbWriteTable(con, 'data', data.frame(id=1:2, date=c('2025-06-01','2025-06-01'), art_id=1:2, station_id=1L, data12=c(20,52), factor=c(2,26)))
DBI::dbWriteTable(con, 'teknisktfel', data.frame(id=1L, date='2025-06-02', station_id=1L))
DBI::dbDisconnect(con)
before <- unname(tools::md5sum(db))
output <- file.path(work, 'daily_conc.rds')
# Exercise the installed function, without a source checkout as working directory.
ns <- loadNamespace('pollenYearbook', lib.loc=lib)
installed_update <- getExportedValue('pollenYearbook', 'update_data')
old <- setwd(work)
result <- installed_update(db)
setwd(old)
stopifnot(is.data.frame(result), nrow(result)==60L,
          identical(result, readRDS(output)),
          result$conc[result$species=='Betula' & result$date==as.Date('2025-06-01')]==20,
          result$conc[result$species=='Cladosporium' & result$date==as.Date('2025-06-01')]==24,
          all(result$is_error[result$date==as.Date('2025-06-02')]),
          all(result$conc[result$date==as.Date('2025-06-03')]==0),
          identical(before, unname(tools::md5sum(db))),
          is.factor(result$station))
for (name in c('stations','pollen','swe_month','levels_tab')) {
  e <- new.env()
  utils::data(list=name, package='pollenYearbook', lib.loc=lib, envir=e)
  stopifnot(exists(name, envir=e, inherits=FALSE))
}
stopifnot(!file.exists(file.path(pkg,'data','daily_conc.rda')))
result2 <- installed_update(db, output=output)
stopifnot(identical(result,result2), identical(readRDS(output),result))
bad <- tryCatch(installed_update(file.path(work,'missing.sqlite'), output=output), error=identity)
stopifnot(inherits(bad,'error'), !file.exists(file.path(work,'missing.sqlite')))
bad <- tryCatch(installed_update(db, output=db), error=identity)
stopifnot(inherits(bad,'error'), identical(before, unname(tools::md5sum(db))))
cat('PASS: local RDS round trip, bundled tables, conversion, zero fill, error flags, database preservation, and input validation.\n')
unlink(work, recursive=TRUE)
