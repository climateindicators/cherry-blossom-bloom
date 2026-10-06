# Regression checks on the generated data for the Community Connection: Cherry Blossom Bloom Dates in Washington, D.C. indicator.
#
#   Rscript tests/test-data.R
#
# The checks below are shape-independent: they hold whatever the reshape in
# R/build_data.R turns each figure into. Value snapshots, which pin the actual
# numbers so a data update fails loudly instead of passing silently, are the
# TODO at the bottom.

setwd(here::here())
source("R/utils/write_stable.R")

# Keeps the dictionary check readable when a meta.yml field is absent altogether.
`%||%` <- function(a, b) if (is.null(a)) b else a

failures <- character()
check <- function(label, ok) {
  ok <- isTRUE(ok)
  cat(sprintf("  [%s] %s\n", if (ok) "PASS" else "FAIL", label))
  if (!ok) failures <<- c(failures, label)
  invisible(ok)
}

rd <- function(f) {
  readr::read_csv(file.path("data", f),
                  col_types = readr::cols(.default = readr::col_character()),
                  na = character(), progress = FALSE)
}

meta <- yaml::read_yaml("data/meta.yml")

cat("\nData dictionary\n")
check("meta.yml documents 1 dataset(s)", length(meta$datasets) == 1L)
check("meta.yml has no timestamp",
      !any(grepl("\\d{4}-\\d{2}-\\d{2}T|Sys\\.time|generated_at",
                 readLines("data/meta.yml", warn = FALSE))))

for (ds in meta$datasets) {
  df   <- rd(ds$file)
  cols <- vapply(ds$columns, function(x) x$name, character(1))
  check(sprintf("%s: meta.yml lists the columns the file actually has", ds$file),
        identical(cols, names(df)))
  check(sprintf("%s: meta.yml row count matches the file", ds$file),
        identical(as.integer(ds$rows), nrow(df)))
  check(sprintf("%s: every column has a type and a description", ds$file),
        all(vapply(ds$columns, function(x) nzchar(x$type %||% "") && nzchar(x$description %||% ""), logical(1))))
  check(sprintf("%s: source file is still present and unchanged", ds$file),
        identical(file_sha256(file.path("data-raw", ds$source_file)), ds$source_sha256))
  check(sprintf("%s: no blank rows", ds$file), nrow(df) > 0L)
}

cat("\nFile hygiene\n")
for (f in list.files("data", pattern = "[.](csv|yml)$", full.names = TRUE)) {
  check(sprintf("%s is UTF-8, LF, no BOM, no mojibake", basename(f)),
        tryCatch({ assert_clean_output(f); TRUE },
                 error = function(e) { cat("      ", conditionMessage(e), "\n"); FALSE }))
}

cat("\nValue snapshots\n")
d <- rd("cherry_blossom_bloom.csv")
check("cherry_blossom_bloom.csv: 276 rows", nrow(d) == 276L)

# Each series: row count, first and last observation, and the value range.
# Values are compared as strings; the source carries whole numbers of days.
series_snapshot <- function(key, n, first, last, vmin, vmax) {
  s <- d[d$series_key == key, ]
  check(sprintf("%s: %d rows", key, n), nrow(s) == n)
  check(sprintf("%s: first row is %s = %s", key, first[1], first[2]),
        identical(c(s$year[1], s$value[1]), first))
  check(sprintf("%s: last row is %s = %s", key, last[1], last[2]),
        identical(c(s$year[nrow(s)], s$value[nrow(s)]), last))
  v <- as.integer(s$value)
  check(sprintf("%s: min %d, max %d", key, vmin, vmax), min(v) == vmin && max(v) == vmax)
}

series_snapshot("peak_bloom",        104L, c("1921", "79"),  c("2024", "77"), 74L, 108L)
series_snapshot("festival_start",     86L, c("1934", "109"), c("2024", "80"), 77L, 109L)
series_snapshot("festival_duration",  86L, c("1934", "2"),   c("2024", "25"),  1L,  38L)

check("series are peak_bloom, festival_start, festival_duration",
      setequal(unique(d$series_key), c("peak_bloom", "festival_start", "festival_duration")))
check("festival series omit 1942-1946 (not held)",
      !any(d$series_key != "peak_bloom" & d$year %in% as.character(1942:1946)))
check("peak_bloom 1990 is 74 and 1958 is 108",
      identical(d$value[d$series_key == "peak_bloom" & d$year == "1990"], "74") &&
      identical(d$value[d$series_key == "peak_bloom" & d$year == "1958"], "108"))
check("festival_duration 2012 is 38",
      identical(d$value[d$series_key == "festival_duration" & d$year == "2012"], "38"))

cat("\n")
if (length(failures)) {
  cat(sprintf("%d FAILED:\n", length(failures)))
  for (f in failures) cat("  -", f, "\n")
  quit(status = 1L)
}
cat("All data checks passed.\n")
