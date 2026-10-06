# Build tidy long-format data for the Community Connection: Cherry Blossom Bloom Dates in Washington, D.C. indicator.
#
#   Rscript R/build_data.R
#
# Reads EPA's published figure CSVs in data-raw/ and writes data/*.csv plus
# data/meta.yml. Rerunning with unchanged inputs produces byte-identical output.
# Nothing here touches the network.
#
# Generated as a stub by the build-indicator skill. Each figure below has a
# `todo_reshape()` call standing where its reshape belongs. Replace that line,
# then delete the todo_reshape() helper once no call to it remains.
#
# TO UPDATE THE DATA: drop replacement CSVs into data-raw/ and rerun. Headers
# are asserted, not assumed, so a renamed or reordered column stops the build.

suppressPackageStartupMessages({
  library(dplyr)
})

root <- here::here()
source(file.path(root, "R/utils/epa_csv.R"))
source(file.path(root, "R/utils/write_stable.R"))

raw_dir <- file.path(root, "data-raw")
out_dir <- file.path(root, "data")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Indicator constants -----------------------------------------------------

INDICATOR <- list(
  name                    = "Community Connection: Cherry Blossom Bloom Dates in Washington, D.C.",
  slug                    = "cherry-blossom-bloom",
  publisher               = "U.S. Environmental Protection Agency",
  source_page             = "https://19january2025snapshot.epa.gov/climate-indicators/cherry-blossoms/index.html",
  technical_documentation = "https://19january2025snapshot.epa.gov/system/files/documents/2024-05/cherry-blossoms_documentation.pdf",
  rights                  = "Public domain, work of the U.S. Government (17 U.S.C. 105)"
)

# ---- Series lookup table ------------------------------------------------------
#
# `source_header` must match the CSV header byte for byte. `label` is the
# display string used in the narrative prose.

FIG1_SERIES <- tibble::tribble(
  ~source_header,                       ~series_key,         ~label,                              ~unit,
  "Yoshino peak bloom date",            "peak_bloom",        "Yoshino peak bloom date",           "days from January 1",
  "Cherry blossom festival start date", "festival_start",    "Cherry blossom festival start date", "days from January 1",
  "Cherry blossom festival duration",   "festival_duration", "Cherry blossom festival duration",  "days"
)

# ---- Figure 1: Peak Bloom Date for Cherry Trees Around Washington, D.C.'s Tidal Basin, 1921-2024 ----

f1_path <- file.path(raw_dir, "cherry-blossoms_fig-1.csv")
f1_meta <- read_epa_preamble(f1_path)
f1_raw  <- read_epa_csv(f1_path)

# All columns are listed as expected headers so a rename or reorder stops the
# build from the first run.
assert_headers(
  f1_raw,
  id_cols          = "Year",
  expected_headers = FIG1_SERIES$source_header,
  what             = "cherry-blossoms_fig-1.csv"
)

# The source's Units line carries one unit per series, so the unit travels as a
# column of the output rather than in meta.yml's single-valued `unit:` field.
# A blank source cell is OMITTED rather than written as an empty value: the
# festival columns are blank before 1934 and in the war years it was not held,
# and an empty `value` would invite a reader to treat those years as a zero.
f1 <- f1_raw |>
  tidyr::pivot_longer(
    cols      = -"Year",
    names_to  = "source_header",
    values_to = "value"
  ) |>
  dplyr::filter(trimws(value) != "") |>
  dplyr::inner_join(FIG1_SERIES, by = "source_header") |>
  dplyr::mutate(year = as.integer(Year)) |>
  dplyr::arrange(year, match(series_key, FIG1_SERIES$series_key)) |>
  dplyr::select(year, series_key, series_label = label, unit, value)

stopifnot(!anyNA(suppressWarnings(as.numeric(f1$value))))
assert_conservation(f1_raw, FIG1_SERIES$source_header, nrow(f1), "figure 1")

write_csv_stable(f1, file.path(out_dir, "cherry_blossom_bloom.csv"))

# ---- Data dictionary ---------------------------------------------------------

col <- function(name, type, description) {
  list(name = name, type = type, description = description)
}

meta <- list(
  indicator = INDICATOR,
  datasets = list(
    list(
      file            = "cherry_blossom_bloom.csv",
      figure          = "Figure 1",
      figure_title    = f1_meta$title,
      source_file     = "cherry-blossoms_fig-1.csv",
      source_sha256   = file_sha256(f1_path),
      source_encoding = "UTF-8",
      data_source     = f1_meta$data_source,
      web_update      = f1_meta$web_update,
      unit            = f1_meta$units,
      rows            = nrow(f1),
      columns = list(
        col("year", "integer", "Calendar year"),
        col("series_key", "string", "Machine-readable series identifier"),
        col("series_label", "string", "Display label, as used in EPA's published source file"),
        col("unit", "string", "Unit of `value`"),
        col("value", "number", "Days from January 1 (peak bloom, festival start) or days (festival duration), verbatim from the source file.")
      ),
      series = list(
        list(
          key           = "peak_bloom",
          label         = FIG1_SERIES$label[FIG1_SERIES$series_key == "peak_bloom"],
          source_header = FIG1_SERIES$source_header[FIG1_SERIES$series_key == "peak_bloom"],
          coverage      = "1921-2024",
          note          = "Peak bloom date of the Yoshino cherry trees around the Tidal Basin, when 70 percent of blossoms are in full bloom."
        ),
        list(
          key           = "festival_start",
          label         = FIG1_SERIES$label[FIG1_SERIES$series_key == "festival_start"],
          source_header = FIG1_SERIES$source_header[FIG1_SERIES$series_key == "festival_start"],
          coverage      = "1934-2024, excluding 1942-1946",
          note          = "Blank in the source file outside this coverage; those years are omitted from this file rather than written as empty values."
        ),
        list(
          key           = "festival_duration",
          label         = FIG1_SERIES$label[FIG1_SERIES$series_key == "festival_duration"],
          source_header = FIG1_SERIES$source_header[FIG1_SERIES$series_key == "festival_duration"],
          coverage      = "1934-2024, excluding 1942-1946",
          note          = "Blank in the source file outside this coverage; those years are omitted from this file rather than written as empty values."
        )
      )
    )
  )
)

write_yaml_stable(meta, file.path(out_dir, "meta.yml"))

# ---- Verify what was written -------------------------------------------------

written <- list.files(out_dir, pattern = "[.](csv|yml)$", full.names = TRUE)
invisible(lapply(written, assert_clean_output))

cat("\nWrote:\n")
for (p in written) {
  cat(sprintf("  %-34s %8d bytes  %s\n", basename(p), file.size(p), substr(file_sha256(p), 1, 12)))
}
