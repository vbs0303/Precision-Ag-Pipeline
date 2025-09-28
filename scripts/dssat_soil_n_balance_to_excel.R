# dssat_soil_n_balance_to_excel.R
# Parse DSSAT soil inorganic N balance report text files split by *RUN and export:
#  - One Excel per input file with one sheet per run
#  - A compact summary workbook
# Sample data: data/dssat_output/SiteA_SoilNiBalance.txt, SiteB_SoilNiBalance.txt

suppressPackageStartupMessages({
  library(openxlsx)
  library(stringr)
  library(purrr)
  library(dplyr)
  library(writexl)
  library(tools)
})

# -------- Inputs --------
input_txt_files <- c(
  "data/dssat_output/SiteA_SoilNiBalance.txt",
  "data/dssat_output/SiteB_SoilNiBalance.txt"
)
output_dir <- "outputs_soil_n"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# -------- Helpers --------
parse_runs <- function(file_path) {
  lines <- readLines(file_path, warn = FALSE)
  idx <- grep("\\*RUN", lines)
  idx <- c(idx, length(lines) + 1L)
  out <- lapply(seq_len(length(idx) - 1L), function(i) lines[idx[i]:(idx[i+1]-1L)])
  names(out) <- paste0("Run_", seq_along(out))
  out
}

section_to_df <- function(section_lines) {
  tab <- section_lines[grep("^\\!", section_lines)]
  tab <- gsub("^!\\s*", "", tab)
  split_rows <- strsplit(tab, "\\s{2,}")
  maxlen <- max(vapply(split_rows, length, 1L))
  mat <- t(vapply(split_rows, function(x) c(x, rep("", maxlen - length(x))), character(maxlen)))
  as.data.frame(mat, stringsAsFactors = FALSE)
}

extract_summary <- function(df, season_label) {
  get_val <- function(label, col = 2) {
    row <- which(str_detect(df[[1]], fixed(label)))
    if (length(row) > 0) suppressWarnings(as.numeric(df[row[1], col])) else NA_real_
  }
  tibble::tibble(
    Season            = season_label,
    Soil_NO3_Initial  = get_val("Soil NO3", 2),
    Soil_NH4_Initial  = get_val("Soil NH4", 2),
    Fertilizer_N      = get_val("Fertilizer N", 2),
    Mineralized_N     = get_val("Mineralized N", 2),
    Soil_NO3_Final    = get_val("Soil NO3", 3),
    Soil_NH4_Final    = get_val("Soil NH4", 3),
    Soil_N_gases      = get_val("Soil N gases", 3),
    N_uptake          = get_val("N Uptake From Soil", 2),
    N_immobilized     = get_val("N immobilized", 2),
    N_leached         = get_val("N leached", 2),
    NH3_loss          = get_val("NH3 loss", 2),
    N2O_loss          = get_val("N2O loss", 2),
    N2_loss           = get_val("N2 loss", 2),
    NO_loss           = get_val("NO loss", 2)
  )
}

# -------- Main --------
for (txt in input_txt_files) {
  runs <- parse_runs(txt)
  if (length(runs) == 0L) next
  base <- file_path_sans_ext(basename(txt))

  # Excel with one sheet per run
  wb <- createWorkbook()
  for (nm in names(runs)) {
    df <- section_to_df(runs[[nm]])
    addWorksheet(wb, nm)
    writeData(wb, nm, df)
  }
  out_xlsx <- file.path(output_dir, paste0(base, "_Per_Run.xlsx"))
  saveWorkbook(wb, out_xlsx, overwrite = TRUE)

  # Summary workbook
  season_labels <- paste0("Season_", seq_along(runs))
  sum_list <- purrr::imap(runs, function(sec, i) extract_summary(section_to_df(sec), season_labels[i]))
  summary_df <- dplyr::bind_rows(sum_list)
  out_sum <- file.path(output_dir, paste0(base, "_Summary.xlsx"))
  writexl::write_xlsx(summary_df, out_sum)

  cat("Wrote:\n", normalizePath(out_xlsx), "\n", normalizePath(out_sum), "\n", sep = "")
}

