# nitrogen_losses_stats_plots.R
# Linear / Mixed-model ready analysis for nitrogen loss metrics.
# Sample data: data/statistics/nitrogen_losses_example.csv
# Update: No dependency on emmeans::CLD/cld. Falls back to multcomp letters,
# and if unavailable, plots without letters (with a clear message).

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(emmeans)
  library(readr)
})

# -------- Inputs --------
input_csv  <- "data/statistics/nitrogen_losses_example.csv"
output_dir <- "outputs_stats"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# -------- Load --------
df <- read_csv(input_csv, show_col_types = FALSE) %>%
  mutate(Treatment = factor(Treatment), Zone = factor(Zone))

# -------- Helper: compute letters robustly --------
letters_from_emmeans_or_multcomp <- function(fit, spec = ~ Zone * Treatment, adjust = "sidak") {
  # Try emmeans::CLD if present
  if ("CLD" %in% getNamespaceExports("emmeans")) {
    emm <- emmeans::emmeans(fit, spec)
    cld <- emmeans::CLD(emm, adjust = adjust, alpha = 0.05, Letters = letters)
    return(as.data.frame(cld)[, c("Zone","Treatment",".group"), drop = FALSE])
  }
  # Try multcomp + multcompView (Tukey on interaction)
  if (requireNamespace("multcomp", quietly = TRUE) && requireNamespace("multcompView", quietly = TRUE)) {
    intf <- interaction(df$Zone, df$Treatment, sep = ":")
    # refit on interaction only to get group means tested with Tukey
    resp <- model.response(model.frame(fit))
    dat2 <- data.frame(y = resp, intf = intf)
    fit2 <- lm(y ~ intf, data = dat2)
    gl <- multcomp::glht(fit2, linfct = multcomp::mcp(intf = "Tukey"))
    summ <- summary(gl)
    # Build compact letter display
    # Create a named vector of means
    means <- tapply(dat2$y, dat2$intf, mean, na.rm = TRUE)
    # p-value matrix from pairwise comps
    comps <- data.frame(summ$test$coefficients, summ$test$pvalues)
    # Names in form "intfB - intfA"
    nm <- names(summ$test$coefficients)
    parts <- strsplit(nm, " - ")
    lvls <- names(means)
    # Build a square matrix of p-values
    P <- matrix(1, nrow = length(lvls), ncol = length(lvls), dimnames = list(lvls, lvls))
    for (i in seq_along(parts)) {
      B <- parts[[i]][1]; A <- parts[[i]][2]
      p <- summ$test$pvalues[i]
      P[B, A] <- p; P[A, B] <- p
    }
    # Use multcompView to obtain letters (threshold alpha = 0.05)
    let <- multcompView::multcompLetters(P, threshold = 0.05)$Letters
    # Turn into a data.frame with separate Zone/Treatment columns
    zt <- do.call(rbind, strsplit(names(let), ":", fixed = TRUE))
    out <- data.frame(Zone = zt[,1], Treatment = zt[,2], .group = unname(let), row.names = NULL, stringsAsFactors = FALSE)
    return(out)
  }
  # Nothing available
  message("Note: Could not compute CLD letters (need emmeans::CLD or multcomp+multcompView). Proceeding without letters.")
  return(NULL)
}

add_letters <- function(p, cld_df, df, ycol, dodge = 0.9, pad = 0.05) {
  if (is.null(cld_df)) return(p)
  max_by_group <- df %>% group_by(Treatment, Zone) %>%
    summarise(ymax = max(.data[[ycol]], na.rm = TRUE), .groups = "drop")
  lab_df <- left_join(cld_df, max_by_group, by = c("Treatment","Zone")) %>%
    mutate(letter_y = ymax * (1 + pad))
  p + geom_text(data = lab_df, aes(x = Treatment, y = letter_y, label = .group),
                position = position_dodge(width = dodge), size = 3.8)
}

plot_box <- function(df, y, ylab) {
  ggplot(df, aes(Treatment, .data[[y]], fill = Zone)) +
    geom_boxplot(position = position_dodge(0.9)) +
    labs(x = "Treatment", y = ylab, fill = "Zone") +
    theme_classic(base_size = 12)
}

analyze_one <- function(resp_formula, ycol, ylab) {
  fit <- lm(resp_formula, data = df)
  cld_df <- letters_from_emmeans_or_multcomp(fit, spec = ~ Zone * Treatment, adjust = "sidak")
  p <- plot_box(df, ycol, ylab)
  p <- add_letters(p, cld_df, df, ycol)
  p
}

# -------- Run panels --------
p1 <- analyze_one(N_Leached ~ Zone * Treatment, "N_Leached", "N leached (kg N/ha)")
p2 <- analyze_one(Volatilization ~ Zone * Treatment, "Volatilization", "Volatilization (kg N/ha)")
p3 <- analyze_one(Denitrification ~ Zone * Treatment, "Denitrification", "Denitrification (kg N/ha)")
p4 <- analyze_one(TotalNLosses ~ Zone * Treatment, "TotalNLosses", "Total N loss (kg N/ha)")

# -------- Save --------
ggsave(file.path(output_dir, "N_leached_boxletters.png"), p1, width = 8, height = 5, dpi = 300)
ggsave(file.path(output_dir, "Volatilization_boxletters.png"), p2, width = 8, height = 5, dpi = 300)
ggsave(file.path(output_dir, "Denitrification_boxletters.png"), p3, width = 8, height = 5, dpi = 300)
ggsave(file.path(output_dir, "TotalNLosses_boxletters.png"), p4, width = 8, height = 5, dpi = 300)

cat("Saved figures to", normalizePath(output_dir), "\n")
