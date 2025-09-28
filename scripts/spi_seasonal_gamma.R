# spi_seasonal_gamma.R
# Compute Oct–May seasonal SPI from daily precipitation.
# Sample data: data/precip_daily.csv with columns: DATE (YYYY-MM-DD), PRCP (mm)

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(fitdistrplus)
  library(ggplot2)
})

# -------- Inputs --------
input_csv  <- "data/precip_daily.csv"
output_dir <- "outputs_spi"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# -------- Load --------
x <- read.csv(input_csv, stringsAsFactors = FALSE)
stopifnot(all(c("DATE","PRCP") %in% names(x)))
x$DATE <- as.Date(x$DATE)

# -------- Prep --------
x <- x %>%
  mutate(Year = year(DATE), Month = month(DATE)) %>%
  filter(!is.na(PRCP))

# Oct–May season, season year = year of May
season_months <- c(10,11,12,1,2,3,4,5)
s <- x %>%
  filter(Month %in% season_months) %>%
  mutate(SeasonYear = ifelse(Month %in% c(10,11,12), Year + 1L, Year))

# Require complete Oct–May months (8 months present)
has_full_season <- function(df, Y) {
  need <- data.frame(y = c(rep(Y-1,3), rep(Y,5)),
                     m = c(10,11,12,1,2,3,4,5))
  all(apply(need, 1, function(z) any(df$Year==z[1] & df$Month==z[2])))
}
cand <- sort(unique(s$SeasonYear))
ok   <- cand[sapply(cand, function(Y) has_full_season(s, Y))]
if (length(ok) == 0L) stop("No complete Oct–May seasons found in: ", input_csv)

# Aggregate to seasonal total precip
agg <- s %>%
  filter(SeasonYear %in% ok) %>%
  group_by(SeasonYear) %>%
  summarise(Total_PRCP = sum(PRCP, na.rm = TRUE), .groups = "drop")

# -------- SPI via gamma fit --------
eps  <- 1e-6
vals <- pmax(agg$Total_PRCP, eps)
fit  <- fitdist(vals, "gamma", method = "mme")
alpha <- unname(fit$estimate["shape"])
beta  <- unname(fit$estimate["rate"])

G <- pgamma(vals, shape = alpha, rate = beta)
G <- pmin(pmax(G, 1e-6), 1 - 1e-6)
agg$SPI <- qnorm(G)

# Classify (adjust cutoffs if you prefer)
agg$Class <- ifelse(agg$SPI <= -1.0, "Drought",
                    ifelse(agg$SPI >=  1.0, "Wet", "Normal"))

# -------- Save --------
out_csv <- file.path(output_dir, "SPI_Season_OctMay.csv")
write.csv(agg, out_csv, row.names = FALSE)

p <- ggplot(agg, aes(x = factor(SeasonYear), y = SPI, fill = Class)) +
  geom_col() +
  geom_hline(yintercept = 0, color = "black") +
  geom_hline(yintercept = -1.0, linetype = "dashed", color = "grey40") +
  geom_hline(yintercept =  1.0, linetype = "dashed", color = "grey40") +
  scale_fill_manual(values = c(Drought="#c0392b", Normal="grey60", Wet="#2e86c1")) +
  labs(title = "Seasonal SPI (Oct–May)",
       x = "Season (ending May)", y = "SPI (z-score)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

ggsave(file.path(output_dir, "SPI_Season_OctMay.png"), p, width = 10, height = 5, dpi = 300)
cat("SPI written to:", normalizePath(out_csv), "\n")
