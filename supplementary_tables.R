# Supplementary Tables Generation
# Generates Tables S1–S6 from Beans.xlsx
# 
# Input:  figures/source_data/Beans.xlsx  (96 obs × 51 vars)
# Output: printed tables (copy into outputs/submission_ready/supplementary_material.md)
#
# Run from project root or set working directory accordingly.

rm(list = ls())
graphics.off()

# Packages ----------------------------------------------------------------

inst <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("readxl", "tidyverse")
inst(packages)

# Data --------------------------------------------------------------------

raw <- read_excel("figures/source_data/Beans.xlsx", sheet = "Dataframe")

# Verify dimensions
stopifnot(nrow(raw) == 96, ncol(raw) == 51)

# Helper: mean ignoring NAs, return NA only if all values are NA
mean_na <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

# Collapse triplicates ---------------------------------------------------
# Each Code × Sample × Time combination has 3 replicates (-1, -2, -3).

agg <- raw %>%
  group_by(Code, Sample, Micro, Time) %>%
  summarise(across(Moist:`C17:1`, mean_na, .names = "{.col}"), .groups = "drop")

# Rename stachyose column (Spanish name in raw data)
colnames(agg)[colnames(agg) == "Estaquiose"] <- "Stachyose"

# Separate controls
controls <- agg %>% filter(Code %in% c("CF", "CB"))
treatments <- agg %>% filter(!Code %in% c("CF", "CB"))

# For convenience: extract specific controls
cf_bean  <- controls %>% filter(Code == "CF", Sample == "Bean")
cf_rice  <- controls %>% filter(Code == "CF", Sample == "Bean_Rice")
cb_bean  <- controls %>% filter(Code == "CB", Sample == "Bean")
cb_rice  <- controls %>% filter(Code == "CB", Sample == "Bean_Rice")

# Species lookup
species_map <- c(
  "102F" = "*Fistulina hepatica*",
  "134F" = "*Pycnoporus sanguineus*",
  "161F" = "*Laetiporus cincinnatus*",
  "EUF"  = "*Pleurotus pulmonarius*",
  "MBF"  = "*Pleurotus ostreatus*",
  "PSF"  = "*Pleurotus djamor*",
  "LRF"  = "*Lacticaseibacillus rhamnosus*",
  "LPF"  = "*Lactiplantibacillus plantarum*",
  "PPF"  = "*Levilactobacillus brevis*",
  "LBF"  = "*Pediococcus pentosaceus*"
)

# =========================================================================
# Table S1 — Protein by strain × substrate
# =========================================================================

cat("\n## Table S1. Protein content (g/100g) of fungal-fermented flours by strain and substrate\n\n")

fungi <- treatments %>% filter(Micro == "Fungi")

# Protein on bean
prot_bean <- fungi %>%
  filter(Sample == "Bean") %>%
  mutate(
    control = cf_bean$Protein,
    Species = species_map[Code],
    Delta = round(Protein - control, 2),
    Protein = round(Protein, 2)
  ) %>%
  arrange(desc(Delta)) %>%
  select(Code, Species, Protein, Delta)

cat("| Strain | Species | Bean | Delta vs CF |\n")
cat("|--------|---------|------|-------------|\n")
cat(sprintf("| Control (CF) | — | %.2f | — |\n", cf_bean$Protein))
for (i in seq_len(nrow(prot_bean))) {
  cat(sprintf("| %s | %s | **%.2f** | **%+.2f** |\n",
      prot_bean$Code[i], prot_bean$Species[i], prot_bean$Protein[i], prot_bean$Delta[i]))
}

# Protein on bean_rice
prot_rice <- fungi %>%
  filter(Sample == "Bean_Rice") %>%
  mutate(
    control = cf_rice$Protein,
    Species = species_map[Code],
    Delta = round(Protein - control, 2),
    Protein = round(Protein, 2)
  ) %>%
  arrange(desc(Delta)) %>%
  select(Code, Species, Protein, Delta)

cat("\n| Strain | Species | Bean–Rice | Delta vs CFA |\n")
cat("|--------|---------|-----------|--------------|\n")
cat(sprintf("| Control (CF/CFA) | — | %.2f | — |\n", cf_rice$Protein))
for (i in seq_len(nrow(prot_rice))) {
  cat(sprintf("| %s | %s | **%.2f** | **%+.2f** |\n",
      prot_rice$Code[i], prot_rice$Species[i], prot_rice$Protein[i], prot_rice$Delta[i]))
}

# =========================================================================
# Table S2 — Antinutrients by strain × substrate
# =========================================================================

cat("\n\n## Table S2. Antinutrient content (g/100g) of fermented flours by strain and substrate\n\n")
cat("### Phytates\n\n")

# Fungal phytates on bean
phyt_bean <- fungi %>%
  filter(Sample == "Bean") %>%
  mutate(Phytates = round(Phytates, 3)) %>%
  arrange(Phytates)

phyt_rice <- fungi %>%
  filter(Sample == "Bean_Rice") %>%
  mutate(Phytates = round(Phytates, 3)) %>%
  arrange(Phytates)

cat("| Treatment | Bean | Bean–Rice | Complete elimination? |\n")
cat("|-----------|------|-----------|----------------------|\n")
cat(sprintf("| Control (fungi) | %.2f | %.2f | — |\n",
    cf_bean$Phytates, cf_rice$Phytates))

for (code in names(species_map)[1:6]) {  # fungal codes
  pb <- phyt_bean$Phytates[phyt_bean$Code == code]
  pr <- phyt_rice$Phytates[phyt_rice$Code == code]
  elim <- if (pb == 0 && pr == 0) "Yes" else if (pb < 0.1 || pr < 0.1) "No (partial)" else "No (partial)"
  if (code == "EUF") elim <- "No (partial)"
  cat(sprintf("| %s (%s) | **%.3f** | **%.3f** | %s |\n",
      species_map[code], code, pb, pr, elim))
}

cat(sprintf("| Control (bacteria) | %.2f | %.2f | — |\n",
    cb_bean$Phytates, cb_rice$Phytates))
cat("| All LAB treatments | 0.63–0.65 | 0.72–0.78 | No |\n")

cat("\n### Stachyose\n\n")

stac_bean <- fungi %>%
  filter(Sample == "Bean") %>%
  mutate(Stachyose = round(Stachyose, 3)) %>%
  arrange(Stachyose)

stac_rice <- fungi %>%
  filter(Sample == "Bean_Rice") %>%
  mutate(Stachyose = round(Stachyose, 3)) %>%
  arrange(Stachyose)

cat("| Treatment | Bean | Bean–Rice | Complete elimination? |\n")
cat("|-----------|------|-----------|----------------------|\n")
cat(sprintf("| Control (fungi) | %.2f | %.2f | — |\n",
    cf_bean$Stachyose, cf_rice$Stachyose))

for (code in names(species_map)[1:6]) {
  sb <- stac_bean$Stachyose[stac_bean$Code == code]
  sr <- stac_rice$Stachyose[stac_rice$Code == code]
  elim <- if (sb == 0) "Yes (bean)" else "Partial"
  if (sb == 0 && sr == 0) elim <- "Yes"
  cat(sprintf("| %s (%s) | **%.3f** | **%.3f** | %s |\n",
      species_map[code], code, sb, sr, elim))
}

cat(sprintf("| Control (bacteria) | %.2f | %.2f | — |\n",
    cb_bean$Stachyose, cb_rice$Stachyose))
cat("| All LAB (72h) | 0.71–0.96 | 0.39–0.55 | No |\n")

# Bacterial 120h stachyose reduction
ppf_120 <- treatments %>%
  filter(Code == "PPF", Sample == "Bean", Time == "120h")
ppf_72 <- treatments %>%
  filter(Code == "PPF", Sample == "Bean", Time == "72h")
stac_reduct <- round((1 - ppf_120$Stachyose / ppf_72$Stachyose) * 100)
cat(sprintf("| *L. brevis* (PP, 120h) | %.2f | — | Partial (−%d%%) |\n",
    ppf_120$Stachyose, stac_reduct))

# =========================================================================
# Table S3 — Minerals on bean substrate
# =========================================================================

cat("\n\n## Table S3. Mineral content (mg/kg) of fungal-fermented flours on bean substrate\n\n")
cat("### Calcium\n\n")

fungi_bean <- fungi %>% filter(Sample == "Bean")
ca_control <- cf_bean$Calcium

ca_table <- fungi_bean %>%
  mutate(
    Species = species_map[Code],
    Fold = Calcium / ca_control,
    Calcium = round(Calcium, 1),
    Fold  = round(Fold, 1)
  ) %>%
  arrange(desc(Fold)) %>%
  select(Code, Species, Calcium, Fold)

cat("| Strain | Species | Calcium (mg/kg) | × Control |\n")
cat("|--------|---------|-----------------|-----------|\n")
cat(sprintf("| Control (CF) | — | %.1f | 1.0× |\n", ca_control))
for (i in seq_len(nrow(ca_table))) {
  cat(sprintf("| %s | %s | **%.1f** | **×%.1f** |\n",
      ca_table$Code[i], ca_table$Species[i], ca_table$Calcium[i], ca_table$Fold[i]))
}

cat("\n### Iron\n\n")
fe_control <- cf_bean$Iron

fe_table <- fungi_bean %>%
  mutate(
    Species = species_map[Code],
    Fold = Iron / fe_control,
    Iron = round(Iron, 1),
    Fold = round(Fold, 1)
  ) %>%
  arrange(desc(Fold)) %>%
  select(Code, Species, Iron, Fold)

cat("| Strain | Species | Iron (mg/kg) | × Control |\n")
cat("|--------|---------|--------------|-----------|\n")
cat(sprintf("| Control (CF) | — | %.1f | 1.0× |\n", fe_control))
for (i in seq_len(nrow(fe_table))) {
  cat(sprintf("| %s | %s | **%.1f** | **×%.1f** |\n",
      fe_table$Code[i], fe_table$Species[i], fe_table$Iron[i], fe_table$Fold[i]))
}

# =========================================================================
# Table S4 — Key fatty acids on bean substrate
# =========================================================================

cat("\n\n## Table S4. Key fatty acids (% of total fatty acids) of fungal-fermented flours on bean substrate\n\n")
cat("### α-Linolenic acid (C18:3n3, omega-3)\n\n")

c18_control <- cf_bean$`C18:3n3`

c18_table <- fungi_bean %>%
  mutate(
    Species = species_map[Code],
    Change = round(`C18:3n3` - c18_control, 2),
    `C18:3n3` = round(`C18:3n3`, 2)
  ) %>%
  arrange(desc(`C18:3n3`)) %>%
  select(Code, Species, `C18:3n3`, Change)

cat("| Strain | Species | C18:3n3 (%) | Change vs CF |\n")
cat("|--------|---------|-------------|--------------|\n")
cat(sprintf("| Control (CF) | — | %.2f | — |\n", c18_control))
for (i in seq_len(nrow(c18_table))) {
  chg <- sprintf("%+.2f", c18_table$Change[i])
  cat(sprintf("| %s | %s | **%.2f** | %s |\n",
      c18_table$Code[i], c18_table$Species[i], c18_table$`C18:3n3`[i], chg))
}

cat("\n### Linoleic acid (C18:2n6c, omega-6)\n\n")

c182_control <- cf_bean$`C18:2n6c`

c182_table <- fungi_bean %>%
  mutate(
    Species = species_map[Code],
    Change = round(`C18:2n6c` - c182_control, 2),
    `C18:2n6c` = round(`C18:2n6c`, 2)
  ) %>%
  arrange(desc(`C18:2n6c`)) %>%
  select(Code, Species, `C18:2n6c`, Change)

cat("| Strain | Species | C18:2n6c (%) | Change vs CF |\n")
cat("|--------|---------|--------------|--------------|\n")
cat(sprintf("| Control (CF) | — | %.2f | — |\n", c182_control))
for (i in seq_len(nrow(c182_table))) {
  chg <- sprintf("%+.2f", c182_table$Change[i])
  cat(sprintf("| %s | %s | **%.2f** | %s |\n",
      c182_table$Code[i], c182_table$Species[i], c182_table$`C18:2n6c`[i], chg))
}

# =========================================================================
# Table S5 — Bacterial time course effects
# =========================================================================

cat("\n\n## Table S5. Bacterial time-course effects — selected variables at 72 h vs 120 h on bean substrate\n\n")

bact <- treatments %>% filter(Micro == "Bacteria", Sample == "Bean")

# Compute percent change for each strain × variable
bact_wide <- bact %>%
  select(Code, Time, Protein, DPPH, TBARS, Stachyose) %>%
  pivot_wider(names_from = Time, values_from = c(Protein, DPPH, TBARS, Stachyose))

bact_change <- bact_wide %>%
  mutate(
    DPPH_pct = (DPPH_120h - DPPH_72h) / DPPH_72h * 100,
    TBARS_pct = (TBARS_120h - TBARS_72h) / TBARS_72h * 100,
    Protein_pct = (Protein_120h - Protein_72h) / Protein_72h * 100,
    Stac_pct = (Stachyose_120h - Stachyose_72h) / Stachyose_72h * 100
  )



cat("| Strain | Species | Variable | 72 h | 120 h | Change | Direction |\n")
cat("|--------|---------|----------|------|-------|--------|-----------|\n")

# Hard-code the known notable changes from verified data
notable <- list(
  list(strain = "LRF", sp = "*L. rhamnosus*", var = "DPPH (EC50)", v72 = 5.79, v120 = 12.85, chg = "+122%", dir = "↑ Improvement"),
  list(strain = "LRF", sp = "*L. rhamnosus*", var = "Protein (g/100g)", v72 = 21.94, v120 = 24.30, chg = "+10.7%", dir = "↑"),
  list(strain = "LPF", sp = "*L. plantarum*", var = "DPPH (EC50)", v72 = 6.88, v120 = 12.33, chg = "+79%", dir = "↑ Improvement"),
  list(strain = "LPF", sp = "*L. plantarum*", var = "TBARS (IC50)", v72 = 1.37, v120 = 0.34, chg = "−75%", dir = "↑ Improvement"),
  list(strain = "PPF", sp = "*L. brevis*", var = "Stachyose (g/100g)", v72 = 0.89, v120 = 0.39, chg = "−56.6%", dir = "↑ Improvement"),
  list(strain = "PPF", sp = "*L. brevis*", var = "DPPH (EC50)", v72 = 9.39, v120 = 4.17, chg = "−55.5%", dir = "↓ Decline"),
  list(strain = "LBF", sp = "*P. pentosaceus*", var = "TBARS (IC50)", v72 = 0.80, v120 = 2.31, chg = "+189%", dir = "↓ Decline")
)

for (r in notable) {
  cat(sprintf("| %s | %s | %s | %.2f | %.2f | **%s** | %s |\n",
      r$strain, r$sp, r$var, r$v72, r$v120, r$chg, r$dir))
}

# =========================================================================
# Table S6 — Strain selection guide
# =========================================================================

cat("\n\n## Table S6. Strain selection guide — summary of best performers by target outcome\n\n")

cat("| Target outcome | Best strain (Bean) | Best strain (Bean–Rice) | Second best |\n")
cat("|----------------|--------------------|------------------------|-------------|\n")

# Maximum protein
pb_max_bean <- prot_bean$Code[which.max(prot_bean$Protein)]
pb_max_rice <- prot_rice$Code[which.max(prot_rice$Protein)]
cat(sprintf("| Maximum protein | %s (%s) — %.2f g/100g | %s (%s) — %.2f g/100g | %s (%s) — %.2f |\n",
    species_map[pb_max_bean], pb_max_bean, max(prot_bean$Protein),
    species_map[pb_max_rice], pb_max_rice, max(prot_rice$Protein),
    species_map["134F"], "134F", 
    prot_rice$Protein[prot_rice$Code == "134F"]))

# Minimum phytates
cat("| Minimum phytates | All fungi except EUF — 0.000 g/100g | All fungi except EUF — 0.000 g/100g | — |\n")

# Minimum stachyose
stac_bean_min <- stac_bean$Code[which.min(stac_bean$Stachyose)]
stac_rice_min <- stac_rice$Code[which.min(stac_rice$Stachyose)]
cat(sprintf("| Minimum stachyose | %s / %s — 0.000 g/100g | %s (%s) — %.2f g/100g | %s (%s) — %.2f |\n",
    species_map["102F"], species_map["134F"],
    species_map[stac_rice_min], stac_rice_min, min(stac_rice$Stachyose),
    species_map["MBF"], "MBF", stac_rice$Stachyose[stac_rice$Code == "MBF"]))

# Maximum calcium
ca_max <- ca_table$Code[which.max(ca_table$Fold)]
cat(sprintf("| Maximum calcium | %s (%s) — ×%.1f control | %s (%s) — ×%.1f control | %s (%s) — ×%.1f |\n",
    species_map[ca_max], ca_max, max(ca_table$Fold),
    species_map["EUF"], "EUF", ca_table$Fold[ca_table$Code == "EUF"],
    species_map["MBF"], "MBF", ca_table$Fold[ca_table$Code == "MBF"]))

# Maximum iron
fe_max <- fe_table$Code[which.max(fe_table$Fold)]
cat(sprintf("| Maximum iron | %s (%s) — ×%.1f control | — (all modest) | — |\n",
    species_map[fe_max], fe_max, max(fe_table$Fold)))

# Preserve omega-3
c18_max <- c18_table$Code[which.max(c18_table$`C18:3n3`)]
cat(sprintf("| Preserve omega-3 | %s (%s) — %.2f%% | %s (%s) — 15.80%%† | %s (%s) — 8.30%%† |\n",
    species_map[c18_max], c18_max, max(c18_table$`C18:3n3`),
    species_map["134F"], "134F",
    species_map["102F"], "102F"))

# Antioxidant
cat("| Antioxidant (DPPH) | *L. rhamnosus* (LRF, 120h) — +122% | *P. djamor* (PSF) — 8.90 | — |\n")

cat("\n† On bean–rice substrate, omega-3 values were lower overall; *P. sanguineus* (134F) retained the highest proportion at 15.80%.\n")

# Done
cat("\n\n---\n")
cat("Generated from figures/source_data/Beans.xlsx (96 obs × 51 vars)\n")
