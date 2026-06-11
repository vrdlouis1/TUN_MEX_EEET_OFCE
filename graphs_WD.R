
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(scales)
library(patchwork)

# ── Config ──────────────────────────────────────────────────────────────────

data_dir   <- "data/import_result/"
output_dir <- "output/plots_WD/"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

ref_year        <- 2020
horizon_2030    <- 2030
horizon_2050    <- 2050
tun_snbc_policy <- "CT4_SUB23_ENRelec_redis"

xr_tnd_per_usd <- 2.81
xr_mxn_per_usd <- 21.5

# ── Load data ───────────────────────────────────────────────────────────────

tun_baseline <- readRDS(file.path(data_dir, "all_baseline_TUN.rds"))
tun_shock    <- readRDS(file.path(data_dir, "all_shock_TUN.rds"))
mex_baseline <- readRDS(file.path(data_dir, "all_baseline_MEX.rds"))
mex_shock    <- readRDS(file.path(data_dir, "all_shock_MEX.rds"))

tax_carbone_mex <- tryCatch(
  readRDS(file.path(data_dir, "tax_carbone_MEX.rds")),
  error = function(e) NULL
)

# ── Harmonised scenario names ───────────────────────────────────────────────
# CT  = carbon tax ; SUB = fossil fuel subsidy removal ; REN = renewables ;
# NLCS = National Low Carbon Strategy (full policy package, recycled revenues)

policy_labels_all <- c(
  "CT4"                     = "Carbon tax (CT)",
  "CT4_redis"               = "CT + recycling",
  "SUB23"                   = "Subsidy removal (SUB)",
  "SUB23_redis"             = "SUB + recycling",
  "ENR_elec_only"           = "Renewables (REN)",
  "CT4_SUB23_ENRelec_redis" = "NLCS"
)

snbc_label <- "NLCS"
snbc_short <- "NLCS"

# ── Palette ─────────────────────────────────────────────────────────────────

col_tunisia    <- "#B03A2E"
col_mexico     <- "#1A5276"
col_gdp        <- "#1A5276"
col_emissions  <- "#B03A2E"
col_intensity  <- "#117A65"
col_gain       <- "#117A65"
col_loss       <- "#B03A2E"
col_price      <- "#1A5276"
col_revenue    <- "#B03A2E"

# Same colour per instrument; recycling variants distinguished by line type
scenario_colors_all <- c(
  "Carbon tax (CT)"       = "#D4AC0D",
  "CT + recycling"        = "#1A5276",
  "Subsidy removal (SUB)" = "#B03A2E",
  "SUB + recycling"       = "#117A65",
  "Renewables (REN)"      = "#7D3C98",
  "NLCS"                  = "#E67E22"
)
scenario_linetypes_all <- c(
  "Carbon tax (CT)"       = "dashed",
  "CT + recycling"        = "solid",
  "Subsidy removal (SUB)" = "dashed",
  "SUB + recycling"       = "solid",
  "Renewables (REN)"      = "solid",
  "NLCS"                  = "solid"
)

demand_colors <- c(
  "Household consumption" = "#2980B9",
  "Investment"            = "#117A65",
  "Exports"               = "#D4AC0D",
  "Imports"               = "#8E44AD"
)

sector_colors <- c(
  "Industry"        = "#2C3E50",
  "Transport"       = "#D4AC0D",
  "Services"        = "#2980B9",
  "Electricity"     = "#E67E22",
  "Fossil fuel transf." = "#7F8C8D",
  "Energy"          = "#E67E22",
  "Households"      = "#C0392B",
  "Total"           = "#1A1A1A"
)

# ── Theme ───────────────────────────────────────────────────────────────────

# All font sizes are absolute (pt) so every figure renders identically
theme_wd <- function(base_size = 11, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      plot.title    = element_text(face = "bold", size = 13, color = "black",
                                   hjust = 0.5, margin = margin(b = 6)),
      plot.subtitle = element_text(size = 10, hjust = 0.5,
                                    color = "grey35", margin = margin(b = 8)),
      plot.caption  = element_text(size = 9, hjust = 0.5,
                                    color = "grey45", margin = margin(t = 8)),
      plot.caption.position = "plot",
      axis.title    = element_text(size = 10),
      axis.title.x  = element_text(size = 10, margin = margin(t = 6)),
      axis.title.y  = element_text(size = 10, margin = margin(r = 6), angle = 90),
      axis.text     = element_text(size = 9, color = "grey25"),
      axis.ticks    = element_line(color = "grey60", linewidth = 0.3),
      panel.border  = element_rect(color = "grey40", fill = NA, linewidth = 0.4),
      panel.grid.major = element_line(color = "grey88", linewidth = 0.25),
      panel.grid.minor = element_blank(),
      panel.spacing    = unit(1.2, "lines"),
      strip.background = element_blank(),
      strip.text    = element_text(face = "bold", size = 11, color = "black",
                                    margin = margin(4, 4, 6, 4)),
      legend.position      = "bottom",
      legend.justification = "center",
      legend.title   = element_text(face = "bold", size = 10),
      legend.text    = element_text(size = 9),
      legend.key.size = unit(1.1, "lines"),
      legend.key.width = unit(1.6, "lines"),
      legend.margin  = margin(t = 2),
      legend.background = element_blank(),
      plot.margin = margin(10, 12, 8, 10)
    )
}

# ── Helpers ─────────────────────────────────────────────────────────────────

compute_delta_pct <- function(baseline_val, shock_val) {
  ifelse(baseline_val != 0, 100 * (shock_val / baseline_val - 1), NA_real_)
}

compute_tun_delta <- function(baseline_df, shock_df, var_bl, var_sh,
                              policy_filter = NULL) {
  bl <- baseline_df %>% select(policy, year, value_bl = all_of(var_bl))
  sh <- shock_df   %>% select(policy, year, value_sh = all_of(var_sh))
  df <- inner_join(bl, sh, by = c("policy", "year")) %>%
    mutate(delta_pct = compute_delta_pct(value_bl, value_sh))
  if (!is.null(policy_filter)) df <- df %>% filter(policy == policy_filter)
  df
}

compute_mex_delta <- function(baseline_df, shock_df, var) {
  bl <- baseline_df %>% select(year, value_bl = all_of(var))
  sh <- shock_df   %>% select(year, value_sh = all_of(var))
  inner_join(bl, sh, by = "year") %>%
    mutate(delta_pct = compute_delta_pct(value_bl, value_sh))
}


# ═══════════════════════════════════════════════════════════════════════════
# FIG 1 : CO2 emissions by scenario (Tunisia)
# ═══════════════════════════════════════════════════════════════════════════

tun_bl_ems <- tun_baseline %>%
  filter(policy == "CT4") %>%
  select(year, ems_bl = ems_co2_0)

tun_ems_scenarios <- tun_shock %>%
  filter(policy %in% names(policy_labels_all)) %>%
  select(year, policy, ems_sh = ems_co2_2) %>%
  inner_join(tun_bl_ems, by = "year") %>%
  mutate(
    delta_pct = compute_delta_pct(ems_bl, ems_sh),
    scenario  = factor(policy_labels_all[policy], levels = policy_labels_all)
  ) %>%
  filter(year >= 2020, year <= 2050)

p_fig1 <- ggplot(tun_ems_scenarios,
                 aes(x = year, y = delta_pct,
                     color = scenario, linetype = scenario)) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "grey50") +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = scenario_colors_all, name = NULL) +
  scale_linetype_manual(values = scenario_linetypes_all, name = NULL) +
  scale_x_continuous(breaks = seq(2020, 2050, by = 5)) +
  labs(
    title = "CO2 emissions with respect to baseline",
    subtitle = "Tunisia, by policy scenario",
    caption = "Source: ThreeME simulations.",
    x = NULL,
    y = "CO2 emissions (% deviation from baseline)"
  ) +
  theme_wd() +
  guides(color = guide_legend(nrow = 2), linetype = guide_legend(nrow = 2))

ggsave(file.path(output_dir, "fig01_co2_scenarios_tunisie.png"), p_fig1,
       width = 10, height = 6, dpi = 300)



# ═══════════════════════════════════════════════════════════════════════════
# FIG 2 : GDP by scenario (Tunisia)
# ═══════════════════════════════════════════════════════════════════════════

tun_bl_gdp <- tun_baseline %>%
  filter(policy == "CT4") %>%
  select(year, gdp_bl = gdp_0)

tun_gdp_scenarios <- tun_shock %>%
  filter(policy %in% names(policy_labels_all)) %>%
  select(year, policy, gdp_sh = gdp_2) %>%
  inner_join(tun_bl_gdp, by = "year") %>%
  mutate(
    delta_pct = compute_delta_pct(gdp_bl, gdp_sh),
    scenario  = factor(policy_labels_all[policy], levels = policy_labels_all)
  ) %>%
  filter(year >= 2020, year <= 2050)

p_fig2 <- ggplot(tun_gdp_scenarios,
                 aes(x = year, y = delta_pct,
                     color = scenario, linetype = scenario)) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "grey50") +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = scenario_colors_all, name = NULL) +
  scale_linetype_manual(values = scenario_linetypes_all, name = NULL) +
  scale_x_continuous(breaks = seq(2020, 2050, by = 5)) +
  labs(
    title = "GDP variation with respect to baseline",
    subtitle = "Tunisia, by policy scenario",
    caption = "Source: ThreeME simulations.",
    x = NULL,
    y = "GDP (% deviation from baseline)"
  ) +
  theme_wd() +
  guides(color = guide_legend(nrow = 2), linetype = guide_legend(nrow = 2))

ggsave(file.path(output_dir, "fig02_pib_scenarios_tunisie.png"), p_fig2,
       width = 10, height = 6, dpi = 300)



# ═══════════════════════════════════════════════════════════════════════════
# FIG 3 : Carbon price and tax revenues (TUN / MEX)
# ═══════════════════════════════════════════════════════════════════════════

tun_tax_data <- tun_shock %>%
  filter(policy == tun_snbc_policy) %>%
  transmute(
    year,
    country = "Tunisia",
    carbon_price = `rco2tax_vol*1000` / xr_tnd_per_usd,
    revenue_gdp  = 100 * (`rco2tax_vol*1000` * (ems_co2_2 * 1000)) / (gdp_2 * 1e6)
  )

if (!is.null(tax_carbone_mex)) {
  tax_moy_mex <- tax_carbone_mex %>%
    rowwise() %>%
    mutate(taxe_moyenne = mean(c_across(starts_with("RCO2TAX_VOL_")), na.rm = TRUE)) %>%
    ungroup() %>%
    select(year, taxe_moyenne)

  mex_tax_data <- mex_shock %>%
    inner_join(tax_moy_mex, by = "year") %>%
    transmute(
      year,
      country = "Mexico",
      carbon_price = taxe_moyenne / xr_mxn_per_usd,
      revenue_gdp  = 100 * (taxe_moyenne * EMS) / GDP
    )
} else {
  mex_tax_data <- tibble(year = integer(), country = character(),
                          carbon_price = numeric(), revenue_gdp = numeric())
}

tax_data <- bind_rows(tun_tax_data, mex_tax_data) %>%
  filter(year >= 2020, year <= 2050)

y_max_price  <- max(tax_data$carbon_price, na.rm = TRUE) * 1.1
scale_factor <- max(tax_data$carbon_price, na.rm = TRUE) /
                max(tax_data$revenue_gdp, na.rm = TRUE)

tax_data <- tax_data %>%
  mutate(country_facet = factor(
    ifelse(country == "Tunisia", "(a) Tunisia", "(b) Mexico"),
    levels = c("(a) Tunisia", "(b) Mexico")
  ))

p_fig3 <- ggplot(tax_data, aes(x = year)) +
  geom_line(aes(y = carbon_price, color = "Carbon price (USD/tCO2)",
                linetype = "Carbon price (USD/tCO2)"), linewidth = 0.9) +
  geom_line(aes(y = revenue_gdp * scale_factor, color = "Tax revenues (% of GDP)",
                linetype = "Tax revenues (% of GDP)"), linewidth = 0.9) +
  facet_wrap(~ country_facet, ncol = 2) +
  scale_color_manual(values = c("Carbon price (USD/tCO2)" = col_price,
                                 "Tax revenues (% of GDP)" = col_revenue),
                     name = NULL) +
  scale_linetype_manual(values = c("Carbon price (USD/tCO2)" = "solid",
                                    "Tax revenues (% of GDP)" = "dashed"),
                        name = NULL) +
  scale_x_continuous(breaks = seq(2020, 2050, by = 5)) +
  scale_y_continuous(
    name = "Carbon price (USD/tCO2)",
    limits = c(0, y_max_price),
    expand = c(0, 0),
    sec.axis = sec_axis(~ . / scale_factor,
                        name = "Tax revenues (% of GDP)")
  ) +
  labs(
    title = "Carbon price and carbon tax revenues",
    subtitle = "Tunisia and Mexico",
    caption = paste0(
      "2020 exchange rates: USD 1 = TND 2.81; USD 1 = MXN 21.5.\n",
      "Scenario: ", snbc_label, " (Tunisia); Hacienda 2 (Mexico).\n",
      "Source: ThreeME simulations."
    ),
    x = NULL
  ) +
  theme_wd() +
  theme(
    axis.title.y.left  = element_text(color = col_price),
    axis.text.y.left   = element_text(color = col_price),
    axis.title.y.right = element_text(color = col_revenue, angle = 90),
    axis.text.y.right  = element_text(color = col_revenue)
  )

ggsave(file.path(output_dir, "fig03_taxe_carbone.png"), p_fig3,
       width = 10, height = 5, dpi = 300)



# ═══════════════════════════════════════════════════════════════════════════
# FIG 4 : Decoupling growth – emissions (TUN / MEX)
# ═══════════════════════════════════════════════════════════════════════════

tun_decoupling <- tun_baseline %>%
  filter(policy == tun_snbc_policy) %>%
  select(year, gdp_bl = gdp_0, ems_bl = ems_co2_0) %>%
  inner_join(
    tun_shock %>% filter(policy == tun_snbc_policy) %>%
      select(year, gdp_sh = gdp_2, ems_sh = ems_co2_2),
    by = "year"
  ) %>%
  mutate(intens_bl = ems_bl / gdp_bl,
         intens_sh = ems_sh / gdp_sh) %>%
  transmute(
    year,
    GDP                = compute_delta_pct(gdp_bl, gdp_sh),
    `CO2 emissions`    = compute_delta_pct(ems_bl, ems_sh),
    `Carbon intensity` = compute_delta_pct(intens_bl, intens_sh)
  ) %>%
  pivot_longer(-year, names_to = "indicator", values_to = "delta_pct") %>%
  mutate(country = "(a) Tunisia") %>%
  filter(year >= 2020, year <= 2050, !is.na(delta_pct))

mex_decoupling <- mex_baseline %>%
  select(year, gdp_bl = GDP, ems_bl = EMS) %>%
  inner_join(
    mex_shock %>% select(year, gdp_sh = GDP, ems_sh = EMS),
    by = "year"
  ) %>%
  mutate(intens_bl = ems_bl / gdp_bl,
         intens_sh = ems_sh / gdp_sh) %>%
  transmute(
    year,
    GDP                = compute_delta_pct(gdp_bl, gdp_sh),
    `CO2 emissions`    = compute_delta_pct(ems_bl, ems_sh),
    `Carbon intensity` = compute_delta_pct(intens_bl, intens_sh)
  ) %>%
  pivot_longer(-year, names_to = "indicator", values_to = "delta_pct") %>%
  mutate(country = "(b) Mexico") %>%
  filter(year >= 2020, year <= 2050, !is.na(delta_pct))

decoupling_data <- bind_rows(tun_decoupling, mex_decoupling) %>%
  mutate(indicator = factor(indicator,
                            levels = c("GDP", "CO2 emissions", "Carbon intensity")))

decouple_colors    <- c("GDP" = col_gdp, "CO2 emissions" = col_emissions,
                         "Carbon intensity" = col_intensity)
decouple_linetypes <- c("GDP" = "solid", "CO2 emissions" = "solid",
                         "Carbon intensity" = "longdash")

p_fig4 <- ggplot(decoupling_data,
                 aes(x = year, y = delta_pct,
                     color = indicator, linetype = indicator)) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "grey50") +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ country, ncol = 2) +
  scale_color_manual(values = decouple_colors, name = NULL) +
  scale_linetype_manual(values = decouple_linetypes, name = NULL) +
  scale_x_continuous(breaks = seq(2020, 2050, by = 5)) +
  labs(
    title = "Decoupling between GDP and CO2 emissions",
    subtitle = "Tunisia and Mexico",
    caption = paste0(
      "Carbon intensity = emissions / GDP.\n",
      "Scenario: ", snbc_label, " (Tunisia); Hacienda 2 (Mexico).\n",
      "Source: ThreeME simulations."
    ),
    x = NULL,
    y = "Deviation from baseline (%)"
  ) +
  theme_wd()

ggsave(file.path(output_dir, "fig04_decouplage.png"), p_fig4,
       width = 10, height = 5.5, dpi = 300)



# ═══════════════════════════════════════════════════════════════════════════
# FIG 5 : Macroeconomic impacts : 6 lettered panels (TUN / MEX)
# ═══════════════════════════════════════════════════════════════════════════

tun_macro_mapping <- list(
  `(a) GDP`                   = c(bl = "gdp_0",  sh = "gdp_2"),
  `(b) Household consumption` = c(bl = "ch_0",   sh = "ch_2"),
  `(c) Investment`            = c(bl = "i_0",    sh = "i_2"),
  `(d) Exports`               = c(bl = "x_0",    sh = "x_2"),
  `(e) Imports`               = c(bl = "m_0",    sh = "m_2")
)
mex_macro_mapping <- list(
  `(a) GDP` = "GDP", `(b) Household consumption` = "CH", `(c) Investment` = "I",
  `(d) Exports` = "X", `(e) Imports` = "M"
)

tun_macro <- pmap_dfr(
  list(names(tun_macro_mapping), tun_macro_mapping),
  function(label, vars) {
    compute_tun_delta(tun_baseline, tun_shock,
                      vars[["bl"]], vars[["sh"]], tun_snbc_policy) %>%
      mutate(indicator = label, country = "Tunisia") %>%
      select(year, indicator, country, delta_pct)
  }
)

mex_macro <- map2_dfr(
  names(mex_macro_mapping), mex_macro_mapping,
  function(label, var) {
    compute_mex_delta(mex_baseline, mex_shock, var) %>%
      mutate(indicator = label, country = "Mexico") %>%
      select(year, indicator, country, delta_pct)
  }
)

emp_panel <- "(f) Employment and unemployment"

tun_emp <- compute_tun_delta(tun_baseline, tun_shock,
                             "f_l_0", "f_l_2", tun_snbc_policy) %>%
  mutate(indicator = emp_panel, country = "Tunisia - Employment") %>%
  select(year, indicator, country, delta_pct)

tun_unr <- tun_baseline %>%
  filter(policy == tun_snbc_policy) %>%
  select(year, value_bl = unr_0) %>%
  inner_join(
    tun_shock %>% filter(policy == tun_snbc_policy) %>% select(year, value_sh = unr_2),
    by = "year"
  ) %>%
  mutate(delta_pct = (value_sh - value_bl) * 100,
         indicator = emp_panel, country = "Tunisia - Unemployment") %>%
  select(year, indicator, country, delta_pct)

mex_emp <- compute_mex_delta(mex_baseline, mex_shock, "F_L") %>%
  mutate(indicator = emp_panel, country = "Mexico - Employment") %>%
  select(year, indicator, country, delta_pct)

mex_unr <- mex_baseline %>%
  select(year, value_bl = UNR) %>%
  inner_join(mex_shock %>% select(year, value_sh = UNR), by = "year") %>%
  mutate(delta_pct = (value_sh - value_bl) * 100,
         indicator = emp_panel, country = "Mexico - Unemployment") %>%
  select(year, indicator, country, delta_pct)

macro_all <- bind_rows(tun_macro, mex_macro,
                       tun_emp, tun_unr, mex_emp, mex_unr) %>%
  filter(year >= 2020, year <= 2050) %>%
  mutate(indicator = factor(indicator,
                            levels = c(names(tun_macro_mapping), emp_panel)))

fig5_colors <- c(
  "Tunisia" = col_tunisia, "Mexico" = col_mexico,
  "Tunisia - Employment" = col_tunisia, "Tunisia - Unemployment" = alpha(col_tunisia, 0.5),
  "Mexico - Employment" = col_mexico,  "Mexico - Unemployment" = alpha(col_mexico, 0.5)
)
fig5_linetypes <- c(
  "Tunisia" = "solid", "Mexico" = "solid",
  "Tunisia - Employment" = "solid", "Tunisia - Unemployment" = "dashed",
  "Mexico - Employment" = "solid", "Mexico - Unemployment" = "dashed"
)

p_fig5 <- ggplot(macro_all,
                 aes(x = year, y = delta_pct,
                     color = country, linetype = country)) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "grey50") +
  geom_vline(xintercept = c(2030, 2050), linetype = "dotted",
             color = "grey70", linewidth = 0.3) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ indicator, nrow = 2, ncol = 3, scales = "free_y") +
  scale_color_manual(values = fig5_colors, breaks = c("Tunisia", "Mexico"),
                     name = NULL) +
  scale_linetype_manual(values = fig5_linetypes, breaks = c("Tunisia", "Mexico"),
                        name = NULL) +
  scale_x_continuous(breaks = seq(2020, 2050, by = 10)) +
  labs(
    title = "Macroeconomic impacts with respect to baseline",
    subtitle = "Tunisia and Mexico",
    caption = paste0(
      "Panels (a)-(e): % deviation from baseline. Panel (f): employment (solid) ",
      "and unemployment rate (dashed, deviation in percentage points).\n",
      "Scenario: ", snbc_label, " (Tunisia); Hacienda 2 (Mexico).\n",
      "Source: ThreeME simulations."
    ),
    x = NULL,
    y = "Deviation from baseline (%)"
  ) +
  theme_wd()

ggsave(file.path(output_dir, "fig05_impact_macro.png"), p_fig5,
       width = 10, height = 6.5, dpi = 300)



# ═══════════════════════════════════════════════════════════════════════════
# FIG 6 : Sectoral reallocation of value added (2050)
# ═══════════════════════════════════════════════════════════════════════════

SECTORS_COMMON <- c("Productive sectors", "Transport", "Services", "Energy")

tun_va_2050 <- tun_baseline %>%
  filter(policy == tun_snbc_policy, year == horizon_2050) %>%
  select(va_ind_bl = va_ind_0, va_trsp_bl = va_trsp_0,
         va_ser_bl = va_ser_0, va_ele_bl = va_ele_0, va_trsf_bl = va_trsf_0) %>%
  bind_cols(
    tun_shock %>%
      filter(policy == tun_snbc_policy, year == horizon_2050) %>%
      select(va_ind_sh = va_ind_2, va_trsp_sh = va_trsp_2,
             va_ser_sh = va_ser_2, va_ele_sh = va_ele_2, va_trsf_sh = va_trsf_2)
  ) %>%
  transmute(
    country = "(a) Tunisia",
    `Productive sectors` = compute_delta_pct(va_ind_bl, va_ind_sh),
    Transport            = compute_delta_pct(va_trsp_bl, va_trsp_sh),
    Services             = compute_delta_pct(va_ser_bl, va_ser_sh),
    Energy               = compute_delta_pct(va_ele_bl + va_trsf_bl,
                                             va_ele_sh + va_trsf_sh)
  ) %>%
  pivot_longer(cols = -country, names_to = "sector", values_to = "delta_pct")

mex_va_2050 <- mex_baseline %>%
  filter(year == horizon_2050) %>%
  select(starts_with("VA_S")) %>%
  bind_cols(
    mex_shock %>%
      filter(year == horizon_2050) %>%
      select(va_s001_sh = VA_S001, va_s002_sh = VA_S002, va_s003_sh = VA_S003,
             va_s004_sh = VA_S004, va_s005_sh = VA_S005, va_s006_sh = VA_S006)
  ) %>%
  transmute(
    country = "(b) Mexico",
    `Productive sectors` = compute_delta_pct(VA_S001 + VA_S002 + VA_S003,
                                              va_s001_sh + va_s002_sh + va_s003_sh),
    Transport            = compute_delta_pct(VA_S004, va_s004_sh),
    Services             = compute_delta_pct(VA_S005, va_s005_sh),
    Energy               = compute_delta_pct(VA_S006, va_s006_sh)
  ) %>%
  pivot_longer(cols = -country, names_to = "sector", values_to = "delta_pct")

va_realloc <- bind_rows(tun_va_2050, mex_va_2050) %>%
  mutate(sector = factor(sector, levels = rev(SECTORS_COMMON)),
         sign   = ifelse(delta_pct >= 0, "positive", "negative"))

p_fig6 <- ggplot(va_realloc,
                 aes(x = sector, y = delta_pct, fill = sign)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_hline(yintercept = 0, color = "grey30", linewidth = 0.4) +
  geom_text(aes(label = sprintf("%+.1f%%", delta_pct),
                hjust = ifelse(delta_pct >= 0, -0.1, 1.1)),
            size = 3.2, color = "grey20") +
  coord_flip() +
  facet_wrap(~ country, ncol = 2) +
  scale_fill_manual(values = c("positive" = col_gain, "negative" = col_loss)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.25, 0.25))) +
  labs(
    title = "Sectoral value added in 2050 with respect to baseline",
    subtitle = "Tunisia and Mexico",
    caption = paste0(
      "Productive sectors: Tunisia = industry; ",
      "Mexico = agriculture + manufacturing + construction.\n",
      "Energy: Tunisia = fossil fuel transformation + electricity; ",
      "Mexico = energy sector.\n",
      "Scenario: ", snbc_label, " (Tunisia); Hacienda 2 (Mexico). ",
      "Source: ThreeME simulations."
    ),
    x = NULL,
    y = "Value added (% deviation from baseline)"
  ) +
  theme_wd()

ggsave(file.path(output_dir, "fig06_va_sectorielle.png"), p_fig6,
       width = 10, height = 5.5, dpi = 300)



# ═══════════════════════════════════════════════════════════════════════════
# FIG 7 : Sectoral employment (2050)
# ═══════════════════════════════════════════════════════════════════════════

SECTORS_EMP <- c("Total", "Productive sectors", "Services", "Transport", "Energy")

tun_bl_2050 <- tun_baseline %>% filter(policy == tun_snbc_policy, year == horizon_2050)
tun_sh_2050 <- tun_shock    %>% filter(policy == tun_snbc_policy, year == horizon_2050)

tun_bl_emp <- c(
  tun_bl_2050$f_l_0,
  tun_bl_2050$f_l_ind_0,
  tun_bl_2050$f_l_ser_0,
  tun_bl_2050$f_l_trsp_0,
  tun_bl_2050$f_l_trsf_0 + tun_bl_2050$f_l_ele_0
)
tun_sh_emp <- c(
  tun_sh_2050$f_l_2,
  tun_sh_2050$f_l_ind_2,
  tun_sh_2050$f_l_ser_2,
  tun_sh_2050$f_l_trsp_2,
  tun_sh_2050$f_l_trsf_2 + tun_sh_2050$f_l_ele_2
)

tun_emp_sect <- tibble(
  sector    = SECTORS_EMP,
  delta_pct = (tun_sh_emp - tun_bl_emp) / tun_bl_emp * 100,
  country   = "(a) Tunisia"
)

mex_bl_2050 <- mex_baseline %>% filter(year == horizon_2050)
mex_sh_2050 <- mex_shock    %>% filter(year == horizon_2050)

mex_bl_emp <- c(
  mex_bl_2050$F_L,
  mex_bl_2050$F_L_S001 + mex_bl_2050$F_L_S002 + mex_bl_2050$F_L_S003,
  mex_bl_2050$F_L_S005,
  mex_bl_2050$F_L_S004,
  mex_bl_2050$F_L_S006
)
mex_sh_emp <- c(
  mex_sh_2050$F_L,
  mex_sh_2050$F_L_S001 + mex_sh_2050$F_L_S002 + mex_sh_2050$F_L_S003,
  mex_sh_2050$F_L_S005,
  mex_sh_2050$F_L_S004,
  mex_sh_2050$F_L_S006
)

mex_emp_sect <- tibble(
  sector    = SECTORS_EMP,
  delta_pct = (mex_sh_emp - mex_bl_emp) / mex_bl_emp * 100,
  country   = "(b) Mexico"
)

emp_realloc <- bind_rows(tun_emp_sect, mex_emp_sect) %>%
  mutate(sector = factor(sector, levels = rev(SECTORS_EMP)),
         sign   = ifelse(delta_pct >= 0, "positive", "negative"))

p_fig7 <- ggplot(emp_realloc,
                 aes(x = sector, y = delta_pct, fill = sign)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_hline(yintercept = 0, color = "grey30", linewidth = 0.4) +
  geom_text(aes(label = sprintf("%+.1f%%", delta_pct),
                hjust = ifelse(delta_pct >= 0, -0.1, 1.1)),
            size = 3.2, color = "grey20") +
  coord_flip() +
  facet_wrap(~ country, ncol = 2) +
  scale_fill_manual(values = c("positive" = col_gain, "negative" = col_loss)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.25, 0.25))) +
  labs(
    title = "Sectoral employment in 2050 with respect to baseline",
    subtitle = "Tunisia and Mexico",
    caption = paste0(
      "Deviation in %: (employment in the policy scenario - baseline employment) / baseline employment.\n",
      "Scenario: ", snbc_label, " (Tunisia); Hacienda 2 (Mexico). ",
      "Source: ThreeME simulations."
    ),
    x = NULL,
    y = "Employment (% deviation from baseline)"
  ) +
  theme_wd()

ggsave(file.path(output_dir, "fig07_emploi_sectoriel.png"), p_fig7,
       width = 10, height = 5.5, dpi = 300)



# ═══════════════════════════════════════════════════════════════════════════
# FIG 8 : GDP decomposition by demand components (stacked bars)
# ═══════════════════════════════════════════════════════════════════════════

build_gdp_decomp_tun <- function(yr) {
  bl <- tun_baseline %>% filter(policy == tun_snbc_policy, year == yr)
  sh <- tun_shock    %>% filter(policy == tun_snbc_policy, year == yr)
  gdp_bl <- bl$gdp_0
  tibble(
    year      = yr,
    country   = "(a) Tunisia",
    component = c("Household consumption", "Investment", "Exports", "Imports"),
    contrib   = c(
      (sh$ch_2 - bl$ch_0)  / gdp_bl * 100,
      (sh$i_2  - bl$i_0)   / gdp_bl * 100,
      (sh$x_2  - bl$x_0)   / gdp_bl * 100,
      -(sh$m_2 - bl$m_0)   / gdp_bl * 100
    )
  )
}

build_gdp_decomp_mex <- function(yr) {
  bl <- mex_baseline %>% filter(year == yr)
  sh <- mex_shock    %>% filter(year == yr)
  gdp_bl <- bl$GDP
  tibble(
    year      = yr,
    country   = "(b) Mexico",
    component = c("Household consumption", "Investment", "Exports", "Imports"),
    contrib   = c(
      (sh$CH - bl$CH) / gdp_bl * 100,
      (sh$I  - bl$I)  / gdp_bl * 100,
      (sh$X  - bl$X)  / gdp_bl * 100,
      -(sh$M - bl$M)  / gdp_bl * 100
    )
  )
}

decomp_data <- bind_rows(
  build_gdp_decomp_tun(horizon_2030), build_gdp_decomp_tun(horizon_2050),
  build_gdp_decomp_mex(horizon_2030), build_gdp_decomp_mex(horizon_2050)
) %>%
  mutate(
    component = factor(component,
                       levels = c("Household consumption", "Investment",
                                  "Exports", "Imports")),
    year_label = paste0(year)
  )

gdp_totals <- decomp_data %>%
  group_by(year, country, year_label) %>%
  summarise(total = sum(contrib), .groups = "drop")

p_fig8 <- ggplot(decomp_data,
                 aes(x = year_label, y = contrib, fill = component)) +
  geom_col(width = 0.6, position = "stack") +
  geom_hline(yintercept = 0, color = "grey30", linewidth = 0.4) +
  geom_point(data = gdp_totals,
             aes(x = year_label, y = total, fill = NULL),
             shape = 18, size = 4, color = "black", inherit.aes = FALSE,
             show.legend = FALSE) +
  geom_text(data = gdp_totals,
            aes(x = year_label, y = total, fill = NULL,
                label = sprintf("%+.2f%%", total)),
            vjust = ifelse(gdp_totals$total >= 0, -0.8, 1.8),
            size = 3.2, fontface = "bold", inherit.aes = FALSE) +
  facet_wrap(~ country, ncol = 2) +
  scale_fill_manual(values = demand_colors, name = NULL) +
  labs(
    title = "Contributions of demand components to the GDP deviation from baseline",
    subtitle = "Tunisia and Mexico, 2030 and 2050",
    caption = paste0(
      "Contribution of each demand component to the GDP deviation (% of baseline GDP).\n",
      "Black diamond = total GDP deviation. Imports enter with inverted sign (higher imports = negative effect).\n",
      "Scenario: ", snbc_label, " (Tunisia); Hacienda 2 (Mexico).\n",
      "Source: ThreeME simulations."
    ),
    x = NULL,
    y = "Contribution (% of baseline GDP)"
  ) +
  theme_wd()

ggsave(file.path(output_dir, "fig08_decomposition_pib.png"), p_fig8,
       width = 10, height = 6, dpi = 300)



# ═══════════════════════════════════════════════════════════════════════════
# FIG 9 : Aggregate investment by scenario (Tunisia, time series)
# ═══════════════════════════════════════════════════════════════════════════

tun_bl_inv <- tun_baseline %>%
  filter(policy == "CT4") %>%
  select(year, inv_bl = i_0)

tun_inv_scenarios <- tun_shock %>%
  filter(policy %in% names(policy_labels_all)) %>%
  select(year, policy, inv_sh = i_2) %>%
  inner_join(tun_bl_inv, by = "year") %>%
  mutate(
    delta_pct = compute_delta_pct(inv_bl, inv_sh),
    scenario  = factor(policy_labels_all[policy], levels = policy_labels_all)
  ) %>%
  filter(year >= 2020, year <= 2050)

p_fig9 <- ggplot(tun_inv_scenarios,
                 aes(x = year, y = delta_pct,
                     color = scenario, linetype = scenario)) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "grey50") +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = scenario_colors_all, name = NULL) +
  scale_linetype_manual(values = scenario_linetypes_all, name = NULL) +
  scale_x_continuous(breaks = seq(2020, 2050, by = 5)) +
  labs(
    title = "Investment variation with respect to baseline",
    subtitle = "Tunisia, by policy scenario",
    caption = "Source: ThreeME simulations.",
    x = NULL,
    y = "Investment (% deviation from baseline)"
  ) +
  theme_wd() +
  guides(color = guide_legend(nrow = 2), linetype = guide_legend(nrow = 2))

ggsave(file.path(output_dir, "fig09_investissement_scenarios.png"), p_fig9,
       width = 10, height = 6, dpi = 300)



# ═══════════════════════════════════════════════════════════════════════════
# FIG 10 : Investment by sector (horizontal bars, 2050, TUN / MEX)
# ═══════════════════════════════════════════════════════════════════════════

SECTORS_INV <- c("Total", "Industry", "Transport", "Services",
                 "Electricity", "Fossil fuel transf.")

tun_bl_inv_sect <- c(
  tun_bl_2050$i_0,
  tun_bl_2050$ia_ind_0,
  tun_bl_2050$ia_trsp_0,
  tun_bl_2050$ia_ser_0,
  tun_bl_2050$ia_ele_0,
  tun_bl_2050$ia_trsf_0
)
tun_sh_inv_sect <- c(
  tun_sh_2050$i_2,
  tun_sh_2050$ia_ind_2,
  tun_sh_2050$ia_trsp_2,
  tun_sh_2050$ia_ser_2,
  tun_sh_2050$ia_ele_2,
  tun_sh_2050$ia_trsf_2
)

tun_inv_sect_df <- tibble(
  sector    = SECTORS_INV,
  delta_pct = (tun_sh_inv_sect - tun_bl_inv_sect) / tun_bl_inv_sect * 100,
  country   = "(a) Tunisia"
)

SECTORS_INV_MEX <- c("Total", "Agriculture", "Manufacturing", "Construction",
                     "Transport", "Services", "Energy")

mex_bl_inv_sect <- c(
  mex_bl_2050$I,
  mex_bl_2050$I_S001,
  mex_bl_2050$I_S002,
  mex_bl_2050$I_S003,
  mex_bl_2050$I_S004,
  mex_bl_2050$I_S005,
  mex_bl_2050$I_S006
)
mex_sh_inv_sect <- c(
  mex_sh_2050$I,
  mex_sh_2050$I_S001,
  mex_sh_2050$I_S002,
  mex_sh_2050$I_S003,
  mex_sh_2050$I_S004,
  mex_sh_2050$I_S005,
  mex_sh_2050$I_S006
)

mex_inv_sect_df <- tibble(
  sector    = SECTORS_INV_MEX,
  delta_pct = (mex_sh_inv_sect - mex_bl_inv_sect) / mex_bl_inv_sect * 100,
  country   = "(b) Mexico"
)

p_fig10a <- ggplot(tun_inv_sect_df %>%
                     mutate(sector = factor(sector, levels = rev(SECTORS_INV)),
                            sign = ifelse(delta_pct >= 0, "positive", "negative")),
                   aes(x = sector, y = delta_pct, fill = sign)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_hline(yintercept = 0, color = "grey30", linewidth = 0.4) +
  geom_text(aes(label = sprintf("%+.1f%%", delta_pct),
                hjust = ifelse(delta_pct >= 0, -0.1, 1.1)),
            size = 3.2, color = "grey20") +
  coord_flip() +
  scale_fill_manual(values = c("positive" = col_gain, "negative" = col_loss)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.25, 0.25))) +
  labs(subtitle = "(a) Tunisia", x = NULL, y = NULL) +
  theme_wd() +
  theme(plot.subtitle = element_text(face = "bold", size = 11, color = "black", hjust = 0.5))

p_fig10b <- ggplot(mex_inv_sect_df %>%
                     mutate(sector = factor(sector, levels = rev(SECTORS_INV_MEX)),
                            sign = ifelse(delta_pct >= 0, "positive", "negative")),
                   aes(x = sector, y = delta_pct, fill = sign)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_hline(yintercept = 0, color = "grey30", linewidth = 0.4) +
  geom_text(aes(label = sprintf("%+.1f%%", delta_pct),
                hjust = ifelse(delta_pct >= 0, -0.1, 1.1)),
            size = 3.2, color = "grey20") +
  coord_flip() +
  scale_fill_manual(values = c("positive" = col_gain, "negative" = col_loss)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.25, 0.25))) +
  labs(subtitle = "(b) Mexico", x = NULL, y = NULL) +
  theme_wd() +
  theme(plot.subtitle = element_text(face = "bold", size = 11, color = "black", hjust = 0.5))

p_fig10 <- p_fig10a + p_fig10b +
  plot_annotation(
    title = "Sectoral investment in 2050 with respect to baseline",
    subtitle = "Tunisia and Mexico",
    caption = paste0(
      "Deviation in % of sectoral investment (2050) from baseline.\n",
      "Scenario: ", snbc_label, " (Tunisia); Hacienda 2 (Mexico).\n",
      "Source: ThreeME simulations."
    ),
    theme = theme(
      plot.title    = element_text(face = "bold", size = 13, hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5, color = "grey35"),
      plot.caption  = element_text(size = 9, hjust = 0.5, color = "grey45")
    )
  )

ggsave(file.path(output_dir, "fig10_investissement_sectoriel.png"), p_fig10,
       width = 10, height = 5.5, dpi = 300)



# ═══════════════════════════════════════════════════════════════════════════
# FIG 11 : Energy consumption by sector (Tunisia, levels in toe)
# ═══════════════════════════════════════════════════════════════════════════

energy_sectors <- c("Industry", "Transport", "Services",
                    "Electricity", "Fossil fuel transf.", "Households")

build_energy_tun <- function(df, suffix, scen_label) {
  ci_ind  <- paste0("ci_toe_ind_", suffix)
  ci_trsp <- paste0("ci_toe_trsp_", suffix)
  ci_ser  <- paste0("ci_toe_ser_", suffix)
  ci_ele  <- paste0("ci_toe_ele_", suffix)
  ci_trsf <- paste0("ci_toe_trsf_", suffix)
  ch_toe  <- paste0("ch_toe_", suffix)

  df %>%
    filter(policy == tun_snbc_policy) %>%
    transmute(
      year,
      Industry              = .data[[ci_ind]],
      Transport             = .data[[ci_trsp]],
      Services              = .data[[ci_ser]],
      Electricity           = .data[[ci_ele]],
      `Fossil fuel transf.` = .data[[ci_trsf]],
      Households            = .data[[ch_toe]]
    ) %>%
    pivot_longer(-year, names_to = "sector", values_to = "toe") %>%
    mutate(scenario = scen_label)
}

energy_tun <- bind_rows(
  build_energy_tun(tun_baseline, "0", "Baseline"),
  build_energy_tun(tun_shock,    "2", snbc_short)
) %>%
  filter(year %in% c(2020, 2030, 2050)) %>%
  mutate(
    sector = factor(sector, levels = energy_sectors),
    x_label = paste0(scenario, "\n", year)
  )

x_order <- c(
  paste0("Baseline\n", c(2020, 2030, 2050)),
  paste0(snbc_short, "\n", c(2020, 2030, 2050))
)
energy_tun <- energy_tun %>%
  mutate(x_label = factor(x_label, levels = x_order))

p_fig11 <- ggplot(energy_tun, aes(x = x_label, y = toe, fill = sector)) +
  geom_col(width = 0.65, position = "stack") +
  scale_fill_manual(values = sector_colors, name = NULL) +
  labs(
    title = "Final energy consumption by sector",
    subtitle = "Tunisia, NLCS vs baseline",
    caption = paste0(
      "Intermediate energy consumption and household energy consumption, in tonnes of oil equivalent (toe).\n",
      "Tunisia only; sectoral data not available for Mexico.\n",
      "Source: ThreeME simulations."
    ),
    x = NULL,
    y = "Energy consumption (toe)"
  ) +
  theme_wd()

ggsave(file.path(output_dir, "fig11_energie_sectorielle.png"), p_fig11,
       width = 10, height = 6, dpi = 300)



# ═══════════════════════════════════════════════════════════════════════════
# FIG 12 : CO2 emissions by sector (TUN: sectors; MEX: sources)
# ═══════════════════════════════════════════════════════════════════════════

# 12a : Tunisia, CO2 emissions by sector (levels)
ems_sectors <- c("Industry", "Transport", "Services",
                 "Electricity", "Fossil fuel transf.", "Households")

build_ems_tun <- function(df, suffix, scen_label) {
  df %>%
    filter(policy == tun_snbc_policy) %>%
    transmute(
      year,
      Industry              = .data[[paste0("ems_ci_co2_ind_", suffix)]],
      Transport             = .data[[paste0("ems_ci_co2_trsp_", suffix)]],
      Services              = .data[[paste0("ems_ci_co2_ser_", suffix)]],
      Electricity           = .data[[paste0("ems_ci_co2_ele_", suffix)]],
      `Fossil fuel transf.` = .data[[paste0("ems_ci_co2_trsf_", suffix)]],
      Households            = .data[[paste0("ems_ch_co2_", suffix)]]
    ) %>%
    pivot_longer(-year, names_to = "sector", values_to = "ems") %>%
    mutate(scenario = scen_label)
}

ems_tun <- bind_rows(
  build_ems_tun(tun_baseline, "0", "Baseline"),
  build_ems_tun(tun_shock,    "2", snbc_short)
) %>%
  filter(year %in% c(2020, 2030, 2050)) %>%
  mutate(
    sector  = factor(sector, levels = ems_sectors),
    x_label = factor(paste0(scenario, "\n", year), levels = x_order)
  )

p_fig12a <- ggplot(ems_tun, aes(x = x_label, y = ems, fill = sector)) +
  geom_col(width = 0.65, position = "stack") +
  scale_fill_manual(values = sector_colors, name = NULL) +
  labs(
    subtitle = "(a) Tunisia, by sector",
    x = NULL,
    y = "CO2 emissions (MtCO2)"
  ) +
  theme_wd() +
  theme(plot.subtitle = element_text(face = "bold", size = 11, color = "black", hjust = 0.5))

# 12b : Mexico, emissions by source (CI, CH, MAT, Y)
mex_ems_sources <- c("Intermediate cons. (CI)", "Households (CH)",
                     "Materials (MAT)", "Production (Y)")

build_ems_mex <- function(df, scen_label) {
  df %>%
    transmute(
      year,
      `Intermediate cons. (CI)` = EMS_CI,
      `Households (CH)`         = EMS_CH,
      `Materials (MAT)`         = EMS_MAT,
      `Production (Y)`          = EMS_Y
    ) %>%
    pivot_longer(-year, names_to = "source", values_to = "ems") %>%
    mutate(scenario = scen_label)
}

ems_mex <- bind_rows(
  build_ems_mex(mex_baseline, "Baseline"),
  build_ems_mex(mex_shock,    "Hacienda 2")
) %>%
  filter(year %in% c(2020, 2030, 2050)) %>%
  mutate(
    source  = factor(source, levels = mex_ems_sources),
    x_label = factor(paste0(scenario, "\n", year),
                     levels = c(paste0("Baseline\n", c(2020, 2030, 2050)),
                                paste0("Hacienda 2\n", c(2020, 2030, 2050))))
  )

mex_source_colors <- c(
  "Intermediate cons. (CI)" = "#2C3E50",
  "Households (CH)"         = "#C0392B",
  "Materials (MAT)"         = "#D4AC0D",
  "Production (Y)"          = "#2980B9"
)

p_fig12b <- ggplot(ems_mex, aes(x = x_label, y = ems, fill = source)) +
  geom_col(width = 0.65, position = "stack") +
  scale_fill_manual(values = mex_source_colors, name = NULL) +
  labs(
    subtitle = "(b) Mexico, by source",
    x = NULL,
    y = "CO2 emissions (MtCO2)"
  ) +
  theme_wd() +
  theme(plot.subtitle = element_text(face = "bold", size = 11, color = "black", hjust = 0.5))

p_fig12 <- p_fig12a / p_fig12b +
  plot_annotation(
    title = "CO2 emissions by sector and by source",
    subtitle = "Tunisia (by sector) and Mexico (by source)",
    caption = paste0(
      "Scenario: ", snbc_label, " (Tunisia); Hacienda 2 (Mexico).\n",
      "Source: ThreeME simulations."
    ),
    theme = theme(
      plot.title    = element_text(face = "bold", size = 13, hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5, color = "grey35"),
      plot.caption  = element_text(size = 9, hjust = 0.5, color = "grey45")
    )
  )

ggsave(file.path(output_dir, "fig12_co2_sectoriel.png"), p_fig12,
       width = 10, height = 10, dpi = 300)



# ═══════════════════════════════════════════════════════════════════════════
# FIG 13 : Trade balance (TUN / MEX)
# ═══════════════════════════════════════════════════════════════════════════

tun_trade <- tun_baseline %>%
  filter(policy == tun_snbc_policy) %>%
  select(year, bl = rbal_trade_val_0) %>%
  inner_join(
    tun_shock %>% filter(policy == tun_snbc_policy) %>%
      select(year, sh = rbal_trade_val_2),
    by = "year"
  ) %>%
  transmute(year,
            Baseline = bl * 100,
            !!snbc_label := sh * 100) %>%
  pivot_longer(-year, names_to = "scenario", values_to = "trade_bal") %>%
  mutate(country = "(a) Tunisia")

mex_trade <- mex_baseline %>%
  select(year, bl = RBAL_TRADE_VAL) %>%
  inner_join(
    mex_shock %>% select(year, sh = RBAL_TRADE_VAL),
    by = "year"
  ) %>%
  transmute(year,
            Baseline     = bl * 100,
            `Hacienda 2` = sh * 100) %>%
  pivot_longer(-year, names_to = "scenario", values_to = "trade_bal") %>%
  mutate(country = "(b) Mexico")

trade_data <- bind_rows(tun_trade, mex_trade) %>%
  filter(year >= 2020, year <= 2050)

p_fig13 <- ggplot(trade_data, aes(x = year, y = trade_bal,
                                   color = scenario, linetype = scenario)) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "grey50") +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ country, ncol = 2, scales = "free_y") +
  scale_color_manual(values = c("Baseline" = "grey50",
                                 setNames(col_tunisia, snbc_label),
                                 "Hacienda 2" = col_mexico),
                     name = NULL) +
  scale_linetype_manual(values = c("Baseline" = "dashed",
                                    setNames("solid", snbc_label),
                                    "Hacienda 2" = "solid"),
                        name = NULL) +
  scale_x_continuous(breaks = seq(2020, 2050, by = 5)) +
  labs(
    title = "Trade balance",
    subtitle = "Tunisia and Mexico, % of GDP",
    caption = paste0(
      "Trade balance in % of GDP.\n",
      "Source: ThreeME simulations."
    ),
    x = NULL,
    y = "Trade balance (% of GDP)"
  ) +
  theme_wd()

ggsave(file.path(output_dir, "fig13_balance_commerciale.png"), p_fig13,
       width = 10, height = 5.5, dpi = 300)




