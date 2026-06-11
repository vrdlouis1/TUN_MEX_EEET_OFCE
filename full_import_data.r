# =========================
# PACKAGES
# =========================
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(tibble)

# =========================
# 0) HELPERS (shared)
# =========================

# Safe Excel reader with error handling
safe_read_excel <- function(path, sheet) {
  tryCatch(
    read_excel(path, sheet = sheet, .name_repair = "unique"),
    error = \(e) NULL
  )
}

# Convert Excel date columns to years and transform to long format
to_year_wide_exact <- function(df) {
  df <- select(df, -matches("^\\.\\.\\.[0-9]+$"))
  nms <- names(df)
  date_col <- nms[which(str_trim(nms) == "@date")][1]
  if (is.na(date_col)) date_col <- nms[1]
  
  # Convert numeric column names to years
  old_names <- names(df)
  nums <- suppressWarnings(as.numeric(old_names[-1]))
  if (length(nums) > 0 && all(!is.na(nums))) {
    names(df) <- c(
      old_names[1],
      format(as.Date(nums, origin = "1899-12-30"), "%Y")
    )
  }
  
  df %>%
    rename(variable = all_of(date_col)) %>%
    pivot_longer(
      -variable,
      names_to = "year",
      values_to = "value",
      values_transform = list(value = as.numeric)
    ) %>%
    mutate(year = as.integer(year)) %>%
    pivot_wider(names_from = variable, values_from = value) %>%
    arrange(year)
}

# Merge main and reporting data, keeping reporting values on overlap
merge_keep_reporting <- function(main_df, rep_df) {
  if (is.null(rep_df)) return(main_df)
  overlap <- intersect(
    setdiff(names(main_df), "year"),
    setdiff(names(rep_df), "year")
  )
  main_df %>%
    select(-all_of(overlap)) %>%
    left_join(rep_df, by = "year")
}

# Extract variable names from final dataframe
vars_from_df <- function(df, id_cols) {
  tibble(variable = setdiff(names(df), id_cols)) %>%
    filter(variable != "year") %>%
    distinct() %>%
    arrange(variable)
}

# Merge additional variables into an existing df (no persistent extra columns)
# - If overlapping variables exist, keep main values and fill missing with "add" values.
merge_additional_vars <- function(main_df, add_df, id_cols) {
  if (is.null(add_df) || nrow(add_df) == 0) return(main_df)
  
  overlap <- intersect(
    setdiff(names(main_df), id_cols),
    setdiff(names(add_df), id_cols)
  )
  
  if (length(overlap) == 0) {
    return(left_join(main_df, add_df, by = id_cols))
  }
  
  out <- left_join(main_df, add_df, by = id_cols, suffix = c("", "_add"))
  
  for (v in overlap) {
    add_name <- paste0(v, "_add")
    if (add_name %in% names(out)) {
      out[[v]] <- dplyr::coalesce(out[[v]], out[[add_name]])
      out[[add_name]] <- NULL
    }
  }
  
  out
}

# =========================
# 1) TUNISIA — MAIN WORKBOOKS (Baseline vs Shock) + reporting merge
# =========================

scenarios_TUN <- tribble(
  ~country, ~policy, ~redis, ~path, ~sheet_base, ~sheet_shock,
  ~sheet_rep_base, ~sheet_rep_shock,
  
  "Tunisie", "CT4", "avecRedis",
  "data/Tunisie/2_10-13_22-24-04_CT4_REDIS2.xlsx",
  "Baseline_SUB", "Shock_SUB",
  "reporting_base", "reporting_shock",
  
  "Tunisie", "CT4", "sansRedis",
  "data/Tunisie/1_10-28_10-51-28_Result_Tunisie_CT4_sansRedis2.xlsx",
  "Baseline_SUB", "Shock_SUB",
  "reporting_base", "reporting_shock",
  
  "Tunisie", "SUB23", "avecRedis",
  "data/Tunisie/4_10-21_15-16-45_Result_Tunisie_SUB23_avecRedis.xlsx",
  "Baseline_SUB", "Shock_SUB",
  "reporting_base", "reporting_shock",
  
  "Tunisie", "SUB23", "sansRedis",
  "data/Tunisie/3_10-21_15-32-47_Result_Tunisie_SUB23_sansRedis.xlsx",
  "Baseline_SUB", "Shock_SUB",
  "reporting_base", "reporting_shock",
  
  "Tunisie", "ENR_elec_only", "NA",
  "data/Tunisie/5_10-13_09-31-19_Result_Tunisie_ENR_elec_only.xlsx",
  "Baseline_SUB", "Shock_SUB",
  "reporting_base", "reporting_shock",
  
  "Tunisie", "CT4_SUB23_ENRelec", "avecRedis",
  "data/Tunisie/6_10-15_17-50-37_Result_Tunisie_CT4_SUB23_ENRelec_REDIS2.xlsx",
  "Baseline_SUB", "Shock_SUB",
  "reporting_base", "reporting_shock"
)

read_tun_scenario <- function(country, policy, redis, path, sheet_main,
                              sheet_reporting, scenario) {
  
  main_df <- safe_read_excel(path, sheet_main) %>%
    to_year_wide_exact()
  
  if (is.null(main_df)) {
    stop(sprintf("Sheet '%s' not found in: %s", sheet_main, path))
  }
  
  rep_df <- safe_read_excel(path, sheet_reporting) %>%
    to_year_wide_exact()
  
  # Align policy naming with your current standard
  policy_name <- if_else(
    redis == "avecRedis",
    paste0(policy, "_redis"),
    policy
  )
  
  merge_keep_reporting(main_df, rep_df) %>%
    mutate(
      country = country,
      policy  = policy_name,
      scenario = scenario
    ) %>%
    relocate(country, policy, scenario, year)
}

all_baseline_TUN <- pmap_dfr(
  scenarios_TUN %>%
    select(
      country, policy, redis, path,
      sheet_main = sheet_base,
      sheet_reporting = sheet_rep_base
    ),
  \(...) read_tun_scenario(..., scenario = "Baseline")
)

all_shock_TUN <- pmap_dfr(
  scenarios_TUN %>%
    select(
      country, policy, redis, path,
      sheet_main = sheet_shock,
      sheet_reporting = sheet_rep_shock
    ),
  \(...) read_tun_scenario(..., scenario = "Shock")
)

# =========================
# 2) TUNISIA — DISAGGREGATED WORKBOOK (import + merge INTO main Tunisia dfs)
# =========================

path_TUN_DISAGG <- "data/Tunisie/Sectors and commodities_MissingData copie.xlsx"

# Label sheets (kept, useful for mapping codes to labels later)
sectors_TUN <- safe_read_excel(path_TUN_DISAGG, sheet = "Sectors") %>%
  rename_with(str_trim)

commodities_TUN <- safe_read_excel(path_TUN_DISAGG, sheet = "Commodities") %>%
  rename_with(str_trim)

clean_disagg_df <- function(df) {
  df %>%
    select(-matches("^\\.\\.\\.[0-9]+$")) %>%
    rename_with(str_trim)
}

# Keep only vars with a given suffix (e.g. "_0" for baseline, "_2" for shock)
# and optionally keep variables with no suffix (pwd_coil, etc.)
keep_suffix_vars <- function(df, suffix, keep_no_suffix = TRUE) {
  stopifnot("year" %in% names(df))
  
  meta <- c("country", "policy", "scenario", "year")
  cols <- names(df)
  
  # columns that end with suffix
  suff_cols <- cols[str_detect(cols, paste0(stringr::fixed(suffix), "$"))]
  
  if (keep_no_suffix) {
    # "no suffix" = does not end with _0 or _2 (or generally _<digit>)
    nosuff_cols <- cols[!str_detect(cols, "_[0-9]$")]
  } else {
    nosuff_cols <- character(0)
  }
  
  keep <- unique(c(intersect(meta, cols), suff_cols, nosuff_cols))
  df %>% select(all_of(keep))
}

# From MAIN sheet: Scenario | Description | Year | 2015 | 2016 | ...
to_year_wide_disagg_main <- function(df) {
  df <- clean_disagg_df(df)
  
  nms <- names(df)
  scenario_col <- nms[which(tolower(nms) == "scenario")][1]
  yearcol_col  <- nms[which(tolower(nms) == "year")][1]   # contains variable names
  descr_col    <- nms[which(tolower(nms) %in% c("description", "desc"))][1]
  
  if (is.na(scenario_col) || is.na(yearcol_col)) {
    stop("Disagg MAIN sheet: cannot find required columns 'Scenario' and 'Year'. Check headers.")
  }
  
  id_cols <- c(scenario_col, yearcol_col)
  if (!is.na(descr_col)) id_cols <- c(id_cols, descr_col)
  
  df %>%
    pivot_longer(
      cols = -all_of(id_cols),
      names_to = "year",
      values_to = "value",
      values_transform = list(value = as.numeric)
    ) %>%
    mutate(
      year = suppressWarnings(as.integer(year)),
      variable = .data[[yearcol_col]]
    ) %>%
    select(all_of(scenario_col), year, variable, value) %>%
    pivot_wider(names_from = variable, values_from = value) %>%
    arrange(year) %>%
    rename(Scenario = all_of(scenario_col))
}

# From CI sheet: Year | 2015 | 2016 | ...
to_year_wide_disagg_ci <- function(df) {
  df <- clean_disagg_df(df)
  
  nms <- names(df)
  yearcol_col <- nms[which(tolower(nms) == "year")][1]
  if (is.na(yearcol_col)) {
    stop("Disagg CI sheet: cannot find required column 'Year'. Check headers.")
  }
  
  df %>%
    pivot_longer(
      cols = -all_of(yearcol_col),
      names_to = "year",
      values_to = "value",
      values_transform = list(value = as.numeric)
    ) %>%
    mutate(
      year = suppressWarnings(as.integer(year)),
      variable = .data[[yearcol_col]]
    ) %>%
    select(year, variable, value) %>%
    pivot_wider(names_from = variable, values_from = value) %>%
    arrange(year)
}

split_baseline_shock <- function(df_with_scenario) {
  df_with_scenario %>%
    mutate(
      scenario = case_when(
        str_detect(str_to_lower(Scenario), "baseline") ~ "Baseline",
        str_detect(str_to_lower(Scenario), "shock")    ~ "Shock",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(scenario)) %>%
    select(-Scenario)
}

# Read one PAIR: main sheet + CI sheet
# Baseline: keep *_0; Shock: keep *_2
# IMPORTANT: output is aligned with the "main Tunisia" standard: country, policy, scenario, year + vars
read_disagg_pair <- function(path, sheet_main, sheet_ci, policy, redis,
                             keep_no_suffix = TRUE) {
  
  # policy naming aligned with main Tunisia datasets
  policy_name <- if_else(redis == "avecRedis", paste0(policy, "_redis"), policy)
  
  # MAIN
  main_raw <- read_excel(path, sheet = sheet_main, .name_repair = "unique")
  main_wide <- main_raw %>%
    to_year_wide_disagg_main() %>%
    split_baseline_shock()
  
  main_baseline <- main_wide %>% filter(scenario == "Baseline")
  main_shock    <- main_wide %>% filter(scenario == "Shock")
  
  # CI (no scenario inside)
  ci_raw  <- read_excel(path, sheet = sheet_ci, .name_repair = "unique")
  ci_wide <- ci_raw %>% to_year_wide_disagg_ci()
  
  # join CI onto each scenario by year, then keep suffix vars
  baseline <- main_baseline %>%
    left_join(ci_wide, by = "year") %>%
    mutate(country = "Tunisie", policy = policy_name) %>%
    relocate(country, policy, scenario, year) %>%
    keep_suffix_vars("_0", keep_no_suffix = keep_no_suffix)
  
  shock <- main_shock %>%
    left_join(ci_wide, by = "year") %>%
    mutate(country = "Tunisie", policy = policy_name) %>%
    relocate(country, policy, scenario, year) %>%
    keep_suffix_vars("_2", keep_no_suffix = keep_no_suffix)
  
  list(baseline = baseline, shock = shock)
}

disagg_pairs_TUN <- tibble::tribble(
  ~policy,            ~redis,        ~sheet_main,             ~sheet_ci,
  "CT4",              "sansRedis",   "CT4sansRedis",          "CI_CT4sansRedis",
  "CT4",              "avecRedis",   "CT4 avec Redis",        "CI_CT4 avec Redis",
  "SUB23",            "sansRedis",   "Subv sansRedis",        "CI_Subv sansRedis",
  "SUB23",            "avecRedis",   "Subv avec Redis",       "CI_Subv avec Redis",
  "ENR_elec_only",    "NA",          "ENR only",              "CI_ENR only",
  "CT4_SUB23_ENRelec","avecRedis",   "All avec redis",        "CI_All avec redis"
)

disagg_results_TUN <- pmap(
  disagg_pairs_TUN,
  \(policy, redis, sheet_main, sheet_ci) {
    cat("\n[DISAGG] importing:", policy, redis, "|", sheet_main, "+", sheet_ci, "\n")
    read_disagg_pair(
      path = path_TUN_DISAGG,
      sheet_main = sheet_main,
      sheet_ci = sheet_ci,
      policy = policy,
      redis = redis,
      keep_no_suffix = TRUE
    )
  }
)

all_baseline_TUN_DISAGG <- map_dfr(disagg_results_TUN, "baseline")
all_shock_TUN_DISAGG    <- map_dfr(disagg_results_TUN, "shock")

# Optional: keep by-pair structure (still useful for debugging / inspection)
disagg_by_pair_TUN <- set_names(
  disagg_results_TUN,
  paste0(disagg_pairs_TUN$policy, "__", disagg_pairs_TUN$redis)
)

# ---- MERGE disaggregated variables INTO the main Tunisia datasets ----
id_cols_TUN_join <- c("country", "policy", "scenario", "year")

all_baseline_TUN <- merge_additional_vars(all_baseline_TUN, all_baseline_TUN_DISAGG, id_cols_TUN_join)
all_shock_TUN    <- merge_additional_vars(all_shock_TUN,    all_shock_TUN_DISAGG,    id_cols_TUN_join)

# =========================
# 3) MEXICO — WORKBOOKS
# =========================

process_mex <- function(sheet_name, scenario) {
  read_excel(
    "data/Mexique/threeme_results_MEX_meriem.xlsx",
    sheet = sheet_name
  ) %>%
    mutate(country = "Mexique", scenario = scenario) %>%
    relocate(country, scenario, year)
}

all_baseline_MEX <- process_mex("baseline", "Baseline")
all_shock_MEX    <- process_mex("shock", "Shock")

tax_carbone_MEX <- read_excel(
  "data/Mexique/Tax_Carbone_mexique.xlsx",
  sheet = "TAX_LONG"
)

variables_MEX <- read_excel(
  "data/Mexique/threeme_results_MEX_meriem.xlsx",
  sheet = "variables",
  col_types = c("text", "text", "skip")
)

# =========================
# 4) VARIABLES LISTS (after merge)
# =========================

id_cols_TUN <- c("country", "policy", "scenario", "year")
id_cols_MEX <- c("country", "scenario", "year")

vars_TUN_all <- bind_rows(
  vars_from_df(all_baseline_TUN, id_cols_TUN) %>%
    mutate(country = "Tunisie", dataset = "Baseline"),
  vars_from_df(all_shock_TUN, id_cols_TUN) %>%
    mutate(country = "Tunisie", dataset = "Shock")
) %>%
  distinct(country, dataset, variable) %>%
  arrange(country, dataset, variable)

vars_MEX_all <- bind_rows(
  vars_from_df(all_baseline_MEX, id_cols_MEX) %>%
    mutate(country = "Mexique", dataset = "Baseline"),
  vars_from_df(all_shock_MEX, id_cols_MEX) %>%
    mutate(country = "Mexique", dataset = "Shock")
) %>%
  distinct(country, dataset, variable) %>%
  arrange(country, dataset, variable)

vars_all <- bind_rows(vars_TUN_all, vars_MEX_all) %>%
  arrange(country, dataset, variable)

# =========================
# 5) SAVE OUTPUTS
# =========================

out_dir <- "data/import_result"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

walk2(
  list(
    # variable dictionaries
    vars_TUN_all, vars_MEX_all, vars_all,
    
    # main results (Tunisia already merged with disagg)
    all_baseline_TUN, all_shock_TUN,
    all_baseline_MEX, all_shock_MEX,
    
    # Mexico extras
    tax_carbone_MEX, variables_MEX,
    
    # Tunisia disagg extras (kept, but not required for analysis scripts)
    sectors_TUN, commodities_TUN,
    all_baseline_TUN_DISAGG, all_shock_TUN_DISAGG,
    disagg_by_pair_TUN
  ),
  c(
    "vars_TUN_all", "vars_MEX_all", "vars_all_TUN_MEX",
    
    "all_baseline_TUN", "all_shock_TUN",
    "all_baseline_MEX", "all_shock_MEX",
    
    "tax_carbone_MEX", "variables_MEX",
    
    "sectors_TUN", "commodities_TUN",
    "all_baseline_TUN_DISAGG", "all_shock_TUN_DISAGG",
    "disagg_by_pair_TUN"
  ),
  \(data, name) saveRDS(data, file.path(out_dir, paste0(name, ".rds")))
)

cat("\nDone. Tunisia main datasets now include disaggregated variables (merged by country/policy/scenario/year).\n")

