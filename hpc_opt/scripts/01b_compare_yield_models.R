suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(dplyr)
  library(tidyr)
  library(lme4)
  library(performance)
  library(broom)
  library(broom.mixed)
  library(purrr)
})

source(here("hpc_opt","R","10_helpers.R"))
source(here("hpc_opt","R","12_irrigation_predict.R"))

# -----------------------------
# inputs
# -----------------------------
yrs_common <- 2006:2023

df_combined_county <- read_csv(
  here("hpc_opt","outputs","common_inputs_county_hist_baseline.csv"),
  show_col_types = FALSE
)

df_yield_obs <- read_csv(
  here("data","CropYieldData_County.csv"),
  show_col_types = FALSE
)

# if GDD is stored separately
df_gdd <- read_csv(
  here("data","gdd_all_gs_hist_baseline.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    FIPS = as.character(FIPS),
    Crop = recode(crop,
                  "Corn" = "Corn",
                  "Soybeans" = "Soybeans",
                  "Sorghum" = "Sorghum",
                  "Wheat" = "Wheat"
    )
  ) %>%
  select(FIPS, Year, Crop, GDD)

# existing irrigation model
irr_lm <- readRDS(here("hpc_opt","models","irr_lm.rds"))

# -----------------------------
# rebuild historical crop table
# -----------------------------
df_combined_county <- df_combined_county %>%
  filter(Year %in% yrs_common) %>%
  mutate(FIPS = as.character(FIPS))

df_yield_obs <- df_yield_obs %>%
  filter(Year %in% yrs_common) %>%
  mutate(FIPS = as.character(FIPS))

irr_aug <- predict_irrigation_depth(
  df_all = df_combined_county,
  irrigation_lm = irr_lm,
  irrigated_crops = c("Corn", "Soybeans"),
  require_irrigated_mgmt = TRUE
)

df_crop <- irr_aug %>%
  filter(!is.na(Crop), Year %in% yrs_common) %>%
  mutate(
    irrigation_WaterUse_m = if ("WaterManagement" %in% names(.)) {
      if_else(
        WaterManagement %in% c("Rainfed", "Non-Irrigated"),
        0,
        coalesce(irrigation_WaterUse_m, 0)
      )
    } else {
      coalesce(irrigation_WaterUse_m, 0)
    },
    totalWater_m = precip_m + irrigation_WaterUse_m
  ) %>%
  left_join(
    df_yield_obs %>% mutate(FIPS = as.character(FIPS)),
    by = c("Year", "FIPS", "Crop")
  ) %>%
  left_join(df_gdd, by = c("Year", "FIPS", "Crop")) %>% rename(GDD = GDD.x)%>%
  filter(is.finite(totalWater_m), is.finite(GDD)) 

if ("area_ha" %in% names(df_crop)) {
  area_ha_thres <- 64.75 * 10
  df_crop <- df_crop %>% filter(area_ha > area_ha_thres)
}

# -----------------------------
# helper
# -----------------------------
calc_metrics <- function(obs, pred) {
  tibble(
    RMSE = sqrt(mean((obs - pred)^2, na.rm = TRUE)),
    MAE  = mean(abs(obs - pred), na.rm = TRUE),
    R2   = cor(obs, pred, use = "complete.obs")^2
  )
}

fit_compare_one <- function(dat, response) {
  dat <- dat %>% filter(is.finite(.data[[response]]))
  
  f_lm <- as.formula(
    paste0(response, " ~ poly(totalWater_m, 2, raw = TRUE) + GDD")
  )
  
  f_lmer <- as.formula(
    paste0(response, " ~ poly(totalWater_m, 2, raw = TRUE) + GDD + (1|FIPS) + (1|Year)")
  )
  
  m_lm <- lm(f_lm, data = dat)
  m_lmer <- lmer(f_lmer, data = dat, REML = FALSE)
  
  dat_pred <- dat %>%
    mutate(
      pred_lm         = predict(m_lm, newdata = dat),
      pred_lmer_full  = predict(m_lmer, newdata = dat, re.form = NULL),
      pred_lmer_fixed = predict(m_lmer, newdata = dat, re.form = NA)
    )
  
  metrics <- bind_rows(
    calc_metrics(dat_pred[[response]], dat_pred$pred_lm) %>% mutate(model = "lm"),
    calc_metrics(dat_pred[[response]], dat_pred$pred_lmer_full) %>% mutate(model = "lmer_full"),
    calc_metrics(dat_pred[[response]], dat_pred$pred_lmer_fixed) %>% mutate(model = "lmer_fixed")
  )
  
  list(
    metrics = metrics,
    lm_model = m_lm,
    lmer_model = m_lmer,
    fixed_lm = broom::tidy(m_lm),
    fixed_lmer = broom.mixed::tidy(m_lmer, effects = "fixed"),
    r2 = performance::r2_nakagawa(m_lmer),
    pred = dat_pred
  )
}

# -----------------------------
# run by crop and response
# -----------------------------
responses <- c("yield_kgHa_detrended", "yield_kcalHa_detrended")
crops <- sort(unique(df_crop$Crop))

results <- list()

for (cr in crops) {
  dat_cr <- df_crop %>% filter(Crop == cr)
  for (resp in responses) {
    key <- paste(cr, resp, sep = "__")
    results[[key]] <- fit_compare_one(dat_cr, resp)
  }
}

# metrics table
metrics_all <- bind_rows(
  lapply(names(results), function(nm) {
    parts <- strsplit(nm, "__")[[1]]
    results[[nm]]$metrics %>%
      mutate(Crop = parts[1], Response = parts[2])
  })
)

dir.create(here("hpc_opt","outputs","model_checks"), recursive = TRUE, showWarnings = FALSE)
write_csv(metrics_all, here("hpc_opt","outputs","model_checks","yield_model_comparison.csv"))

print(metrics_all)



coef_compare <- bind_rows(
  lapply(names(results), function(nm) {
    parts <- strsplit(nm, "__")[[1]]
    crop <- parts[1]
    response <- parts[2]
    
    lm_tab <- broom::tidy(results[[nm]]$lm_model) %>%
      transmute(
        Crop = crop,
        Response = response,
        model = "lm",
        term,
        estimate,
        std.error,
        statistic,
        p.value
      )
    
    lmer_tab <- broom.mixed::tidy(results[[nm]]$lmer_model, effects = "fixed") %>%
      transmute(
        Crop = crop,
        Response = response,
        model = "lmer",
        term,
        estimate,
        std.error,
        statistic = NA_real_,   # broom.mixed may not always return these consistently
        p.value = NA_real_
      )
    
    bind_rows(lm_tab, lmer_tab)
  })
)


print(coef_compare)


coef_compare_wide <- coef_compare %>%
  select(Crop, Response, model, term, estimate, std.error) %>%
  pivot_wider(
    names_from = model,
    values_from = c(estimate, std.error)
  ) %>%
  mutate(
    estimate_diff = estimate_lmer - estimate_lm,
    abs_estimate_diff = abs(estimate_diff)
  ) %>%
  arrange(Response, Crop, term)


print(coef_compare_wide)


coef_compare_wide <- coef_compare_wide %>%
  mutate(
    pct_diff_vs_lm = if_else(
      is.finite(estimate_lm) & abs(estimate_lm) > 1e-8,
      100 * (estimate_lmer - estimate_lm) / estimate_lm,
      NA_real_
    )
  )
