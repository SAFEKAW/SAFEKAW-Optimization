source('StakeholderMeetingTradeoffs/R_app/data_prep_app.R')
source('StakeholderMeetingTradeoffs/models_app/wq_refined_predict.R')
source('StakeholderMeetingTradeoffs/global.R')

# create samples ----------------------------------------------------------
require(lhs)

n_samples_init <- 20000
lhs_init <- randomLHS(n_samples_init,7)
share_sums <- rowSums(lhs_init[,1:3])
lhs_trim <- lhs_init[share_sums <= 1,]
n_samples <- nrow(lhs_trim)

ranges <- list(
  corn_range <- 0:100,
  soy_range <- 0:100,
  sor_range <- 0:100,
  cult_area_range <- -30:30,
  fert_range <- -10:10,
  irr_area_range <- -30:30,
  irr_eff_range <- -10:10
)

range_mins <- sapply(ranges, min)
range_lengths <- sapply(ranges, length)-1

samples_scale <- lhs_trim * range_lengths[col(lhs_trim)]
samples <- samples_scale + range_mins[col(samples_scale)]
colnames(samples) <- c('corn', 'soy', 'sor', 'cult_area', 'fert_eff', 'irr_area', 'irr_eff')

print(paste0(n_samples,'/',n_samples_init,' samples retained'))
#benchmarks suggested that the model takes about 300ms to run, or 3x per second
print(paste('Est. time to run:', round(n_samples/3/60), 'minutes'))

# run samples through model -----------------------------------------------
sample_summary <- data.frame(sample = 1:nrow(samples)) %>%
  cbind(samples)

eval_sample <- function(s){
  x <- c(s['corn'], s['soy'], s['sor'])/100
  
  policy <- list(
    cult_area_factor = 1 + s['cult_area']/100,
    
    fert_factor = 1 - s['fert_eff'] / 100,
    fert_factor = max(0, 1 - s['fert_eff'] / 100),
    fert_gamma  = 0.1,   # keep fixed for now (or make a slider later)
    
    irrig_frac_factor = 1 + s['irr_area']/100,
    irr_eff = 1 + s['irr_eff']/100
  )
  
  out <- eval_candidate_all_years(
    x = x,
    years_vec = years_vec,
    df_opt = df_opt,
    wq_model_mlr = wq_model_mlr,
    wq_base_by_year = wq_base_by_year,
    lu_baseline = lu_baseline,
    universal_costs = universal_costs,
    crop_params = crop_params,
    baseline_irrig_frac = baseline_irrig_frac,   
    hist_mix = hist_mix,
    fert_ref_by_year = fert_ref_by_year,
    policy = policy
  )
  out[2] = -1*out[2]
  names(out) <- c('n','prof','irr')
  return(out)
}
model_output <- t(apply(samples,1,eval_sample))

sample_summary <- cbind(sample_summary, model_output)

#save the results if wanted
# write_csv(sample_summary, 'StakeholderMeetingTradeoffs/sample_summary.csv')


# compare baseline outputs to sampled -------------------------------------
#read in previous results if wanted
# sample_summary <- read_csv('StakeholderMeetingTradeoffs/sample_summary.csv')

baseline_sample <- c(obs_x*100,0,0,0,0)
names(baseline_sample) <- colnames(samples)
baseline_output <- eval_sample(baseline_sample)

#nitrate
n_dist <- ecdf(sample_summary$n)
n_baseline_pct <- n_dist(baseline_output['n'])

plot(n_dist, ann = F)
abline(h = n_baseline_pct, col = 'red', lty = 'dashed')
abline(v = baseline_output['n'], col = 'red', lty = 'dashed')
title(main = paste0('Baseline Nitrate: ',round(n_baseline_pct,3)*100,'th percentile'),
      ylab = 'Percentile',
      xlab = 'N Export')

#profits
prof_dist <- ecdf(sample_summary$prof)
prof_baseline_pct <- prof_dist(baseline_output['prof'])

plot(prof_dist, ann = F)
abline(h = prof_baseline_pct, col = 'red', lty = 'dashed')
abline(v = baseline_output['prof'], col = 'red', lty = 'dashed')
title(main = paste0('Baseline Profits: ',round(prof_baseline_pct,3)*100,'th percentile'),
      ylab = 'Percentile',
      xlab = 'Net Profits')

#irrigation
irr_dist <- ecdf(sample_summary$irr)
irr_baseline_pct <- irr_dist(baseline_output['irr'])

plot(irr_dist, ann = F)
abline(h = irr_baseline_pct, col = 'red', lty = 'dashed')
abline(v = baseline_output['irr'], col = 'red', lty = 'dashed')
title(main = paste0('Baseline Irrigation: ',round(irr_baseline_pct,3)*100,'th percentile'),
      ylab = 'Percentile',
      xlab = 'Irrigation Use')


pct_sum <- (1-n_baseline_pct) + prof_baseline_pct + (1-irr_baseline_pct)

(1-n_baseline_pct)/pct_sum
prof_baseline_pct/pct_sum
(1-irr_baseline_pct)/pct_sum
