allocate_irrigation <- function(crop_shares,
                                theta_irrig,
                                hist_mix = NULL,
                                mode = c("preserve_hist_mix", "uniform_by_crop")) {
  mode <- match.arg(mode)
  crop_shares <- crop_shares[!is.na(crop_shares)]
  theta_irrig <- max(0, min(1, theta_irrig))
  crop_shares[crop_shares < 0] <- 0
  
  if (sum(crop_shares) <= 0) {
    irrig  <- crop_shares * 0
    rainfed <- crop_shares * 0
    return(list(irrig = irrig, rainfed = rainfed))
  }
  
  crop_shares <- crop_shares / sum(crop_shares)
  
  if (mode == "uniform_by_crop" || is.null(hist_mix)) {
    irrig  <- crop_shares * theta_irrig
    rainfed <- crop_shares - irrig
  } else {
    if (!all(c("Crop", "irrig_frac") %in% names(hist_mix))) {
      irrig  <- crop_shares * theta_irrig
      rainfed <- crop_shares - irrig
    } else {
      hm <- hist_mix[match(names(crop_shares), hist_mix$Crop), , drop = FALSE]
      hist_frac <- hm$irrig_frac
      hist_frac[is.na(hist_frac)] <- 0
      hist_frac <- pmax(0, pmin(1, hist_frac))
      
      raw_irrig <- crop_shares * hist_frac
      
      if (sum(raw_irrig) <= 0) {
        irrig <- crop_shares * theta_irrig
      } else {
        scale_factor <- theta_irrig / sum(raw_irrig)
        irrig <- raw_irrig * scale_factor
        irrig <- pmin(irrig, crop_shares)
      }
      
      rainfed <- crop_shares - irrig
    }
  }
  
  list(irrig = irrig, rainfed = rainfed)
}
