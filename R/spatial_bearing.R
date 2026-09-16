spatial_bearing <- function(xyz, nearest) {
  # Make a place to hold the computed mean bearings of nearest neighbors
  near_avg_bearing_p <- base::vector(mode = "numeric", length = base::nrow(xyz))
  near_avg_bearing_Rbar <- base::vector(mode = "numeric", length = base::nrow(xyz))
  
  # Go through each artifact, get the nearest (default is 39), and compute mean bearing angle
  for (k in seq_len(nrow(xyz))) {
    centerx <- xyz$X1[k]
    centery <- xyz$Y1[k]
    centerz <- xyz$Z1[k]
    d <- base::sqrt((centerx - xyz$X1)^2 + (centery - xyz$Y1)^2 + (centerz - xyz$Z1)^2)
    xyz_subsample <- xyz[base::order(d)[seq_len(nearest)], ]
    
    # Get the mean bearing angle, test significance, and mean plunge angle of this subset of artifacts
    near_avg_bearing_p[k] <- base::round(CircStats::r.test(2 * xyz_subsample$orientation_pi, degree = TRUE)$p.value, 2)
    near_avg_bearing_Rbar[k] <- base::round(CircStats::r.test(2 * xyz_subsample$orientation_pi, degree = TRUE)$r.bar, 2)
  }
  
  # Color coding based on average bearing and on average plunge (higher plunge angles are more less saturated - i.e. more white)
  base::return(base::list(L = near_avg_bearing_Rbar * 100, R.p = near_avg_bearing_p))
}
