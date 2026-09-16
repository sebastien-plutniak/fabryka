benn <- function(xyz, level = "All Points", min_sample) {
  benn <- base::matrix(
    nrow = base::length(base::unique(level)), ncol = 6,
    dimnames = base::list(base::unique(level), base::c("N", "E1", "E2", "E3", "IS", "EL"))
  )
  
  for (l in unique(level)) {
    xyz_level <- base::subset(xyz, level == l)
    
    if (nrow(xyz_level) < min_sample) {
      benn[l, ] <- base::c(nrow(xyz_level), base::rep(NA, 5))
    } else {
      # Normalize and compute eigen values
      e <- eigen_values(vector_normals(xyz_level))
      
      # Compute shape indices for Benn Diagram
      isotropy <- e$values[3] / e$values[1]
      elongation <- 1 - (e$values[2] / e$values[1])
      
      benn[l, ] <- base::c(nrow(xyz_level), e$values[1], e$values[2], e$values[3], isotropy, elongation)
    }
  }
  
  return(benn)
}