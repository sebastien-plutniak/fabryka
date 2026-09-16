spatial_benn <- function(xyz, nearest, maximum_distance = NA) {
      benn <- base::matrix(NA,
                           nrow = base::nrow(xyz), ncol = 2,
                           dimnames = base::list(base::rownames(xyz), base::c("elongation", "isotropy"))
      )
      
      # For each artifact, get the nearest artifacts and compute Benn values
      for (k in seq_len(nrow(xyz))) {
        centerx <- xyz$X1[k]
        centery <- xyz$Y1[k]
        centerz <- xyz$Z1[k]
        d <- base::sqrt((centerx - xyz$X1)^2 + (centery - xyz$Y1)^2 + (centerz - xyz$Z1)^2)
        sorted_pos <- base::order(d)
        if (!base::is.na(maximum_distance)) sorted_pos <- sorted_pos[d[sorted_pos] <= maximum_distance]
        xyz_subsample <- xyz[sorted_pos[1:nearest], ]
        benn[k, ] <- benn(xyz_subsample, min_sample = nearest)[, base::c("EL", "IS")]
      }
      
      # Calculate where the points would fall on Benn diagram so colors can be assigned
      b <- benn_coords(base::cbind(elongation = benn[, "elongation"], isotropy = benn[, "isotropy"]))
      xp <- b[, 1]
      yp <- b[, 2]
      
      base::return(base::list(benn))
    }