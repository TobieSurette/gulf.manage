library(gulf.data)
library(gulf.spatial)
library(gulf.graphics)

year <- 1998
# Extract NAFO area 4T bounds:
p <- read.gulf.spatial("nafo")
p <- p[unlist(lapply(p, function(x) x$label)) == "4T"][[1]]
p <- as.polygon(p$longitude, p$latitude)

# Read text file:
file <- paste0(options()$gulf.path$snow.crab$root, "/Databases/Temperatures/TEMP_3D_Sep_", year, ".llv")
y <- readLines(file)
y <- y[-1]

# Parse data:
res <- data.frame(longitude = rep(NA, length(y)),
                  latitude = rep(NA, length(y)),
                  depth = rep(NA, length(y)),
                  temperature = rep(NA, length(y)))
for (i in 1:100){
   ix <- ((i-1)*500000 + 1):(i*500000)
   ix <- ix[ix <= length(y)]
   if (length(ix) > 0){
      cat(paste0(i, ") Parsing records ", min(ix), " to ", max(ix), ".\n"))
      tmp <- strsplit(y[ix], " ")
      res$longitude[ix]   <- as.numeric(unlist(lapply(tmp, function(x) x[1])))
      res$latitude[ix]    <- as.numeric(unlist(lapply(tmp, function(x) x[2])))
      res$depth[ix]       <- as.numeric(unlist(lapply(tmp, function(x) x[3])))
      res$temperature[ix] <- as.numeric(unlist(lapply(tmp, function(x) x[5])))
   }
}

# Remove points outside NAFO area 4T (slow):
ix <- (res$longitude >= min(p[[1]]$x)) & (res$longitude <= max(p[[1]]$x)) & (res$latitude >= min(p[[1]]$y)) & (res$latitude <= max(p[[1]]$y))
ix <- ix & res$latitude <= 49 & res$longitude >= -67
y <- res[ix, ]
ix <- in.polygon(p, y$longitude, y$latitude)
y <- y[ix, ]

library(akima)

# Define interpolation grid:
xx <- seq(-67, -60, by = 0.025)
yy <- seq(45.5, 49, by = 0.02)
grid <- expand.grid(xx, yy)
names(grid) <- c("longitude", "latitude")
grid$depth <- depth(grid$longitude, grid$latitude)
depth <- grid$depth
dim(depth) <- c(length(xx), length(yy))
   
# Re-interpolate data on a regular grid:
clg()
dd <- sort(unique(y$depth))
dd <- dd[dd <= 350]
zz <- array(NA, dim = c(length(xx), length(yy), length(dd)))
dimnames(zz) <- list(longitude = xx, latitude = yy, depth = dd)
for (i in 1:length(dd)){
   cat(paste0("Interpolating depth layer : ", dd[i], " meters.\n"))
   rr <- y[y$depth == dd[i], ]
   zz[,,i] <- akima::interp(rr$longitude, rr$latitude, rr$temperature, xo = xx, yo = yy, extrap = FALSE, duplicate = "mean")$z
   
   # Apply depth mask to remove spurious extrapolations:
   tmp <- zz[,,i]
   tmp[depth < dd[i]] <- NA
   zz[,,i] <- tmp
   rm(tmp)
   
   # Plot sample map:
   if ((((i-1) %% 20) == 0) & (i < 100)){
      image(xx, yy, zz[,,i])
      map("coast")
      mtext(paste0("Depth = ", dd[i]), 3, 0.5, cex = 1.5)
   }
}

# Write to file:
x <- zz
save(x, file = paste0("//ent.dfo-mpo.ca/dfo-mpo/GROUP/GLF/Regional_Shares/AquaRes_Common/Crab/Databases/Temperatures/water.temperature.september.", year, ".rdata"))
save(x, file = paste0(gsub("manage", "data", getwd()), "/inst/extdata/water.temperature.september.", year, ".rdata"))

