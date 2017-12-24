library(magrittr)
library(PtProcess)
library(raster)
library(SpaDES)

set.seed(1)

modulePath <- normalizePath("..")

start <- end <- 1

# Define simulation parameters
times <- list(start = start, end = end, timeunit = "year")
modules <- list("fireSense_SizePredict")
paths <- list(
  modulePath = modulePath
)

# Create random weather and fire size data
  # data.frame
  dataObject <- data.frame(
    weather = rnorm(1000, 150, 30),
    size_ha = rtappareto(1000, .3, 1e4, a = 1)
  )
  
  # raster
  nx <- ny <- 100L
  size_ha <- raster(nrows = ny, ncols = nx, xmn = -nx/2, xmx = nx/2, ymn = -ny/2, ymx = ny/2) %>%
    setValues(rtappareto(ncell(.), .3, 1e4, a = 1))
  weather <- gaussMap(size_ha, scale = 300, var = 0.03, speedup = nx/5e2, inMemory = TRUE)
  dataObject <- stack(weather, size_ha) %>%
    setNames(c("weather", "size_ha"))

# Create a typical output of fireSense_SizeFit
fireSense_SizeFitted <- list(
  formula = list(beta = size_ha ~ weather2,
                 theta = size_ha ~ weather),
  a = 1,
  link = list(beta = make.link("log"),
              theta = make.link("identity")),
  coef = list(beta = setNames(c(1, 0.01), c("intercept", "weather2")),
              theta = setNames(c(1, 0.001), c("intercept", "weather")))
)
class(fireSense_SizeFitted) <- "fireSense_SizeFit"

# Define module parameters
parameters <- list(
  fireSense_SizePredict = list(
    mapping = list(weather2 = "weather"), # One can use mapping to map variables
                                          # in the formula of the fitted object
                                          # to those in data. Here weather2 
                                          # (formula) is mapped to weather (data).
    data = "dataObject"
  )
)

# Objects to pass from the global environment to the simList environment
objects <- c("dataObject", "fireSense_SizeFitted")

sim <- simInit(
  times = times, 
  params = parameters, 
  modules = modules, 
  objects = objects, 
  paths = paths
)

sim <- spades(sim)
sim$fireSense_SizePredicted
