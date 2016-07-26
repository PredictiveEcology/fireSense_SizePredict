# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "fireSense_SizePredict",
  description = "Make predictions from a model fitted using fireSense_SizeFit.",
  keywords = c("fire size distribution", "tapered Pareto", "fireSense", "statistical model", "predict"),
  authors=c(person("Jean", "Marchal", email="jean.d.marchal@gmail.com", role=c("aut", "cre"))),
  childModules = character(),
  version = numeric_version("0.0.0.9000"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = NA_character_, # e.g., "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "fireSense_SizePredict.Rmd"),
  reqdPkgs = list("dplyr", "magrittr", "raster"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", default, min, max, "parameter description")),
    defineParameter(name = "newData", class = "character", default = "", desc = "optionally, the name of a data.frame, a RasterStack (or a list of RasterStacks for time series) loaded as inputs during the simInit, in which to look for variables with which to predict. If omitted, the fitted values are used.")
  ), #It can be a data.frame or a list of rasters (or stacks if its a time series).
  inputObjects = data.frame(
    objectName = "fireSense_SizeFit",
    objectClass = "fireSense_SizeFit",
    sourceURL = "",
    other = NA_character_,
    stringsAsFactors = FALSE
  ),
  outputObjects = data.frame(
    objectName = c("fireSense_SizeBeta", "fireSense_SizeTheta"),
    objectClass = c("ANY", "ANY"),
    other = NA_character_,
    stringsAsFactors = FALSE
  )
))

## Toolbox: set of functions used internally by the module
  ## Predict function
    setMethod(f = "predict",
              signature = "fireSense_SizeFit",
              definition = function(object, data, sim){
                browser()
              }
    )

## event types
#   - type `init` is required for initialiazation

doEvent.fireSense_SizePredict = function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    ### check for more detailed object dependencies:
    ### (use `checkObject` or similar)

    # do stuff for this event
    sim <- sim$fireSense_SizePredictInit(sim)

  } else {
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  }
  
  invisible(sim)
}

## event functions
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
fireSense_SizePredictInit <- function(sim) {
  
  if (is.null(params(sim)$fireSense_SizePredict$newData)) {
    sim$fireSense_SizeFit$fitted.values
  } else {
    if (is.data.frame(sim[[params(sim)$fireSense_SizePredict$newData]])) {
      data.frame(beta = 
                   model.matrix(sim$fireSense_SizeFit$formula$beta, sim[[params(sim)$fireSense_SizePredict$newData]]) %>%
                   . %*% sim$fireSense_SizeFit$coefBeta %>%
                   drop %>%
                   sim$fireSense_SizeFit$linkFunTheta$linkinv,
                 theta = 
                   model.matrix(sim$fireSense_SizeFit$formula$theta, sim[[params(sim)$fireSense_SizePredict$newData]]) %>%
                   . %*% sim$fireSense_SizeFit$coefTheta %>%
                   drop %>%
                   sim$fireSense_SizeFit$linkFunTheta$linkinv)
    } else if (is(sim[[params(sim)$fireSense_SizePredict$newData]], "RasterStack")) {
      predict(sim[[params(sim)$fireSense_SizePredict$newData]], model = sim$fireSense_SizeFit, fun = predict, na.rm = TRUE)
    } else {
      stop(paste0("fireSense_SizePredict> '", params(sim)$fireSense_SizePredict$newData, "' is not a 'data.frame', or a list of 'RasterLayer' or 'RasterStack'."))
    }
  }

  invisible(sim)
}
