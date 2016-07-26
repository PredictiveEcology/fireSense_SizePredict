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
    defineParameter(name = "newData", class = "character", default = NA, desc = "optionally, a character vector indicating the name(s) of object(s) accessible in the sim environment, in which to look for variables with which to predict. Objects can be data.frame(s), RasterLayer(s), or RasterStack(s) (for time series). If omitted, the fitted values are used.")
  ), #It can be a data.frame or a list of rasters (or stacks if its a time series).
  inputObjects = data.frame(
    objectName = "fireSense_SizeFit",
    objectClass = "fireSense_SizeFit",
    sourceURL = "",
    other = NA_character_,
    stringsAsFactors = FALSE
  ),
  outputObjects = data.frame(
    objectName = c("fireSense_SizePredictBeta", "fireSense_SizePredictTheta"),
    objectClass = c("ANY", "ANY"),
    other = NA_character_,
    stringsAsFactors = FALSE
  )
))

## Toolbox: set of functions used internally by the module
  ## Predict functions
    fireSense_SizePredictBetaRaster <- function(model, data, sim) {
      sim$fireSense_SizePredictBeta <- sim$fireSense_SizeFit$formula$beta %>%
        terms.formula %>%
        delete.response %>%
        model.matrix(data) %>%
        `%*%` (sim$fireSense_SizeFit$coefBeta) %>%
        drop %>% sim$fireSense_SizeFit$linkFunBeta$linkinv(.)
    }
    
    fireSense_SizePredictThetaRaster <- function(model, data, sim) {
      sim$fireSense_SizePredictTheta <- sim$fireSense_SizeFit$formula$theta %>%
        terms.formula %>%
        delete.response %>%
        model.matrix(data) %>%
        `%*%` (sim$fireSense_SizeFit$coefTheta) %>%
        drop %>% sim$fireSense_SizeFit$linkFunTheta$linkinv(.)
    }
    
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
  
  ## Note: is.na() is temporary and should be replaced by is.null in the future
  stopifnot(!is.null(params(sim)$fireSense_SizePredict$newData[1]))
  
  if (is.na(params(sim)$fireSense_SizePredict$newData[1])) {
    
    sim$fireSense_SizePredictBeta <- sim$fireSense_SizeFit$fitted.values$beta
    sim$fireSense_SizePredictTheta <- sim$fireSense_SizeFit$fitted.values$theta
    
  } else if (length(params(sim)$fireSense_SizePredict$newData) == 1L) {
    
    if (is.data.frame(sim[[params(sim)$fireSense_SizePredict$newData]])) {
      
      sim$fireSense_SizePredictBeta <- sim$fireSense_SizeFit$formula$beta %>%
        model.matrix(sim[[params(sim)$fireSense_SizePredict$newData]]) %>%
        `%*%` (sim$fireSense_SizeFit$coefBeta) %>%
        drop %>% sim$fireSense_SizeFit$linkFunBeta$linkinv(.)
      sim$fireSense_SizePredictTheta <- sim$fireSense_SizeFit$formula$theta %>%
        model.matrix(sim[[params(sim)$fireSense_SizePredict$newData]]) %>%
        `%*%` (sim$fireSense_SizeFit$coefTheta) %>%
        drop %>% sim$fireSense_SizeFit$linkFunTheta$linkinv(.)
      
    } else if (is(sim[[params(sim)$fireSense_SizePredict$newData]], "RasterStack")) {
      
      sim$fireSense_SizePredictBeta <- sim[[params(sim)$fireSense_SizePredict$newData]] %>%
        unstack %>% 
        lapply(function(x, sim) 
          predict(setNames(x, params(sim)$fireSense_SizePredict$newData), 
                  model = sim$fireSense_SizeFit, fun = fireSense_SizePredictBetaRaster, 
                  na.rm = TRUE, sim = sim), sim = sim) %>%
        stack
      
      sim$fireSense_SizePredictTheta <- sim[[params(sim)$fireSense_SizePredict$newData]] %>%
        unstack %>% 
        lapply(function(x, sim) 
          predict(setNames(x, params(sim)$fireSense_SizePredict$newData), 
                  model = sim$fireSense_SizeFit, fun = fireSense_SizePredictThetaRaster, 
                  na.rm = TRUE, sim = sim), sim = sim) %>%
        stack
    
    } else if (is(sim[[params(sim)$fireSense_SizePredict$newData]], "RasterLayer")) {
      
      sim$fireSense_SizePredictBeta <- 
        predict(sim[[params(sim)$fireSense_SizePredict$newData]], model = sim$fireSense_SizeFit, fun = fireSense_SizePredictBetaRaster, na.rm = TRUE, sim = sim)
      sim$fireSense_SizePredictTheta <- 
        predict(sim[[params(sim)$fireSense_SizePredict$newData]], model = sim$fireSense_SizeFit, fun = fireSense_SizePredictThetaRaster, na.rm = TRUE, sim = sim)
      
    } else {
      stop(paste0("fireSense_SizePredict> '", params(sim)$fireSense_SizePredict$newData, "' is not a 'data.frame', a 'RasterLayer' or a 'RasterStack'."))
    }
  } else {

    if (all(unlist(lapply(params(sim)$fireSense_SizePredict$newData, function(x) is.data.frame(sim[[x]]))))) {

      sim$fireSense_SizePredictBeta <- sim$fireSense_SizeFit$formula$beta %>%
        model.matrix(dplyr::bind_cols(mget(params(sim)$fireSense_SizePredict$newData, envir = envir(sim), inherits = FALSE))) %>%
        `%*%` (sim$fireSense_SizeFit$coefBeta) %>%
        drop %>% sim$fireSense_SizeFit$linkFunBeta$linkinv(.)
      sim$fireSense_SizePredictTheta <- sim$fireSense_SizeFit$formula$theta %>%
        model.matrix(dplyr::bind_cols(mget(params(sim)$fireSense_SizePredict$newData, envir = envir(sim), inherits = FALSE))) %>%
        `%*%` (sim$fireSense_SizeFit$coefTheta) %>%
        drop %>% sim$fireSense_SizeFit$linkFunTheta$linkinv(.)
      
    } else if (all(unlist(lapply(params(sim)$fireSense_SizePredict$newData, function(x) is(sim[[x]], "RasterStack"))))) {

      sim$fireSense_SizePredictBeta <- 
        mget(params(sim)$fireSense_SizePredict$newData, envir = envir(sim), inherits = FALSE) %>%
        lapply(unstack) %>%
        c(list(FUN = function(...) stack(list(...)), SIMPLIFY = FALSE)) %>%
        do.call("mapply", args = .) %>%
        lapply(function(x, sim)
          predict(x, model = sim$fireSense_SizeFit, fun = fireSense_SizePredictBetaRaster, 
                  na.rm = TRUE, sim = sim), sim = sim) %>%
        stack

      sim$fireSense_SizePredictTheta <- 
        mget(params(sim)$fireSense_SizePredict$newData, envir = envir(sim), inherits = FALSE) %>%
        lapply(unstack) %>%
        c(list(FUN = function(...) stack(list(...)), SIMPLIFY = FALSE)) %>%
        do.call("mapply", args = .) %>%
        lapply(function(x, sim)
          predict(x, model = sim$fireSense_SizeFit, fun = fireSense_SizePredictThetaRaster, 
                  na.rm = TRUE, sim = sim), sim = sim) %>%
        stack
      
    } else if (all(unlist(lapply(params(sim)$fireSense_SizePredict$newData, function(x) is(sim[[x]], "RasterLayer"))))) {
      
      sim$fireSense_SizePredictBeta <- 
        predict(stack(mget(params(sim)$fireSense_SizePredict$newData, envir = envir(sim), inherits = FALSE)), model = sim$fireSense_SizeFit, fun = fireSense_SizePredictBetaRaster, na.rm = TRUE, sim = sim)
      sim$fireSense_SizePredictTheta <- 
        predict(stack(mget(params(sim)$fireSense_SizePredict$newData, envir = envir(sim), inherits = FALSE)), model = sim$fireSense_SizeFit, fun = fireSense_SizePredictThetaRaster, na.rm = TRUE, sim = sim)
      
    } else {
      stop(paste0("fireSense_SizePredict> '", 
        paste(params(sim)$fireSense_SizePredict$newData[unlist(lapply(params(sim)$fireSense_SizePredict, 
          function(x) is.data.frame(x) || is(x, "RasterLayer") || is(x, "RasterStack")))], collapse = ","),
        "' is not a 'data.frame', a 'RasterLayer' or a 'RasterStack'."))
    }
  }
  
  invisible(sim)
}
