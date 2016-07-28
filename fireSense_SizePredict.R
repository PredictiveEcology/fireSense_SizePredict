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
    defineParameter(name = "data", class = "character", default = NA, desc = "optional. A character vector
      indicating the name(s) of object(s) present in the sim environment, in which to look for variables
      with which to predict. Objects can be data.frame(s), named list(s) of RasterLayer(s), or named list(s)
      of RasterStack(s) (for time series) but objects of different classes cannot be mixed. If omitted, or if
      variables are not found in the data objects(s), they are searched in the sim environment."),
    defineParameter(name = "mapping", class = "character", default = NA, desc = "optional. Named character
      vector to map variable names in the formula to those in the data object(s). Names of unmapped
      variables are used directly to look for variables in data object(s) or in the sim environment.")
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

      sim$fireSense_SizePredictBeta <- model %>%
        model.matrix(data) %>%
        `%*%` (sim$fireSense_SizeFit$coefBeta) %>%
        drop %>% sim$fireSense_SizeFit$linkFunBeta$linkinv(.)
    }
    
    fireSense_SizePredictThetaRaster <- function(model, data, sim) {
      sim$fireSense_SizePredictTheta <- model %>%
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
  stopifnot(!is.null(params(sim)$fireSense_SizePredict$data[1]))
  stopifnot(!is.null(params(sim)$fireSense_SizePredict$mapping[1]))

  dataEnv <- new.env(parent = envir(sim))
  on.exit(rm(dataEnv))
  list2env(as.list(envir(sim)), envir = dataEnv)
  
  if (!is.na(params(sim)$fireSense_SizePredict$data[1]))
    lapply(params(sim)$fireSense_SizePredict$data, function(x, dataEnv) list2env(sim[[x]], envir = dataEnv), dataEnv = dataEnv)

  termsBeta <- delete.response(terms.formula(formulaBeta <- sim$fireSense_SizeFit$formula$beta))
  termsTheta <- delete.response(terms.formula(formulaTheta <- sim$fireSense_SizeFit$formula$theta))
  
  if (!is.na(params(sim)$fireSense_SizePredict$mapping[1])) {

    for (i in 1:length(params(sim)$fireSense_SizePredict$mapping)) {
      
      attr(termsBeta, "term.labels") <- gsub(pattern = names(params(sim)$fireSense_SizePredict$mapping[i]),
                                             replacement = params(sim)$fireSense_SizePredict$mapping[i], x = attr(termsBeta, "term.labels"))
      attr(termsTheta, "term.labels") <- gsub(pattern = names(params(sim)$fireSense_SizePredict$mapping[i]),
                                              replacement = params(sim)$fireSense_SizePredict$mapping[i], x = attr(termsTheta, "term.labels"))
    }
  
  }

  formulaBeta <- reformulate(attr(termsBeta, "term.labels"), response = NULL, attr(termsBeta, "intercept"))
  formulaTheta <- reformulate(attr(termsTheta, "term.labels"), response = NULL, attr(termsTheta, "intercept"))
  
  varsBeta <- all.vars(formulaBeta)
  varsTheta <- all.vars(formulaTheta)
  allVars <- unique(c(varsBeta, varsTheta))
  
  if (all(unlist(lapply(allVars, function(x) is.vector(dataEnv[[x]]))))) {
    
    sim$fireSense_SizePredictBeta <- formulaBeta %>%
      model.matrix(dataEnv) %>%
      `%*%` (sim$fireSense_SizeFit$coefBeta) %>%
      drop %>% sim$fireSense_SizeFit$linkFunBeta$linkinv(.)
    sim$fireSense_SizePredictTheta <- formulaTheta %>%
      model.matrix(dataEnv) %>%
      `%*%` (sim$fireSense_SizeFit$coefTheta) %>%
      drop %>% sim$fireSense_SizeFit$linkFunTheta$linkinv(.)
    
  } else if (all(unlist(lapply(allVars, function(x) is(dataEnv[[x]], "RasterStack"))))) {
    
    sim$fireSense_SizePredictBeta <- mget(varsBeta, envir = envir(sim), inherits = FALSE) %>%
      lapply(unstack) %>%
      c(list(FUN = function(...) stack(list(...)), SIMPLIFY = FALSE)) %>%
      do.call("mapply", args = .) %>%
      lapply(function(x, sim)
        predict(x, model = formulaBeta, fun = fireSense_SizePredictBetaRaster, 
                na.rm = TRUE, sim = sim), sim = sim) %>%
      stack
    
    sim$fireSense_SizePredictTheta <- mget(varsTheta, envir = envir(sim), inherits = FALSE) %>%
      lapply(unstack) %>%
      c(list(FUN = function(...) stack(list(...)), SIMPLIFY = FALSE)) %>%
      do.call("mapply", args = .) %>%
      lapply(function(x, sim)
        predict(x, model = formulaTheta, fun = fireSense_SizePredictThetaRaster, 
                na.rm = TRUE, sim = sim), sim = sim) %>%
      stack
    
  } else if (all(unlist(lapply(allVars, function(x) is(dataEnv[[x]], "RasterLayer"))))) {
    
    sim$fireSense_SizePredictBeta <- mget(varsBeta, envir = envir(sim), inherits = FALSE) %>%
      stack %>% predict(model = formulaBeta, fun = fireSense_SizePredictBetaRaster, na.rm = TRUE, sim = sim)
    sim$fireSense_SizePredictTheta <- mget(varsTheta, envir = envir(sim), inherits = FALSE) %>%
      stack %>% predict(model = formulaTheta, fun = fireSense_SizePredictThetaRaster, na.rm = TRUE, sim = sim)
    
  } else {
    
    varsExist <- allVars %in% ls(dataEnv)
    varsClass <- unlist(lapply(allVars, function(x) is.data.frame(dataEnv[[x]]) || is(dataEnv[[x]], "RasterLayer") || is(dataEnv[[x]], "RasterStack")))
    
    if (any(!varsExist)) {
      stop(paste0("fireSense_SizePredict> Variable '", varsExist[which(!varsExist)[1L]], "' not found."))
    } else if (any(varsTypes)) {
      stop("fireSense_SizePredict> Variables are not of the same class.")
    } else {
      stop(paste0("fireSense_SizePredict> Variable '", 
                  allVars[which(varsClass)[1L]],
                  "' is not a data.frame, a RasterLayer, or a RasterStack."))
    }
  }

  invisible(sim)
}
