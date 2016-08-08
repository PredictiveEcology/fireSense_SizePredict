# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "fireSense_SizePredict",
  description = "Make predictions from a model fitted using fireSense_SizeFit.",
  keywords = c("fire size distribution", "tapered Pareto", "fireSense", "statistical model", "predict"),
  authors=c(person("Jean", "Marchal", email="jean.d.marchal@gmail.com", role=c("aut", "cre"))),
  childModules = character(),
  version = numeric_version("1.2.0.9000"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = NA_character_, # e.g., "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "fireSense_SizePredict.Rmd"),
  reqdPkgs = list("magrittr", "raster"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", default, min, max, "parameter description")),
    defineParameter(name = "data", class = "character", default = NULL,
      desc = "optional. A character vector indicating the names of objects present in the sim environment, in which
              to look for variables with which to predict. Objects can be data.frames, named lists of RasterLayers,
              or named lists of RasterStacks (for times series). However, objects of different classes cannot be
              mixed. For example, variables cannot be searched simultaneously within an object of class data.frame
              and within an object of class RasterLayer. If omitted, or if variables are not found in the data objects,
              variables are searched in the sim environment."),
    defineParameter(name = "mapping", class = "character", default = NULL,
      desc = "optional. Named character vector to map variable names in the formula to those in the data objects.
              Names of unmapped variables are used directly to look for variables in data objects or in the sim environment."),
    defineParameter(name = "initialRunTime", class = "numeric", default = NA,
      desc = "optional. Simulation time at which to start this module. If omitted, start at start(simList)."),
    defineParameter(name = "intervalRunModule", class = "numeric", default = NA, desc = "optional. Interval in simulation time units between two module runs.")
  ),
  inputObjects = data.frame(
    objectName = "fireSense_SizeFitted",
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

      model %>%
        model.matrix(data) %>%
        `%*%` (sim$fireSense_SizeFitted$coefBeta) %>%
        drop %>% sim$fireSense_SizeFitted$linkFunBeta$linkinv(.)

    }

    fireSense_SizePredictThetaRaster <- function(model, data, sim) {

      model %>%
        model.matrix(data) %>%
        `%*%` (sim$fireSense_SizeFitted$coefTheta) %>%
        drop %>% sim$fireSense_SizeFitted$linkFunTheta$linkinv(.)

    }

## event types
#   - type `init` is required for initialiazation

doEvent.fireSense_SizePredict = function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {

    sim <- sim$fireSense_SizePredictInit(sim)

  } else if (eventType == "run") {

    sim <- sim$fireSense_SizePredictRun(sim)

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

  sim <- scheduleEvent(sim, eventTime = if (is.na(p(sim)$initialRunTime)) start(sim) else p(sim)$initialRunTime, "fireSense_SizePredict", "run")
  sim

}

fireSense_SizePredictRun <- function(sim) {

  envData <- new.env(parent = envir(sim))
  on.exit(rm(envData))
  list2env(as.list(envir(sim)), envir = envData)

  if (!is.null(p(sim)$data))
    lapply(p(sim)$data, function(x, envData) if (is.list(sim[[x]])) list2env(sim[[x]], envir = envData), envData = envData)

  termsBeta <- delete.response(terms.formula(formulaBeta <- sim$fireSense_SizeFitted$formula$beta))
  termsTheta <- delete.response(terms.formula(formulaTheta <- sim$fireSense_SizeFitted$formula$theta))

  ## Mapping variables names to data
  if (!is.null(p(sim)$mapping)) {

    for (i in 1:length(p(sim)$mapping)) {

      attr(termsBeta, "term.labels") <- gsub(pattern = names(p(sim)$mapping[i]),
                                             replacement = p(sim)$mapping[i], x = attr(termsBeta, "term.labels"))
      attr(termsTheta, "term.labels") <- gsub(pattern = names(p(sim)$mapping[i]),
                                              replacement = p(sim)$mapping[i], x = attr(termsTheta, "term.labels"))
    }

  }

  formulaBeta <- reformulate(attr(termsBeta, "term.labels"), intercept = attr(termsBeta, "intercept"))
  formulaTheta <- reformulate(attr(termsTheta, "term.labels"), intercept = attr(termsTheta, "intercept"))

  varsBeta <- all.vars(formulaBeta)
  varsTheta <- all.vars(formulaTheta)
  allVars <- unique(c(varsBeta, varsTheta))

  if (all(unlist(lapply(allVars, function(x) is.vector(envData[[x]]))))) {

    sim$fireSense_SizePredictBeta <- formulaBeta %>%
      model.matrix(envData) %>%
      `%*%` (sim$fireSense_SizeFitted$coefBeta) %>%
      drop %>% sim$fireSense_SizeFitted$linkFunBeta$linkinv(.)
    sim$fireSense_SizePredictTheta <- formulaTheta %>%
      model.matrix(envData) %>%
      `%*%` (sim$fireSense_SizeFitted$coefTheta) %>%
      drop %>% sim$fireSense_SizeFitted$linkFunTheta$linkinv(.)

  } else if (all(unlist(lapply(allVars, function(x) is(envData[[x]], "RasterStack"))))) {

    sim$fireSense_SizePredictBeta <- mget(varsBeta, envir = envData, inherits = FALSE) %>%
      lapply(unstack) %>%
      c(list(FUN = function(...) stack(list(...)), SIMPLIFY = FALSE)) %>%
      do.call("mapply", args = .) %>%
      lapply(function(x, sim)
        predict(x, model = formulaBeta, fun = fireSense_SizePredictBetaRaster,
                na.rm = TRUE, sim = sim), sim = sim) %>%
      stack

    sim$fireSense_SizePredictTheta <- mget(varsTheta, envir = envData, inherits = FALSE) %>%
      lapply(unstack) %>%
      c(list(FUN = function(...) stack(list(...)), SIMPLIFY = FALSE)) %>%
      do.call("mapply", args = .) %>%
      lapply(function(x, sim)
        predict(x, model = formulaTheta, fun = fireSense_SizePredictThetaRaster,
                na.rm = TRUE, sim = sim), sim = sim) %>%
      stack

  } else if (all(unlist(lapply(allVars, function(x) is(envData[[x]], "RasterLayer"))))) {

    sim$fireSense_SizePredictBeta <- mget(varsBeta, envir = envData, inherits = FALSE) %>%
      stack %>% predict(model = formulaBeta, fun = fireSense_SizePredictBetaRaster, na.rm = TRUE, sim = sim)
    sim$fireSense_SizePredictTheta <- mget(varsTheta, envir = envData, inherits = FALSE) %>%
      stack %>% predict(model = formulaTheta, fun = fireSense_SizePredictThetaRaster, na.rm = TRUE, sim = sim)

  } else {

    varsExist <- allVars %in% ls(envData)
    varsClass <- unlist(lapply(allVars, function(x) is.data.frame(envData[[x]]) || is(envData[[x]], "RasterLayer") || is(envData[[x]], "RasterStack")))

    if (any(!varsExist)) {
      stop(paste0("fireSense_SizePredict> Variable '", allVars[which(!varsExist)[1L]], "' not found."))
    } else if (any(varsClass)) {
      stop("fireSense_SizePredict> Variables are not of the same class.")
    } else {
      stop(paste0("fireSense_SizePredict> Variable '", allVars[which(!varsClass)[1L]], "' is not a data.frame, a RasterLayer, or a RasterStack."))
    }
  }

  if (!is.na(p(sim)$intervalRunModule))
    sim <- scheduleEvent(sim, time(sim) + p(sim)$intervalRunModule, "fireSense_SizePredict", "run")

  sim
}
