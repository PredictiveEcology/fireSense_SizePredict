# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "fireSense_SizePredict",
  description = "Predict the shape and taper parameters of the tapered Pareto from a model fitted using the fireSense_SizeFit module.",
  keywords = c("fire size distribution", "tapered Pareto", "fireSense", "statistical model", "predict"),
  authors=c(person("Jean", "Marchal", email = "jean.d.marchal@gmail.com", role = c("aut", "cre"))),
  childModules = character(),
  version = numeric_version("0.1.0"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = NA_character_, # e.g., "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "fireSense_SizePredict.Rmd"),
  reqdPkgs = list("magrittr", "raster"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", default, min, max, "parameter description")),
    defineParameter(name = "data", class = "character", default = "dataFireSense_SizePredict",
                    desc = "a character vector indicating the names of objects
                            in the `simList` environment in which to look for
                            variables in the model. `data` objects can be
                            data.frames or named lists of RasterLayers, but
                            data.frames and RasterLayers can not be mixed
                            together. If omitted, or if variables are not found
                            in `data` objects, variables are searched in the
                            `simList` environment."),
    defineParameter(name = "mapping", class = "character, list", default = NULL,
                    desc = "optional named vector or list of character strings
                            mapping one or more variables in the model formula
                            to those in data objects."),
    defineParameter(name = "initialRunTime", class = "numeric", default = start(sim),
                    desc = "when to start this module? By default, the start 
                            time of the simulation."),
    defineParameter(name = "intervalRunModule", class = "numeric", default = NA, 
                    desc = "optional. Interval between two runs of this module,
                            expressed in units of simulation time.")
  ),
  inputObjects = rbind(
    expectsInput(
      objectName = "fireSense_SizeFitted",
      objectClass = "fireSense_SizeFit",
      sourceURL = NA_character_,
      desc = "An object of class fireSense_SizeFit, i.e. created with the fireSense_SizeFit module."
    ),
    expectsInput(
      objectName = "dataFireSense_FrequencyPredict",
      objectClass = "data.frame, raster",
      sourceURL = NA_character_,
      desc = "One or more objects of class data.frame, or named lists of RasterLayer in which to look for variables with which to predict."
    )
  ),
  outputObjects = createsOutput(
    objectName = "fireSense_SizePredicted",
    objectClass = "list",
    desc = "An object whose class depends on that of the inputs, could be a raster or a vector of type numeric."
  )
))

## Toolbox: set of functions used internally by the module
  ## Predict functions
    fireSense_SizePredictBetaRaster <- function(model, data, sim) {

      model %>%
        model.matrix(data) %>%
        `%*%` (sim[[P(sim)$model]]$coef$beta) %>%
        drop %>% sim[[P(sim)$model]]$link$beta$linkinv(.)

    }

    fireSense_SizePredictThetaRaster <- function(model, data, sim) {

      model %>%
        model.matrix(data) %>%
        `%*%` (sim[[P(sim)$model]]$coef$theta) %>%
        drop %>% sim[[P(sim)$model]]$link$theta$linkinv(.)

    }

## event types
#   - type `init` is required for initialiazation

doEvent.fireSense_SizePredict = function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    sim <- sim$fireSense_SizePredictInit(sim)

  } else if (eventType == "run") {
    sim <- sim$fireSense_SizePredictRun(sim)
    
  } else if (eventType == "save") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event
    
    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function
    
    # schedule future event(s)
    
    # e.g.,
    # sim <- scheduleEvent(sim, time(sim) + increment, "fireSense_SizeFit", "save")
    
    # ! ----- STOP EDITING ----- ! #
    
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

  sim <- scheduleEvent(sim, eventTime = P(sim)$initialRunTime, current(sim)$moduleName, "run")
  sim

}

fireSense_SizePredictRun <- function(sim) {

  moduleName <- current(sim)$moduleName
  
  envData <- new.env(parent = envir(sim))
  on.exit(rm(envData))

  # Load data in the container
  list2env(as.list(envir(sim)), envir = envData)
  
  lapply(P(sim)$data, function(x, envData) {
    
    if (!is.null(sim[[x]])) {
      
      if (is.list(sim[[x]]) && !is.null(names(sim[[x]]))) {
        
        list2env(sim[[x]], envir = envData)
        
      } else stop(paste0(moduleName, "> '", x, "' is not a data.frame or a named list."))
      
    }
    
  }, envData = envData)

  termsBeta <- delete.response(terms.formula(formulaBeta <- sim[[P(sim)$model]]$formula$beta))
  termsTheta <- delete.response(terms.formula(formulaTheta <- sim[[P(sim)$model]]$formula$theta))

  ## Mapping variables names to data
  if (!is.null(P(sim)$mapping)) {

    for (i in 1:length(P(sim)$mapping)) {

      attr(termsBeta, "term.labels") <- gsub(
        pattern = names(P(sim)$mapping[i]),
        replacement = P(sim)$mapping[[i]],
        x = attr(termsBeta, "term.labels")
      )
      attr(termsTheta, "term.labels") <- gsub(
        pattern = names(P(sim)$mapping[i]),
        replacement = P(sim)$mapping[[i]],
        x = attr(termsTheta, "term.labels")
      )
    }

  }

  formulaBeta <- reformulate(attr(termsBeta, "term.labels"), intercept = attr(termsBeta, "intercept"))
  formulaTheta <- reformulate(attr(termsTheta, "term.labels"), intercept = attr(termsTheta, "intercept"))

  xyBeta <- all.vars(formulaBeta)
  xyTheta <- all.vars(formulaTheta)
  allxy <- unique(c(xyBeta, xyTheta))

  if (all(unlist(lapply(allxy, function(x) is.vector(envData[[x]]))))) {

    sim$fireSense_SizePredicted <- 
      list(beta = formulaBeta %>%
             model.matrix(envData) %>%
             `%*%` (sim[[P(sim)$model]]$coef$beta) %>%
             drop %>% sim[[P(sim)$model]]$link$beta$linkinv(.),
           theta = formulaTheta %>%
             model.matrix(envData) %>%
             `%*%` (sim[[P(sim)$model]]$coef$theta) %>%
             drop %>% sim[[P(sim)$model]]$link$theta$linkinv(.))
      
  } else if (all(unlist(lapply(allxy, function(x) is(envData[[x]], "RasterLayer"))))) {

    sim$fireSense_SizePredicted <- 
      list(beta = mget(xyBeta, envir = envData, inherits = FALSE) %>%
             stack %>% predict(model = formulaBeta, fun = fireSense_SizePredictBetaRaster, na.rm = TRUE, sim = sim),
           theta = mget(xyTheta, envir = envData, inherits = FALSE) %>%
             stack %>% predict(model = formulaTheta, fun = fireSense_SizePredictThetaRaster, na.rm = TRUE, sim = sim))

  } else {

    exist <- allxy %in% ls(envData)
    class <- unlist(lapply(allxy, function(x) is.data.frame(envData[[x]]) || is(envData[[x]], "RasterLayer")))

    if (any(!exist)) {
      stop(paste0(moduleName, "> Variable '", allxy[which(!exist)[1L]], "' not found."))
    } else if (any(class)) {
      stop(paste0(moduleName, "> Data objects are not of the same class (e.g. data.frames)."))
    } else {
      stop(paste0(moduleName, "> Variable '", allxy[which(!class)[1L]], "' does not match a data.frame's column, a list element, or a RasterLayer."))
    }
  }

  if (!is.na(P(sim)$intervalRunModule))
    sim <- scheduleEvent(sim, time(sim) + P(sim)$intervalRunModule, moduleName, "run")

  sim
}


### template for save events
fireSense_SizePredictSave <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

