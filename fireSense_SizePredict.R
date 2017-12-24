# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "fireSense_SizePredict",
  description = "Predicts two parameters of the tapered Pareto distribution,
                 beta and theta, using a model fitted with the fireSense_SizeFit
                 module. beta controls the rate of frequency decrease as the
                 fire size increases, and theta governs the location of the
                 exponential taper.",
  keywords = c("fire size distribution", "tapered Pareto", "fireSense", "statistical model", "predict"),
  authors=c(person("Jean", "Marchal", email = "jean.d.marchal@gmail.com", role = c("aut", "cre"))),
  childModules = character(),
  version = list(SpaDES.core = "0.1.0", fireSense_SizePredict = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = NA_character_, # e.g., "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "fireSense_SizePredict.Rmd"),
  reqdPkgs = list("magrittr", "raster"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", default, min, max, "parameter description")),
    defineParameter(name = "modelName", class = "character", 
                    default = "fireSense_SizeFitted",
                    desc = "name of the object of class fireSense_SizeFit
                            describing the statistical model used for
                            predictions."),
    defineParameter(name = "data", class = "character", 
                    default = "dataFireSense_SizePredict",
                    desc = "a character vector indicating the names of objects 
                            in the `simList` environment in which to look for 
                            variables present in the model formula. `data`
                            objects can be data.frames, RasterStacks or
                            RasterLayers. However, data.frames cannot be mixed
                            with objects of other classes. If variables are not 
                            found in `data` objects, they are searched in the
                            `simList` environment."),
    defineParameter(name = "mapping", class = "character, list", default = NULL,
                    desc = "optional named vector or list of character strings
                            mapping one or more variables in the model formula
                            to those in data objects."),
    defineParameter(name = ".runInitialTime", class = "numeric", default = start(sim),
                    desc = "when to start this module? By default, the start 
                            time of the simulation."),
    defineParameter(name = ".runInterval", class = "numeric", default = NA, 
                    desc = "optional. Interval between two runs of this module,
                            expressed in units of simulation time."),
    defineParameter(".useCache", "numeric", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = rbind(
    expectsInput(
      objectName = "fireSense_SizeFitted",
      objectClass = "fireSense_SizeFit",
      sourceURL = NA_character_,
      desc = "An object of class fireSense_SizeFit, i.e. created with the fireSense_SizeFit module."
    ),
    expectsInput(
      objectName = "dataFireSense_SizePredict",
      objectClass = "data.frame, RasterLayer, RasterStack",
      sourceURL = NA_character_,
      desc = "One or more objects of class data.frame, RasterLayer or RasterStack in which to look for variables with which to predict."
    )
  ),
  outputObjects = createsOutput(
    objectName = "fireSense_SizePredicted",
    objectClass = NA_character_,
    desc = "An object whose class depends on that of the inputs, could be a RasterLayer or a vector of type numeric."
  )
))

## Toolbox: set of functions used internally by the module
  ## Predict functions
    fireSense_SizePredictBetaRaster <- function(model, data, sim)
    {
      model %>%
        model.matrix(data) %>%
        `%*%` (sim[[P(sim)$modelName]]$coef$beta) %>%
        drop %>% sim[[P(sim)$modelName]]$link$beta$linkinv(.)
    }

    fireSense_SizePredictThetaRaster <- function(model, data, sim) 
    {
      model %>%
        model.matrix(data) %>%
        `%*%` (sim[[P(sim)$modelName]]$coef$theta) %>%
        drop %>% sim[[P(sim)$modelName]]$link$theta$linkinv(.)
    }

## event types
#   - type `init` is required for initialiazation

doEvent.fireSense_SizePredict = function(sim, eventTime, eventType, debug = FALSE) 
{
  switch(
    eventType,
    init = { sim <- sim$fireSense_SizePredictInit(sim) },
    run = { sim <- sim$fireSense_SizePredictRun(sim) },
    save = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      
      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function
      
      # schedule future event(s)
      
      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + increment, "fireSense_SizeFit", "save")
      
      # ! ----- STOP EDITING ----- ! #
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )

  invisible(sim)
}

## event functions
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
fireSense_SizePredictInit <- function(sim)
{
  sim <- scheduleEvent(sim, eventTime = P(sim)$.runInitialTime, moduleName, "run")
  invisible(sim)
}

fireSense_SizePredictRun <- function(sim)
{
  stopifnot(is(sim[[P(sim)$modelName]], "fireSense_SizeFit"))
  
  moduleName <- current(sim)$moduleName
  currentTime <- time(sim, timeunit(sim))
  endTime <- end(sim, timeunit(sim))

  # Create a container to hold the data  
  envData <- new.env(parent = envir(sim))
  on.exit(rm(envData))
  
  # Load inputs in the data container
  list2env(as.list(envir(sim)), envir = envData)
  
  for (x in P(sim)$data)
  {
    if (!is.null(sim[[x]]))
    {
      if (is.data.frame(sim[[x]]))
      {
        list2env(sim[[x]], envir = envData)
      } 
      else if (is(sim[[x]], "RasterStack")) 
      {
        list2env(setNames(unstack(sim[[x]]), names(sim[[x]])), envir = envData)
      } 
      else if (is(sim[[x]], "RasterLayer"))
      {
        # Do nothing
      } else stop(paste0(moduleName, "> '", x, "' is not a data.frame, a RasterLayer or a RasterStack."))
    }
  }
  
  termsBeta <- delete.response(terms.formula(formulaBeta <- sim[[P(sim)$modelName]]$formula$beta))
  termsTheta <- delete.response(terms.formula(formulaTheta <- sim[[P(sim)$modelName]]$formula$theta))

  ## Mapping variables names to data
  if (!is.null(P(sim)$mapping)) 
  {
    for (i in 1:length(P(sim)$mapping)) 
    {
      attr(termsBeta, "term.labels") %<>% gsub(
        pattern = names(P(sim)$mapping[i]),
        replacement = P(sim)$mapping[[i]],
        x = .
      )
      attr(termsTheta, "term.labels") %<>% gsub(
        pattern = names(P(sim)$mapping[i]),
        replacement = P(sim)$mapping[[i]],
        x = .
      )
    }
  }

  formulaBeta <- reformulate(attr(termsBeta, "term.labels"), intercept = attr(termsBeta, "intercept"))
  formulaTheta <- reformulate(attr(termsTheta, "term.labels"), intercept = attr(termsTheta, "intercept"))

  xyBeta <- all.vars(formulaBeta)
  xyTheta <- all.vars(formulaTheta)
  allxy <- unique(c(xyBeta, xyTheta))

  if (all(unlist(lapply(allxy, function(x) is.vector(envData[[x]])))))
  {
    sim$fireSense_SizePredicted <-
      list(beta = formulaBeta %>%
             model.matrix(envData) %>%
             `%*%` (sim[[P(sim)$modelName]]$coef$beta) %>%
             drop %>% sim[[P(sim)$modelName]]$link$beta$linkinv(.),
           theta = formulaTheta %>%
             model.matrix(envData) %>%
             `%*%` (sim[[P(sim)$modelName]]$coef$theta) %>%
             drop %>% sim[[P(sim)$modelName]]$link$theta$linkinv(.),
           a = sim[[P(sim)$modelName]]$a
      )
  } 
  else if (all(unlist(lapply(allxy, function(x) is(envData[[x]], "RasterLayer"))))) 
  {
    sim$fireSense_SizePredicted <-
      list(beta = mget(xyBeta, envir = envData, inherits = FALSE) %>%
             stack %>% predict(model = formulaBeta, fun = fireSense_SizePredictBetaRaster, na.rm = TRUE, sim = sim),
           theta = mget(xyTheta, envir = envData, inherits = FALSE) %>%
             stack %>% predict(model = formulaTheta, fun = fireSense_SizePredictThetaRaster, na.rm = TRUE, sim = sim),
           a = sim[[P(sim)$modelName]]$a
      )

  } 
  else 
  {
    missing <- !allxy %in% ls(envData, all.names = TRUE)
    
    if (s <- sum(missing))
      stop(paste0(moduleName, "> '", allxy[missing][1L], "'",
                  if (s > 1) paste0(" (and ", s-1L, " other", if (s>2) "s", ")"),
                  " not found in data objects."))
    
    badClass <- unlist(lapply(allxy, function(x) is.vector(envData[[x]]) || is(envData[[x]], "RasterLayer")))
    
    if (any(badClass)) 
    {
      stop(paste0(moduleName, "> Data objects of class 'data.frame' cannot be mixed with objects of other classes."))
    } 
    else 
    {
      stop(paste0(moduleName, "> '", paste(allxy[which(!badClass)], collapse = "', '"),
                  "' does not match a data.frame's column, a RasterLayer or a RasterStack's layer."))
    }
  }

  if (!is.na(P(sim)$.runInterval)) # Assumes time only moves forward
    sim <- scheduleEvent(sim, currentTime + P(sim)$.runInterval, moduleName, "run")

  invisible(sim)
}

