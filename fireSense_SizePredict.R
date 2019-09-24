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
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "fireSense_SizePredict.Rmd"),
  reqdPkgs = list("magrittr", "raster"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", default, min, max, "parameter description")),
    defineParameter(name = "modelObjName", class = "character", 
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
                            with objects of other classes."),
    defineParameter(name = "mapping", class = "character, list", default = NULL,
                    desc = "optional named vector or list of character strings
                            mapping one or more variables in the model formula
                            to those in data objects."),
    defineParameter(name = ".runInitialTime", class = "numeric", default = start(sim),
                    desc = "when to start this module? By default, the start 
                            time of the simulation."),
    defineParameter(name = ".runInterval", class = "numeric", default = 1, 
                    desc = "optional. Interval between two runs of this module,
                            expressed in units of simulation time. By default, 1 year."),
    defineParameter(name = ".saveInitialTime", class = "numeric", default = NA, 
                    desc = "optional. When to start saving output to a file."),
    defineParameter(name = ".saveInterval", class = "numeric", default = NA, 
                    desc = "optional. Interval between save events."),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
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

## event types
#   - type `init` is required for initialiazation

doEvent.fireSense_SizePredict = function(sim, eventTime, eventType, debug = FALSE) 
{
  moduleName <- current(sim)$moduleName
  
  switch(
    eventType,
    init = { 
      sim <- scheduleEvent(sim, eventTime = P(sim)$.runInitialTime, moduleName, "run")
      
      if (!is.na(P(sim)$.saveInitialTime))
        sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, moduleName, "save", .last())
    },
    run = { 
      sim <- sizePredictRun(sim) 
      
      if (!is.na(P(sim)$.runInterval)) # Assumes time only moves forward
        sim <- scheduleEvent(sim, time(sim) + P(sim)$.runInterval, moduleName, "run")
    },
    save = {
      sim <- sizePredictSave(sim)
      
      if (!is.na(P(sim)$.saveInterval))
        sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, moduleName, "save", .last())
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

sizePredictRun <- function(sim)
{
  moduleName <- current(sim)$moduleName
  
  if (!is(sim[[P(sim)$modelObjName]], "fireSense_SizeFit"))
    stop(moduleName, "> '", P(sim)$modelObjName, "' should be of class 'fireSense_SizeFit")
  
  ## Toolbox: set of functions used internally by sizePredictRun
    sizePredictBetaRaster <- function(model, data, sim)
    {
      model %>%
        model.matrix(data) %>%
        `%*%` (sim[[P(sim)$modelObjName]]$coef$beta) %>%
        drop %>% sim[[P(sim)$modelObjName]]$link$beta$linkinv(.)
    }
    
    sizePredictThetaRaster <- function(model, data, sim) 
    {
      model %>%
        model.matrix(data) %>%
        `%*%` (sim[[P(sim)$modelObjName]]$coef$theta) %>%
        drop %>% sim[[P(sim)$modelObjName]]$link$theta$linkinv(.)
    }

  # Load inputs in the data container
  # list2env(as.list(envir(sim)), envir = mod)
  
  mod_env <- new.env()
    
  for (x in P(sim)$data)
  {
    if (!is.null(sim[[x]]))
    {
      if (is.data.frame(sim[[x]]))
      {
        list2env(sim[[x]], envir = mod_env)
      } 
      else if (is(sim[[x]], "RasterStack") || is(sim[[x]], "RasterBrick")) 
      {
        list2env(setNames(unstack(sim[[x]]), names(sim[[x]])), envir = mod_env)
      } 
      else if (is(sim[[x]], "RasterLayer"))
      {
        mod_env[[x]] <- sim[[x]]
      } 
      else stop(moduleName, "> '", x, "' is not a data.frame, a RasterLayer, a RasterStack or a RasterBrick.")
    }
  }
  
  termsBeta <- delete.response(terms.formula(formulaBeta <- sim[[P(sim)$modelObjName]]$formula$beta))
  termsTheta <- delete.response(terms.formula(formulaTheta <- sim[[P(sim)$modelObjName]]$formula$theta))

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

  if (all(unlist(lapply(allxy, function(x) is.vector(mod_env[[x]])))))
  {
    sim$fireSense_SizePredicted_Beta <- formulaBeta %>%
      model.matrix(mod_env) %>%
      `%*%` (sim[[P(sim)$modelObjName]]$coef$beta) %>%
      drop %>% sim[[P(sim)$modelObjName]]$link$beta$linkinv(.)
           
    sim$fireSense_SizePredicted_Theta <- formulaTheta %>%
      model.matrix(mod_env) %>%
      `%*%` (sim[[P(sim)$modelObjName]]$coef$theta) %>%
      drop %>% sim[[P(sim)$modelObjName]]$link$theta$linkinv(.)
  } 
  else if (all(unlist(lapply(allxy, function(x) is(mod_env[[x]], "RasterLayer"))))) 
  {
    sim$fireSense_SizePredicted_Beta <- mget(xyBeta, envir = mod_env, inherits = FALSE) %>%
      stack %>% predict(model = formulaBeta, fun = sizePredictBetaRaster, na.rm = TRUE, sim = sim)
    
    sim$fireSense_SizePredicted_Theta <- mget(xyTheta, envir = mod_env, inherits = FALSE) %>%
      stack %>% predict(model = formulaTheta, fun = sizePredictThetaRaster, na.rm = TRUE, sim = sim)
  } 
  else 
  {
    missing <- !allxy %in% ls(mod_env, all.names = TRUE)
    
    if (s <- sum(missing))
      stop(moduleName, "> '", allxy[missing][1L], "'",
           if (s > 1) paste0(" (and ", s-1L, " other", if (s>2) "s", ")"),
           " not found in data objects.")
    
    badClass <- unlist(lapply(allxy, function(x) is.vector(mod_env[[x]]) || is(mod_env[[x]], "RasterLayer")))
    
    if (any(badClass)) 
    {
      stop(moduleName, "> Data objects of class 'data.frame' cannot be mixed with objects of other classes.")
    } 
    else 
    {
      stop(moduleName, "> '", paste(allxy[which(!badClass)], collapse = "', '"),
           "' does not match a data.frame's column, a RasterLayer or a layer from a RasterStack or RasterBrick.")
    }
  }

  invisible(sim)
}

sizePredictSave <- function(sim)
{
  timeUnit <- timeunit(sim)
  currentTime <- time(sim, timeUnit)
  
  saveRDS(
    sim$fireSense_SizePredicted_Beta, 
    file = file.path(paths(sim)$out, paste0("fireSense_SizePredicted_Beta_", timeUnit, currentTime, ".rds"))
  )
  
  saveRDS(
    sim$fireSense_SizePredicted_Theta, 
    file = file.path(paths(sim)$out, paste0("fireSense_SizePredicted_Theta_", timeUnit, currentTime, ".rds"))
  )
  
  invisible(sim)
}
