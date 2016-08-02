library(SpaDES)

## data.frame
  ## 1 var
    # mySim <- simInit(
    #   times = list(start = 1, end = 1, timeunit = "year"),
    #   modules = list("fireSense_SizePredict"),
    #   paths = list(modulePath = " # replace with empty string instead"),
    #   params = list(fireSense_SizePredict = list(data = "dataFireSense_SizePredict")),
    #   inputs = data.frame(
    #     files = c("Z:/fireSense_SizeFitted1var.rds", "Z:/dataFireSense_SizePredict.rds"),
    #     functions = c("readRDS", "readRDS"),
    #     package = c("base", "base"),
    #     objectName = c("fireSense_SizeFitted", NA),
    #     stringsAsFactors = FALSE)
    # )

  ## 6 var
    # mySim <- simInit(
    #   times = list(start = 1, end = 1, timeunit = "year"),
    #   modules = list("fireSense_SizePredict"),
    #   paths = list(modulePath = " # replace with empty string instead"),
    #   params = list(fireSense_SizePredict = list(data = c("dataFireSense_SizePredict", "dataFireSense_SizePredict2"))),
    #   inputs = data.frame(
    #     files = c("Z:/fireSense_SizeFitted6var.rds", "Z:/dataFireSense_SizePredict.rds"),
    #     functions = c("readRDS", "readRDS"),
    #     package = c("base", "base"),
    #     objectName = c("fireSense_SizeFitted", NA),
    #     stringsAsFactors = FALSE)
    # )
    # mySim$dataFireSense_SizePredict2 <- dplyr::select(mySim$dataFireSense_SizePredict, MDC_JUN)
    # mySim$dataFireSense_SizePredict <- dplyr::select(mySim$dataFireSense_SizePredict, -MDC_JUN)

## RasterLayer
  ## 1 var
    # mySim <- simInit(
    #   times = list(start = 1, end = 1, timeunit = "year"),
    #   modules = list("fireSense_SizePredict"),
    #   paths = list(modulePath = " # replace with empty string instead"),
    #   inputs = data.frame(
    #     files = c("Z:/fireSense_SizeFitted1var.rds", "Z:/MDC_JUN.tif"),
    #     functions = c("readRDS", "raster"),
    #     package = c("base", "raster"),
    #     objectName = c("fireSense_SizeFitted", NA),
    #     stringsAsFactors = FALSE)
    # )

  ## 6 var
    # mySim <- simInit(
    #   times = list(start = 1, end = 1, timeunit = "year"),
    #   modules = list("fireSense_SizePredict"),
    #   paths = list(modulePath = " # replace with empty string instead"),
    #   inputs = data.frame(
    #     files = c("Z:/fireSense_SizeFitted6var.rds", "Z:/MDC_JUN.tif", "Z:/MDC_JUN.tif", "Z:/HW.tif", "Z:/HW.tif", "Z:/HW.tif", "Z:/HW.tif"),
    #     functions = c("readRDS", "raster", "raster", "raster", "raster", "raster", "raster"),
    #     package = c("base", "raster", "raster", "raster", "raster", "raster", "raster"),
    #     objectName = c("fireSense_SizeFitted", "MDC_JUN", "MDC_MJ", "HW", "DIST", "O", "WATER"),
    #     stringsAsFactors = FALSE)
    # )

## RasterStack (time series) (with mapping)
  # 1 var
    # mySim <- simInit(
    #   times = list(start = 1, end = 1, timeunit = "year"),
    #   modules = list("fireSense_SizePredict"),
    #   paths = list(modulePath = " # replace with empty string instead"),
    #   params = list(fireSense_SizePredict = list(mapping = c(MDC_JUN = "MDC_JUN__STACK"))),
    #   inputs = data.frame(
    #     files = c("Z:/fireSense_SizeFitted1var.rds", "Z:/MDC_JUN__STACK.tif"),
    #     functions = c("readRDS", "stack"),
    #     package = c("base", "raster"),
    #     objectName = c("fireSense_SizeFitted", "MDC_JUN__STACK"),
    #     stringsAsFactors = FALSE)
    # )

  ## 6 var
    mySim <- simInit(
      times = list(start = 1, end = 1, timeunit = "year"),
      modules = list("fireSense_SizePredict"),
      paths = list(modulePath = " # replace with empty string instead"),
      inputs = data.frame(
        files = c("Z:/fireSense_SizeFitted6var.rds", "Z:/MDC_JUN__STACK.tif", "Z:/MDC_JUN__STACK.tif", "Z:/HW__STACK.tif", "Z:/HW__STACK.tif", "Z:/HW__STACK.tif", "Z:/HW__STACK.tif"),
        functions = c("readRDS", "stack", "stack", "stack", "stack", "stack", "stack"),
        package = c("base", "raster", "raster", "raster", "raster", "raster", "raster"),
        objectName = c("fireSense_SizeFitted", "MDC_JUN", "MDC_MJ", "HW", "DIST", "O", "WATER"),
        stringsAsFactors = FALSE)
    )

spades(mySim, debug = FALSE)

# str(list(mySim$fireSense_SizePredictBeta, mySim$fireSense_SizePredictTheta))
# x11(); Plot(mySim$fireSense_SizePredictBeta); Plot(mySim$fireSense_SizePredictTheta)