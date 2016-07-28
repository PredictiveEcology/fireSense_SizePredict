library(SpaDES)

## data.frame
  ## 1 var
    # mySim <- simInit(
    #   times = list(start = 1, end = 2, timeunit = "year"),
    #   modules = list("fireSense_SizePredict"),
    #   paths = list(modulePath = " # replace with empty string instead"),
    #   inputs = data.frame(
    #     files = c("Z:/fireSense_SizeFit1var.RData", "Z:/dataFireSense_SizePredict.RData"),
    #     functions = c("load", "load"),
    #     package = c("base", "base"),
    #     loadTime = 0,
    #     stringsAsFactors = FALSE)
    # )

  ## 6 var
    # mySim <- simInit(
    #   times = list(start = 1, end = 2, timeunit = "year"),
    #   modules = list("fireSense_SizePredict"),
    #   paths = list(modulePath = " # replace with empty string instead"),
    #   params = list(fireSense_SizePredict = list(newData = c("dataFireSense_SizePredict", "dataFireSense_SizePredict2"))),
    #   inputs = data.frame(
    #     files = c("Z:/fireSense_SizeFit6var.RData", "Z:/dataFireSense_SizePredict.RData"),
    #     functions = c("load", "load"),
    #     package = c("base", "base"),
    #     stringsAsFactors = FALSE)
    # )
    # mySim <- loadFiles(mySim)
    # mySim$dataFireSense_SizePredict2 <- dplyr::select(mySim$dataFireSense_SizePredict, MDC_JUN)
    # mySim$dataFireSense_SizePredict <- dplyr::select(mySim$dataFireSense_SizePredict, -MDC_JUN)

## RasterLayer
  ## 1 var
    # mySim <- simInit(
    #   times = list(start = 1, end = 2, timeunit = "year"),
    #   modules = list("fireSense_SizePredict"),
    #   paths = list(modulePath = " # replace with empty string instead"),
    #   inputs = data.frame(
    #     files = c("Z:/fireSense_SizeFit1var.RData", "Z:/MDC_JUN.tif"),
    #     functions = c("load", "raster"),
    #     package = c("base", "raster"),
    #     objectName = c("fireSense_SizeFit", "MDC_JUN"),
    #     loadTime = 0,
    #     stringsAsFactors = FALSE)
    # )

  ## 6 var
    # mySim <- simInit(
    #   times = list(start = 1, end = 2, timeunit = "year"),
    #   modules = list("fireSense_SizePredict"),
    #   paths = list(modulePath = " # replace with empty string instead"),
    #   inputs = data.frame(
    #     files = c("Z:/fireSense_SizeFit6var.RData", "Z:/MDC_JUN.tif", "Z:/MDC_JUN.tif", "Z:/HW.tif", "Z:/HW.tif", "Z:/HW.tif", "Z:/HW.tif"),
    #     functions = c("load", "raster", "raster", "raster", "raster", "raster", "raster"),
    #     package = c("base", "raster", "raster", "raster", "raster", "raster", "raster"),
    #     objectName = c("fireSense_SizeFit", "MDC_JUN", "MDC_MJ", "HW", "DIST", "O", "WATER"),
    #     loadTime = 0,
    #     stringsAsFactors = FALSE)
    # )

## RasterStack (time series)
  # 1 var
    # mySim <- simInit(
    #   times = list(start = 1, end = 2, timeunit = "year"),
    #   modules = list("fireSense_SizePredict"),
    #   paths = list(modulePath = " # replace with empty string instead"),
    #   inputs = data.frame(
    #     files = c("Z:/fireSense_SizeFit1var.RData", "Z:/MDC_JUNs.tif"),
    #     functions = c("load", "stack"),
    #     package = c("base", "raster"),
    #     objectName = c("fireSense_SizeFit", "MDC_JUN"),
    #     loadTime = 0,
    #     stringsAsFactors = FALSE)
    # )

  ## 6 var
    # mySim <- simInit(
    #   times = list(start = 1, end = 2, timeunit = "year"),
    #   modules = list("fireSense_SizePredict"),
    #   paths = list(modulePath = " # replace with empty string instead"),
    #   inputs = data.frame(
    #     files = c("Z:/fireSense_SizeFit6var.RData", "Z:/MDC_JUNs.tif", "Z:/MDC_JUNs.tif", "Z:/HWs.tif", "Z:/HWs.tif", "Z:/HWs.tif", "Z:/HWs.tif"),
    #     functions = c("load", "stack", "stack", "stack", "stack", "stack", "stack"),
    #     package = c("base", "raster", "raster", "raster", "raster", "raster", "raster"),
    #     objectName = c("fireSense_SizeFit", "MDC_JUN", "MDC_MJ", "HW", "DIST", "O", "WATER"),
    #     stringsAsFactors = FALSE)
    # )

spades(mySim, debug = FALSE)

# str(list(mySim$fireSense_SizePredictBeta, mySim$fireSense_SizePredictTheta))
# x11(); Plot(mySim$fireSense_SizePredictBeta); Plot(mySim$fireSense_SizePredictTheta)