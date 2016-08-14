library(SpaDES)

## data.frame
  ## 1 var
    # mySim <- simInit(
    #   times = list(start = 1, end = 1, timeunit = "year"),
    #   modules = list("fireSense_SizePredict"),
    #   paths = list(modulePath = " # replace with empty string instead"),
    #   params = list(fireSense_SizePredict = list(data = "dataFireSense_Size")),
    #   inputs = data.frame(
    #     files = c("Z:/fireSense_SizeFitted_1v.rds", "Z:/dataFireSense_Size.rds"),
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
    #   params = list(fireSense_SizePredict = list(data = c("dataFireSense_Size", "dataFireSense_Size2"))),
    #   inputs = data.frame(
    #     files = c("Z:/fireSense_SizeFitted_6v.rds", "Z:/dataFireSense_Size.rds"),
    #     functions = c("readRDS", "readRDS"),
    #     package = c("base", "base"),
    #     objectName = c("fireSense_SizeFitted", NA),
    #     stringsAsFactors = FALSE)
    # )
    # mySim$dataFireSense_Size2 <- dplyr::select(mySim$dataFireSense_Size, MDC_JUN)
    # mySim$dataFireSense_Size <- dplyr::select(mySim$dataFireSense_Size, -MDC_JUN)

## RasterLayer
  ## 1 var
    # mySim <- simInit(
    #   times = list(start = 1, end = 1, timeunit = "year"),
    #   modules = list("fireSense_SizePredict"),
    #   paths = list(modulePath = " # replace with empty string instead"),
    #   params = list(fireSense_SizePredict = list(mapping = c(MDC_MJ = "MDC_JUN"))),
    #   inputs = data.frame(
    #     files = c("Z:/fireSense_SizeFitted_1v.rds", "Z:/MDC_JUN.tif"),
    #     functions = c("readRDS", "raster"),
    #     package = c("base", "raster"),
    #     objectName = c("fireSense_SizeFitted", NA),
    #     stringsAsFactors = FALSE)
    # )

  ## 6 var
    mySim <- simInit(
      times = list(start = 1, end = 1, timeunit = "year"),
      modules = list("fireSense_SizePredict"),
      paths = list(modulePath = " # replace with empty string instead"),
      inputs = data.frame(
        files = c("Z:/fireSense_SizeFitted_6v.rds", "Z:/MDC_JUN.tif", "Z:/MDC_JUN.tif", "Z:/HW.tif", "Z:/HW.tif", "Z:/HW.tif", "Z:/HW.tif"),
        functions = c("readRDS", "raster", "raster", "raster", "raster", "raster", "raster"),
        package = c("base", "raster", "raster", "raster", "raster", "raster", "raster"),
        objectName = c("fireSense_SizeFitted", "MDC_JUN", "MDC_MJ", "HW", "DIST", "O", "WATER"),
        stringsAsFactors = FALSE)
    )

spades(mySim, debug = FALSE)

# str(mySim$fireSense_SizePredict)
# x11(); Plot(mySim$fireSense_SizePredict$beta); Plot(mySim$fireSense_SizePredict$theta)