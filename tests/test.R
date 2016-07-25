library(SpaDES)

mySim <- simInit(
  times = list(start = 1, end = 2, timeunit = "year"),
  modules = list("fireSense_SizePredict"),
  paths = list(modulePath = " # replace with empty string instead"),
  params = list(fireSense_SizePredict = list(newData = "s")),
  inputs = data.frame(
    files = "Z:/stack.tif",
    objectName = "s",
    functions = "stack",
    package = "raster",
    loadTime = 0,
    stringsAsFactors = FALSE)
)

spades(mySim, debug = TRUE)
