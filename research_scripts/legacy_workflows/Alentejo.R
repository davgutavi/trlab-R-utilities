# Graficar todas las series temporales de todos los puntos en un único panel ---- 
source("utils/GraphUtilities.R")
devtest <- buildTimeSeriesPlots("/home/david/devtest/devtest.sol","NDVI")
printPlotList(devtest)
soco4gr <- buildTimeSeriesPlots("/home/david/soco04/soco04.sol","NDVI")
soco5gr <- buildTimeSeriesPlots("/home/david/soco05/soco05.sol","NDVI")
soco6gr <- buildTimeSeriesPlots("/home/david/soco06/soco06.sol","NDVI")
soco7gr <- buildTimeSeriesPlots("/home/david/soco07/soco07.sol","NDVI")
printPlotList(soco7gr)

