SSEPlot <- function(SSEObject){
  
  loessRegression <- loess(rowMeans(SSEObject@MeasuredMatrix) ~ SSEObject@SampleSizesTested)

  PreductionLoess <- predict(loessRegression)
  
  plot(PreductionLoess, type = "l", ylim = c(0,1), SimulatedSSEObject)
  
  
  PredictionUpperBorder <- PreductionLoess + 2*apply(SSEObject@MeasuredMatrix, 1, sd)
  PredictionUpperBorder[which(PredictionUpperBorder > 1)] <- 1

  PredictionLowerBorder <- PreductionLoess - 2*apply(SSEObject@MeasuredMatrix, 1, sd)
  PredictionLowerBorder[which(PredictionLowerBorder < 0)] <- 0
  
  lines(predict(loess(PredictionLowerBorder ~ SSEObject@SampleSizesTested)), lty = 2)
  lines(predict(loess(PredictionUpperBorder ~ SSEObject@SampleSizesTested)), lty = 2)
  }
