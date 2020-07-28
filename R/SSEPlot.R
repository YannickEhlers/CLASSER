SSEPlot <- function(SimulatedSSEObject){
  
  SampleSizeWithBootstraps <- cbind(SSE@SampleSizesTested, SSE@MeasuredMatrix[,1], SSE@MeasuredMatrix[,2],SSE@MeasuredMatrix[,3],SSE@MeasuredMatrix[,4],SSE@MeasuredMatrix[,5],SSE@MeasuredMatrix[,6],SSE@MeasuredMatrix[,7],SSE@MeasuredMatrix[,8],SSE@MeasuredMatrix[,9],SSE@MeasuredMatrix[,10])
  
  loessRegression <- loess(rowMeans(SSE@MeasuredMatrix) ~ SSE@SampleSizesTested)
  
  plot(SampleSizeWithBootstrapsDataframe$V1, SampleSizeWithBootstrapsDataframe$V2, ylim = c(-0.1, 1.1))
  points(SampleSizeWithBootstrapsDataframe$V1, SampleSizeWithBootstrapsDataframe$V3)
  points(SampleSizeWithBootstrapsDataframe$V1, SampleSizeWithBootstrapsDataframe$V4)
  points(SampleSizeWithBootstrapsDataframe$V1, SampleSizeWithBootstrapsDataframe$V5)
  points(SampleSizeWithBootstrapsDataframe$V1, SampleSizeWithBootstrapsDataframe$V6)
  points(SampleSizeWithBootstrapsDataframe$V1, SampleSizeWithBootstrapsDataframe$V7)
  points(SampleSizeWithBootstrapsDataframe$V1, SampleSizeWithBootstrapsDataframe$V8)
  points(SampleSizeWithBootstrapsDataframe$V1, SampleSizeWithBootstrapsDataframe$V9)
  points(SampleSizeWithBootstrapsDataframe$V1, SampleSizeWithBootstrapsDataframe$V10)
  
  PreductionLoess <- predict(loessRegression)
  
  lines(PreductionLoess)
  lines(PreductionLoess + 2*apply(SSE@MeasuredMatrix, 1, sd))
  lines(PreductionLoess - 2*apply(SSE@MeasuredMatrix, 1, sd))
}
