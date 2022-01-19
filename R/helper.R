# Helper functions for plotter

grid_maker.1d = function(data, features, grid.size){
  result <- list()
  for (feature in features){
    if (class(data[,feature]) == "numeric"){
      temp.list <- list(seq(min(data[,feature]),max(data[,feature]),length.out = grid.size))
    }
    if (class(data[,feature]) %in% c("factor","integer")){
      temp.list <- list(unique(data[,feature]))
    }
    result <- append(result, temp.list)
  }
  names(result) <- features
  return(result)
}
