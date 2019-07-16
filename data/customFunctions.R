PrintHello = function(name="world!"){
  hello = paste0("hello ", name)
  print(hello)
}

GetPi = function(n=4){
  pi = round(3.141592,n)
  return(pi)
}

ImputeNAsWithMean = function(dataset){
  for(i in 1:ncol(dataset)){
    is_na = is.na(dataset[,i])
    if(any(is_na)){
      na_ids = which(is_na)
      #mean
      var_mean = mean(dataset[,i],na.rm = T)
      dataset[na_ids,i] = var_mean
      message = paste0("Replacing NA with mean: ", colnames(dataset)[i])
      print(message)  
    }
  }
  return(dataset)
}

CMP_subset = CMP[,c(1:4, 14:16)]
CMP_subset_imputed = ImputeNAsWithMean(CMP_subset)
