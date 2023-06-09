#' Title t-test
#'
#' @param x numeric vector x
#' @param y numeric vector y
#' @param alpha numeric value (error rate)
#' @param independentSamp Is independent
#' @param equalVar Is var equal
#'
#' @return t-test for supplied variables
#' @export
#'
#' @examples
#' \dontrun{tConstr(x=x, y=y, alpha=alpha)}
#'
tConstr = function(x, y, alpha, independentSamp = TRUE, equalVar = TRUE){

  if(independentSamp==TRUE && equalVar == TRUE){
    #run the t-test with equal var
    ttest = t.test(x, y, var.equal = TRUE)
  }
  else if(independentSamp==TRUE && equalVar==FALSE){
    #run the t-test with unequal var
    ttest = t.test(x, y, var.equal = TRUE)
  }
  else{
    ttest = t.test(x, y, paired = TRUE)
  }
  #create the data frame of x and y
  #x and y won't always be the same length and may return an error
  #check the lengths of x and y and make corrections to length if needed with NA values
  if(length(x)==length(y)){
    df = data.frame(x = x, y = y)
  }

  else if(length(x)<length(y)){
    xtemp = rep(NA, length(y))
    for (i in 1:length(x)) {
      xtemp[i] = x[i]
    }
    df = data.frame(x = xtemp, y = y)
  }

  else{
    ytemp = rep(NA, length(x))
    for (i in 1:length(y)) {
      ytemp[i] = y[i]
    }
    df = data.frame(x = x, y = ytemp)
  }
  #return the function information in a list
  list(Data = df, Alpha = alpha, CI = ttest$conf.int, Pvalue = ttest$p.value)
}
