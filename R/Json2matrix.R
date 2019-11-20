#' @title JSON2matrix
#'
#' @description This package reads a JSON file and converts it into a matrix
#'
#' @param
#' the JSON file
#'
#' @return the matrix
#
#' @examples Json2matrix("C:/RDATA1/ExampleList.json")
#'
#' @export
#' Json2matrix <- function(filename)

Json2matrix <- function(filename)
{
  # Load the package required to read JSON files
  library("rjson")
  library(data.table)
  library(reshape2)
  #Read the JSON file
  result <- fromJSON(file = filename)
  #Convert Heirarchical list (Long format)
  result1 <- rbindlist(result)
  result_dt <- as.data.table(result1)
  #Remove column not required for analysis
  result_dt = subset(result_dt, select = -c(url,tooltip) )
  #USe DCAST & ROWID function to change the data to Wide format
  result_dt <- dcast(result_dt, rowidv(result_dt, cols=c("clss")) ~ clss , value.var="data")
  #result_dt1  <- copy(result_dt)
  result_dt <- result_dt[, -(1)]
  return(result_dt)
}




