#'@title RDendroPaint
#'@description Paint in console the dondrogram and return a list with non empty cross elements group
#'@param valoresMatriz Tree Matrix with all grups generatod by, normally from 2 to limit-1 \"cutree(hclust_avg, k =2:(length(hclust_avg$order)-1))\"
#'@param tablaMatrix Cross original matrix Document-Term with relation 1 or 0 if term exist or not
#'@return Object DendroTree 
RDendroPaint <- function(valoresMatriz,tablaMatrix) {
  
  setClass("DendroTree", slots=list(name="character", sons="list", documents="list", values="list"))
  
  
  
}