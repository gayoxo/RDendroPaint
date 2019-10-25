
# Can extend and create
#' @export DendroTree
#' @exportClass DendroTree
DendroTree<-setClass("DendroTree", slots=list(name="character", sons="list", documents="character", values="character"))




PintaDendro <- function(valoresMatriz,Ekival,RootEle,tablaMatrix) {
  
  
  library(stringi)
  
  
  listaNew=list();
  
  if (is.array(valoresMatriz)){
    subvaloresmatrizCal=valoresMatriz[,1]
  }else
    subvaloresmatrizCal=valoresMatriz
  
  #Procesa el vector magico
  
  for (j in 1:length(subvaloresmatrizCal))
  {
    
    if (as.character(subvaloresmatrizCal[j]) %in% names(listaNew)){
      listaNew[[as.character(subvaloresmatrizCal[j])]]= c(listaNew[[as.character(subvaloresmatrizCal[j])]],j)
    }else
      listaNew[[as.character(subvaloresmatrizCal[j])]]=j
    
    
  }
  
  #Procesa la lista con los valores segmentados
  
  for (nombrelis in names(listaNew)) {
    
    
    
    
    if (length(listaNew[[nombrelis]])<=1)
    {
      #print(nombrelis)
      
      if (is.array(valoresMatriz)){
        namesLi=rownames(valoresMatriz)
      }else
        namesLi=names(valoresMatriz)
      
      # print(namesLi[listaNew[[nombrelis]]])
      
      RootEleI<-new ("DendroTree", name = paste("C_", RDendroPaint.var,sep = ""))
      
      rownamesvect= namesLi[listaNew[[nombrelis]]]
      
      RootEleI@documents<-rownamesvect;
      
      RooteVAlue<-procesaListaMatrix(rownamesvect,Ekival,tablaMatrix)
      if (!is.null(RooteVAlue))
        RootEleI@values<-RooteVAlue
      
      print(RootEleI@name)
      
      RDendroPaint.var<<-RDendroPaint.var+1
      
      if (is.null(RootEle@sons))
        RootEle@sons<-RootEleI
      else
        RootEle@sons<-c(RootEle@sons,RootEleI)
      
    }
    else{
      
      if (is.array(valoresMatriz)){
        subvaloresmatrizCal=valoresMatriz[listaNew[[nombrelis]],2:ncol(valoresMatriz)]
      }else
        subvaloresmatrizCal=valoresMatriz[listaNew[[nombrelis]]]
      
      
      
      
      # print(subvaloresmatrizCal)
      
      dif=FALSE
      
      while (!dif)
      {
        
        if (is.array(subvaloresmatrizCal))
        {
          
          #        print(subvaloresmatrizCal)
          
          subvaloresmatrizCal1=subvaloresmatrizCal[,1]
          subvaloresmatrizCal2=subvaloresmatrizCal[,2]
          
          listpar1=list()
          
          for (j in 1:length(subvaloresmatrizCal1))
          {
            
            if (as.character(subvaloresmatrizCal1[j]) %in% names(listpar1)){
              listpar1[[as.character(subvaloresmatrizCal1[j])]]= c(listpar1[[as.character(subvaloresmatrizCal1[j])]],j)
            }else
              listpar1[[as.character(subvaloresmatrizCal[j])]]=j
            
          }
          
          listpar2=list()
          
          for (j in 1:length(subvaloresmatrizCal2))
          {
            
            if (as.character(subvaloresmatrizCal2[j]) %in% names(listpar2)){
              listpar2[[as.character(subvaloresmatrizCal2[j])]]= c(listpar2[[as.character(subvaloresmatrizCal2[j])]],j)
            }else
              listpar2[[as.character(subvaloresmatrizCal2[j])]]=j
            
          }
          
          
          if (length(names(listpar1))>1&&length(names(listpar1))!=length(names(listpar2)))
            dif=TRUE
          else
            subvaloresmatrizCal=subvaloresmatrizCal[,2:ncol(subvaloresmatrizCal)]
          
          #      print(subvaloresmatrizCal)
          
        }else
          dif=TRUE
      }
      
      
      #    print(subvaloresmatrizCal)
      
      if (is.array(subvaloresmatrizCal)||length(subvaloresmatrizCal)>2)
      {
        
        
        if (is.array(subvaloresmatrizCal))
        {
          #Obtener nombres de una Matriz
          for (i in 1:length(rownames(subvaloresmatrizCal)))
            if (i==1)
              rownamesvect=rownames(subvaloresmatrizCal)[1]
            else
              rownamesvect=c(rownamesvect,rownames(subvaloresmatrizCal)[i])
            
        }else
        {
          #Obtener nombre de un Vector
          for (i in 1:length(names(subvaloresmatrizCal)))
            if (i==1)
              rownamesvect<-names(subvaloresmatrizCal)[1]
            else
              rownamesvect<-c(rownamesvect,names(subvaloresmatrizCal)[i])
        }
        
        
        
        RootEleI<-new ("DendroTree", name = paste("C_", RDendroPaint.var,sep = ""))
        
        RootEleI@documents<-rownamesvect;
        
        RooteVAlue<-procesaListaMatrix(rownamesvect,Ekival,tablaMatrix)
        if (!is.null(RooteVAlue))
          RootEleI@values<-RooteVAlue
        
        print(RootEleI@name)
        
        RDendroPaint.var<<-RDendroPaint.var+1
        
        RootEleI<-PintaDendro(subvaloresmatrizCal,Ekival,RootEleI,tablaMatrix)
        
        if (is.null(RootEle@sons))
          RootEle@sons<-RootEleI
        else
          RootEle@sons<-c(RootEle@sons,RootEleI)
        
        

       
        
  #      print(RootEleI@sons)
      }
      else
      {
        
        for (i in 1:length(names(subvaloresmatrizCal)))
          if (i==1)
            rownamesvect<-names(subvaloresmatrizCal)[1]
          else
            rownamesvect<-c(rownamesvect,names(subvaloresmatrizCal)[i])
          
          
          RootEleI<-new ("DendroTree", name = paste("C_", RDendroPaint.var,sep = ""))
          
          RootEleI@documents<-rownamesvect;
          
          RooteVAlue<-procesaListaMatrix(rownamesvect,Ekival,tablaMatrix)
          if (!is.null(RooteVAlue))
            RootEleI@values<-RooteVAlue
          
          print(RootEleI@name)
          
          RDendroPaint.var<<-RDendroPaint.var+1
          
         
          
          

          
          for (nombresTa in names(subvaloresmatrizCal)) {
            
            
            RootEleI2<-new ("DendroTree", name = paste("C_", RDendroPaint.var,sep = ""))
            
            
            RootEleI2@documents<-nombresTa;
            
            RooteVAlue<-procesaListaMatrix(nombresTa,Ekival,tablaMatrix)
            if (!is.null(RooteVAlue))
              RootEleI2@values<-RooteVAlue
            
            print(RootEleI2@name)
            
            RDendroPaint.var<<-RDendroPaint.var+1
            
            if (is.null(RootEleI@sons))
              RootEleI@sons<-RootEleI2
            else
              RootEleI@sons<-c(RootEleI@sons,RootEleI2)
            
          }
          
          
          
          if (is.null(RootEle@sons))
            RootEle@sons<-RootEleI
          else
            RootEle@sons<-c(RootEle@sons,RootEleI)
      }
      
    }
  }
  
  return (RootEle)
  
}



procesaListaMatrix <- function (entrada,Ekival,tablaMatrix)
{
  
  #print(Ekival)
  #print(entrada)
  
  validos=c();
  if (is.vector(entrada))
  {
    
    for (i in 1:length(entrada))
    {
      validos=c(validos,Ekival[[entrada[i]]])
    }
    
  }
  else
    validos=c(validos,Ekival[[entrada]])
  
  #  print("SI")
  #  print(validos)
  
  subma=tablaMatrix[validos,]
  #  print(subma)
  
  salida=c()
  if (is.array(subma))
  {
    
    for (i in 1:ncol(subma))
    {
      foundZero=FALSE
      for(j in 1:nrow(subma))
        if (subma[j,i]==0)
          foundZero=TRUE
        
        if (!foundZero)
          salida=c(salida,colnames(subma)[i])
    }
  }
  else
  {
    for (i in 1:length(subma)){
      if (subma[i]==1)
        salida=c(salida,names(subma)[i])
      
    }
  }

  
  return(salida)
}



#'@title RDendroPaint
#'@description Paint in console the dondrogram and return a list with non empty cross elements group
#'@param valoresMatriz Tree Matrix with all grups generatod by, normally from 2 to limit-1 \"cutree(hclust_avg, k =2:(length(hclust_avg$order)-1))\"
#'@param tablaMatrix Cross original matrix Document-Term with relation 1 or 0 if term exist or not
#'@return Object DendroTree 
#'@export
RDendroPaint <- function(valoresMatriz,tablaMatrix) {
  

  Ekival=list();
  
  for (i in 1:length(rownames(tablaMatrix)))
    Ekival[[rownames(tablaMatrix)[i]]]=i
  
  
  
  RDendroPaint.var<<-0
  
  rownamesvect=c()
  
  if (is.array(valoresMatriz))
  {
    rownamesvect=rownames(valoresMatriz)
      
  }else
  {
    #Obtener nombre de un Vector
    rownamesvect=names(valoresMatriz)
  }
  
  RootEle<-new ("DendroTree", name = paste("C_", RDendroPaint.var,sep = ""))
  
  RootEle@documents<-c(rownamesvect);
  
  RooteVAlue<-procesaListaMatrix(rownamesvect,Ekival,tablaMatrix)
  if (!is.null(RooteVAlue))
    RootEle@values<-RooteVAlue
  
  print(RootEle@name)
  
  RDendroPaint.var<<-RDendroPaint.var+1
  
  RootEle<-PintaDendro(valoresMatriz,Ekival,RootEle,tablaMatrix)
  
  return(RootEle)
  
}



#'@title pintaRDendroPaint
#'@description Paint in console the DendroTree returned by RDendroPaint with the generic visualization
#'@param ListaSal input DendroTree
pintaRDendroPaint<- function(ListaSal){
  
  level=0
  pintaRDendroPaintLevel(ListaSal,level)

}

pintaRDendroPaintLevel<- function(ListaSal,level){
  
  
  
  
  
  if (is.vector(ListaSal@documents))
  {
    for (i in 1:length(ListaSal@documents)){
      if (i==1)
        rownamesvect=ListaSal@documents[i]
      else
        rownamesvect=paste(rownamesvect,ListaSal@documents[i],sep = ",")
    }
  }
  else
    rownamesvect=ListaSal@documents
  
  
  base=""
  for(i in 0:level)
    base<- paste(base,"--",sep = "")
  
  #setClass("DendroTree", slots=list(name="character", sons="list", documents="character", values="character"))
  print(paste(base,"name: ", ListaSal@name,sep = ""))
  
  if (is.vector(ListaSal@documents))
  {
    for (i in 1:length(ListaSal@documents)){
      if (i==1)
        rownamesvect=ListaSal@documents[i]
      else
        rownamesvect=paste(rownamesvect,ListaSal@documents[i],sep = ",")
    }
  }
  else
    rownamesvect=ListaSal@documents
  
  print(paste(base,"Documentos: ",rownamesvect ,sep = ""))
  
  if (is.vector( ListaSal@values))
  {
    if (length( ListaSal@values)>0)
    {
    for (i in 1:length( ListaSal@values)){
      if (i==1)
        rownamesvect=ListaSal@values[i]
      else
        rownamesvect=paste(rownamesvect,ListaSal@values[i],sep = ",")
    }
    }else
      rownamesvect=""
    
  }
  else
    rownamesvect=ListaSal@values
  
  print(paste(base,"Values: ",rownamesvect,sep = ""))
  
  for(doc in ListaSal@sons)
  {
    pintaRDendroPaintLevel(doc,level+1)
  }
    
}


test <-function(){

#Carga de librerias
library(LSAfun)
library(lsa)
library(RSpectra)
library(NLP)
library(tm)
library(RcppEigen)
library(plotrix)
library(ppclust)
library(nFCA)
library(factoextra)
library(cluster)
library(fclust)
library(RFLPtools)
library(RJSONIO)
library(stringr)

#Install my Library
#library(devtools)
#install_github("gayoxo/RDendroPaint")

#library(DendroPaint)

#Aleatorio

setwd("C:\\TMP\\NFCA")

now <- format(Sys.time(), "%Y%H%M%S")
nowF =paste(now,"_concept",sep  = "")
nowF

#Fuende de los ficheros
source_dir = "C:/TMP/LSAText/Arte"

#Matiz
data(stopwords_en)
TDM <- textmatrix(source_dir, stopwords=c(stopwords_en, "xx", "xxxx"), stemming=FALSE,
                  removeNumber=F, minGlobFreq=1, minDocFreq=0)
TDM

TTM=tcrossprod(TDM, TDM)
TTM

DDM=tcrossprod(t(TDM), t(TDM))
DDM


eigenSE=eigen(TTM)
eigenUE=eigen(DDM)

rownames(eigenSE$vectors) <- rownames(TTM)
eigenSE

rownames(eigenUE$vectors) <- rownames(DDM)
eigenUE


#Nos quedamos con los values de los documentos

Ev=eigenUE$values
if (length(eigenSE$values)<length(eigenUE$values))
  Ev=eigenSE$values


k=length(Ev);
k

#k=2

# Calculo de E


E=matrix(0L, nrow = k, ncol = k)
for (i in 1:k)
{ 
  
  
  for (j in 1:k)
  {
    if (j==i&&Ev[i]>0)
      E[i,j] <- sqrt(Ev[i])
    else
      E[i,j] <-  0            
  }
  
  
}
E




S=eigenSE$vectors
S=S[,1:k]
S

U=eigenUE$vectors
U=U[,1:k]
U


SxE=tcrossprod(S, E)
UxE=tcrossprod(U, E)


NUxE=matrix(0L, nrow = nrow(UxE), ncol = ncol(UxE))

for (i in 1:nrow(UxE))
{ 
  sum=0;
  for (k in 1:ncol(UxE))
  {
    sum=UxE[i,k]^2+sum
  }
  
  divi=sqrt(sum)
  
  
  for (j in 1:ncol(UxE))
    NUxE[i,j]=UxE[i,j]/divi
  
  
}




NSxE=matrix(0L, nrow = nrow(SxE), ncol = ncol(SxE))



for (i in 1:nrow(SxE))
{ 
  sum=0;
  for (k in 1:ncol(SxE))
  {
    sum=SxE[i,k]^2+sum
  }
  
  divi=sqrt(sum)
  
  
  
  for (j in 1:ncol(SxE))
    NSxE[i,j]=SxE[i,j]/divi
  
  
}


rownames(NUxE) <- rownames(DDM)
rownames(NSxE) <- rownames(TTM)
NUxE
NSxE

str(NUxE)
summary(NUxE)
any(is.na(NUxE))

if (any(is.na(NUxE)))
{
  print("Borrados los NaN")
  NUxE<- na.omit(NUxE)
}


seeds_df_sc <- as.data.frame(scale(NUxE))
summary(seeds_df_sc)
seeds_df_sc

dist_mat <- dist(seeds_df_sc, method = 'euclidean')

hclust_avg <- hclust(dist_mat, method = 'average')
plot(hclust_avg)

cutree(hclust_avg, k =2:(length(hclust_avg$order)-1))

valoresDendro = cutree(hclust_avg, k =2:(length(hclust_avg$order)-1))

#WriteDendro(valoresDendro)

ListaSal=RDendroPaint(valoresDendro,t(TDM))

pintaRDendroPaint(ListaSal)
}
