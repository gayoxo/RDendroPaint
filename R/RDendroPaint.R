#'@title RDendroPaint
#'@description Paint in console the dondrogram and return a list with non empty cross elements group
#'@param valoresMatriz Tree Matrix with all grups generatod by, normally from 2 to limit-1 \"cutree(hclust_avg, k =2:(length(hclust_avg$order)-1))\"
#'@param tablaMatrix Cross original matrix Document-Term with relation 1 or 0 if term exist or not
#'@return Object DendroTree 
RDendroPaint <- function(valoresMatriz,tablaMatrix) {
  
  setClass("DendroTree", slots=list(name="character", sons="list", documents="list", values="list"))
  
  
  
}


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

print(names(ListaSal))