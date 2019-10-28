#Internal
PintaDendroOLD <- function(valoresMatriz,spaces=0, Lista) {


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

      vector="--"
      for (z in 0:spaces) {
        vector=paste(vector,"--",sep="")
      }

      vector=paste(vector,"->",sep="")

      print(paste(vector,"C_",WriteDendro.var,sep = ""))

      Lista[[paste("C_",WriteDendro.var,sep = "")]]=namesLi[listaNew[[nombrelis]]]

      WriteDendro.var<<-WriteDendro.var+1

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



        vector=""
        for (z in 0:spaces) {
          vector=paste(vector,"--",sep="")
        }

        vector=paste(vector,"->",sep="")


        print(paste(vector,"C_", WriteDendro.var,sep = ""))


        Lista[[paste("C_",WriteDendro.var,sep = "")]]=rownamesvect

        WriteDendro.var<<-WriteDendro.var+1


        Lista<-PintaDendroOLD(subvaloresmatrizCal,spaces+1,Lista)
      }
      else
      {

        for (i in 1:length(names(subvaloresmatrizCal)))
          if (i==1)
            rownamesvect<-names(subvaloresmatrizCal)[1]
          else
            rownamesvect<-c(rownamesvect,names(subvaloresmatrizCal)[i])

          vector="--"
          for (z in 0:spaces) {
            vector=paste(vector,"--",sep="")
          }

          vector=paste(vector,"->",sep="")




          Lista[[paste("C_",WriteDendro.var,sep = "")]]=rownamesvect

          print(paste(vector,"C_", WriteDendro.var,sep = ""))

          WriteDendro.var<<-WriteDendro.var+1

          vector="----"
          for (z in 0:spaces) {
            vector=paste(vector,"--",sep="")
          }

          vector=paste(vector,"->",sep="")

          for (nombresTa in names(subvaloresmatrizCal)) {




            Lista[[paste("C_",WriteDendro.var,sep = "")]]=nombresTa

            print(paste(vector,"C_", WriteDendro.var,sep = ""))

            WriteDendro.var<<-WriteDendro.var+1
          }
      }

    }
  }

  Lista

}

procesaLista <- function (entrada)
{



  if (is.vector(entrada))
  {

    for (i in 1:length(entrada))
      if (i==1)
        rownamesvect=entrada[1]
      else
        rownamesvect=paste(rownamesvect,entrada[i], sep = ",")
  }
  else
      rownamesvect=entrada

  return(rownamesvect)
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

  if (is.vector(salida))
  {
    for (i in 1:length(salida)){
      if (i==1)
        rownamesvect=salida[i]
      else
        rownamesvect=paste(rownamesvect,salida[i],sep = ",")
    }
  }
  else
    rownamesvect=salida

  return(rownamesvect)
}


procesaListaMatrixNoFile <- function (entrada,Ekival,tablaMatrix)
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


  if (is.vector(salida))
  {
    for (i in 1:length(salida)){
      if (i==1)
        rownamesvect=salida[i]
      else
        rownamesvect=paste(rownamesvect,salida[i],sep = ",")
    }
  }
  else
    rownamesvect=salida

  return(rownamesvect)
}

#'@title WriteDendro
#'@description Paint in console the dondrogram and return a list with non empty cross elements group
#'@param valoresMatriz Tree Matrix with all grups generatod by, from 2 to limit-1 cutree(hclust_avg, k =2:(length(hclust_avg$order)-1))
#'@param tablaMatrix Cross original matrix Document-Term with relation 1 or 0 if term exist or not
#'@param masdocumenWrite True for extra info of documents, default False if no estra info
#'@param masvacios Paint emptry cross elements groups
#'@return list with non empty element groups
#'@export
WriteDendro <- function(valoresMatriz,tablaMatrix,masdocumenWrite=FALSE,masvacios=FALSE) {

  ClusterSet=list();

  Ekival=list();

  for (i in 1:length(rownames(tablaMatrix)))
    Ekival[[rownames(tablaMatrix)[i]]]=i



  WriteDendro.var<<-0

  Lista=list();



  if (is.array(valoresMatriz))
  {
    rownamesvect=c()
    #Obtener nombres de una Matriz
    for (i in 1:length(rownames(valoresMatriz)))
      if (i==1)
        rownamesvect=rownames(valoresMatriz)[1]
      else
        rownamesvect<-c(rownamesvect,rownames(valoresMatriz)[i])

  }else
  {
    #Obtener nombre de un Vector
    for (i in 1:length(names(valoresMatriz)))
      if (i==1)
        rownamesvect=names(valoresMatriz)[1]
      else
        rownamesvect<-c(rownamesvect,names(valoresMatriz)[i])
  }



  print(paste("->C_", WriteDendro.var,sep = ""))


  Lista[[paste("C_",WriteDendro.var,sep = "")]]=rownamesvect


  WriteDendro.var<<-WriteDendro.var+1

  Lista<-PintaDendroOLD(valoresMatriz,0,Lista)

  for (nombrelis in names(Lista)) {
    pintalo=TRUE
  #  print(pintalo)
    if (!masvacios)
    {
      solucion=procesaListaMatrix(Lista[[nombrelis]],Ekival,tablaMatrix)
     # print(solucion)
      if (is.null(solucion))
        pintalo=FALSE
      else
      {
      pintalo=!(stri_isempty(procesaListaMatrix(Lista[[nombrelis]],Ekival,tablaMatrix)))
      }
  #    print(pintalo)
    }

    if (pintalo)
    {
    if (masdocumenWrite)
      print(paste(nombrelis,":",procesaLista(Lista[[nombrelis]]),"->{",procesaListaMatrix(Lista[[nombrelis]],Ekival,tablaMatrix),"}",sep = ""))
    else
      print(paste(nombrelis,":","{",procesaListaMatrix(Lista[[nombrelis]],Ekival,tablaMatrix),"}",sep = ""))
    }

    solucion=procesaListaMatrixNoFile(Lista[[nombrelis]],Ekival,tablaMatrix)

    if (!is.null(solucion))
      if(!(stri_isempty(procesaListaMatrix(Lista[[nombrelis]],Ekival,tablaMatrix))))
        ClusterSet[[solucion]]=TRUE

  }

  return(ClusterSet)

}
