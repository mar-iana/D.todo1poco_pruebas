# 17 de febrero 2021
### Lista con una matriz de ocurrencias (spp x charco) por año
# base de datos del dropbox: "base_riq_15" (spp de plantas observadas por año, por charco y por um, etc.)
# exploro base madre:
head(base_riq_15)
ncol(base_riq_15)
#riqueza<-as.data.frame(base_riq_15[,1:5])
riqueza<-as.data.frame(br_lista)
class(riqueza[,5])
unique(riqueza[,5])
riqueza[which(riqueza[,5]=="0"),]
table(riqueza[,1:2])
riqueza[16668,]
riqueza<-riqueza[-(which(riqueza[,5]=="0")),]
dim(riqueza)
anos_totales<-unique(na.omit(riqueza$año))
riqueza[which((is.na(riqueza[,5]))==TRUE),] #2016 y 2018 tienen algunas spp con entrada NA
length(unique(riqueza[,5]))
length(unique(as.data.frame(br_lista)[,5]))
########

#función que divide en tantas bases por año y las une en una lista
hace_riq_x_ano<-function(base,colum.anos=1){
  out<-list()
  for (i in 1:length(unique(na.omit(base[,colum.anos])))) {
    out[[i]]<-subset(base,base[,colum.anos]==(unique(na.omit(base[,colum.anos])))[i])
  }
  names(out)<-as.character(unique(na.omit(base[,colum.anos])))
  out
}
riqueza_x_ano_lista<-hace_riq_x_ano(riqueza)#,anos_totales[7:14],1)

#tengo anos donde hay mas de un mes, en ese caso cuando no es tan obvio que es en primavera,
# A PRIORI, voy a incluir el muestreo con mayor numero de indiv. involucrados en cada ano, 
# y que ademas coincide con el mayor numero de charcos activos
# entonces:
#2006->uso muestreo de mes 6
#2007 y 2008->uso muestreo del mes 8
#2009 y 2010->uso muestreo del mes 7

# funcion donde identifica el mes para cada año con mayor registro:
hace_bases_1mes_x_ano<-function(base, lista.base.anos, anos=1,mes=2){
  tabla<-table(base[,c(anos,mes)])
  out<-list()
  for (i in 1:length(rownames(tabla))) {
    out[[i]]<-subset(lista.base.anos[[i]],lista.base.anos[[i]][,2]==as.numeric(colnames(tabla)[which(tabla[i,]==max(tabla[i,]))]))
  }
  names(out)<-names(lista.base.anos)
  out
}

riqueza_x_ano_lista_1mes<-hace_bases_1mes_x_ano(riqueza,riqueza_x_ano_lista,anos = 1,mes = 2)

# RED DE OCURRENCIAS DE SPP X CHARCA PARA CADA ANO: matriz spp x charco 

co.g<-function(M, comunidades_en, especie_en, abundancia){
  SPP<-unique(M[,especie_en])
  COMM<-unique(M[,comunidades_en])
  out<-matrix(0,length(SPP),length(COMM))
  colnames(out)<-COMM
  rownames(out)<-SPP
  for (i in 1:length(SPP)){
    for(j in 1:length(COMM)){
      if(length(which(M[,especie_en]==SPP[i] & M[,comunidades_en]==COMM[j]))>0){
        JJ<-which(colnames(out)==COMM[j])
        II<-which(rownames(out)==SPP[i])
        if(abundancia==FALSE)out[II,JJ]<-1
        if(abundancia==TRUE)out[II,JJ]<-length(which(M[,especie_en]==SPP[i] & M[,comunidades_en]==COMM[j]))
      }
    }
  }
  out
}

hace_matriz.ocurr_x_ano<-function(lista_base_riq, comunidades_en=3, especies_en=5,abundancia=TRUE){
  out<-list()
  for (i in 1:length(lista_base_riq)) {
    out[[i]]<-co.g(lista_base_riq[[i]],comunidades_en,especies_en,abundancia)
      }
  names(out)<-names(lista_base_riq)
  out
}


#matrices_ocurrencia_xano<-hace_matriz.ocurr_x_ano(riqueza_x_ano_lista_1mes, comunidades_en=3,  especies_en = 5, abundancia=TRUE)

matrices_ocurrencia_xano_corregida<-hace_matriz.ocurr_x_ano(riqueza_x_ano_lista_1mes, comunidades_en=3,  especies_en = 5, abundancia=TRUE)

save(matrices_ocurrencia_xano_corregida,file = "matrices_ocurrencia_xano_corregida.RData")
###################
