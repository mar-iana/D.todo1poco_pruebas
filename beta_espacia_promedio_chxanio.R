# matriz: diversidad beta espacial promedio por charco por anio
....

# matriz: diversidad beta temporal promedio entre pares de años consecutivos

#función que divide en tantas bases por charco y las une en una lista
hace_riq_x_ch<-function(base,colum.ch=3){
  out<-list()
  for (i in 1:length(unique(na.omit(base[,colum.ch])))) {
    out[[i]]<-subset(base,base[,colum.ch]==(unique(na.omit(base[,colum.ch])))[i])
  }
  names(out)<-as.character(unique(na.omit(base[,colum.ch])))
  out
}
riqueza_x_ch_lista<-hace_riq_x_ch(riqueza)

# función para hacer matrices de ocurrencia
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

# hace lista con matrices de ocurrencia
hace_matriz.ocurr_x_ano<-function(lista_base_riq, comunidades_en=3, especies_en=5,abundancia=TRUE){
  out<-list()
  for (i in 1:length(lista_base_riq)) {
    out[[i]]<-co.g(lista_base_riq[[i]],comunidades_en,especies_en,abundancia)
  }
  names(out)<-names(lista_base_riq)
  out
}

# ahora en vez de hacer charcos x spp. lo que hago en años x spp, y cada unos de estos elementos debo hacer un vegdist para obtener Beta temporal de cada charco...
matrices_aniosxspp_para_cada.ch<-hace_matriz.ocurr_x_ano(riqueza_x_ch_lista, comunidades_en=1,  especies_en = 5, abundancia=TRUE)

?vegdist
kkk<-as.matrix(vegdist(t(matrices_aniosxspp_para_cada.ch$`9`),method="bray",diag = FALSE,upper = FALSE))
k<-vegdist(t(matrices_aniosxspp_para_cada.ch$`9`),method="bray",diag = FALSE,upper = FALSE)

d <- row(kkk) - col(kkk)

# use split to group on these values
jj<-split(kkk, d)
class(jj)
class(jj$`1`)
names(jj$`1`)<-c("2005-2006","")
