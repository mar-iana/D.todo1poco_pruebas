# matriz: diversidad beta espacial promedio por charco por anio
View(matrices_ocurrencia_xano_corregida$`2005`)

hace_matrices_dist_anioxanio<-function(lista_xch, metodo="bray"){
  out<-list()
  for (i in 1:length(lista_xch)) {
    out[[i]]<-as.matrix(vegdist(t(lista_xch[[i]]),method =metodo))
  }
  names(out)<-names(lista_xch)
  out
}

lista_dist_chxch_xanio<-hace_matrices_dist_anioxanio(matrices_ocurrencia_xano_corregida)
View(lista_dist_chxch_xanio$`2020`)

colSums(lista_dist_chxch_xanio$`2020`)[1]/(ncol(lista_dist_chxch_xanio$`2020`)-1)
rowSums(lista_dist_chxch_xanio$`2020`)[1]          
apply(lista_dist_chxch_xanio$`2020`,1,mean)[1] # esta mal porque deber'ia poder sacar la diagonal

###
hace_vector_beta.esp_xch_xanio<-function(lista_xanio){ #, base_org, ch_en,anio_en){
  out.t<-list()
  #out<-matrix(NA,nrow =length(unique(base_org[,ch_en])) ,ncol = length(unique(base_org[,ch_en])))
  #rownames(out)<-unique(base_org[,anio_en])
  #colnames(out)<-unique(base_org[,ch_en])
  for (i in 1:length(lista_xanio)) {
    m.t<-lista_xanio[[i]]
    diag(m.t)<-NA
    out.t[[i]]<-apply(m.t,1,mean,na.rm=TRUE)
      }
  #out
  names(out.t)<-names(lista_xanio)
  out.t
  }

lista_betaxch_xanio<-hace_vector_beta.esp_xch_xanio(lista_dist_chxch_xanio)

maru<-function(lista_vectore_betaxch_xanio,base_org, ch_en=3){
  out<-matrix(NA,nrow =length(lista_vectore_betaxch_xanio) ,ncol = length(unique(base_org[,ch_en])))
  rownames(out)<-names(lista_vectore_betaxch_xanio)
    #unique(base_org[,anio_en])
  colnames(out)<-unique(base_org[,ch_en])
  for (i in 1:length(lista_vectore_betaxch_xanio)) {
    for (j in 1:length(colnames(out))) {
      for (k in 1:length(lista_vectore_betaxch_xanio[[i]])) {
         #if(names(lista_vectore_betaxch_xanio[[i]])[k]==colnames(out)[j]){
           if(length(which(colnames(out)[j]==(names(lista_vectore_betaxch_xanio[[i]])[k]))==TRUE)>0){ 
      out[i,j]<-lista_vectore_betaxch_xanio[[i]][k]
    }
    else{
      out
    }
      } 
    }
        }
  out
  }
  
temy<-maru(lista_betaxch_xanio,riqueza, ch_en = 3)
out<-matrix(NA,nrow =length(lista_betaxch_xanio) ,ncol = length(unique(riqueza[,3])))
beta_espacial_prom_aniosxch<-temy
save(beta_espacial_prom_aniosxch,file = "beta_espacial_prom_aniosxch.RData")

####___________________
jj<-lista_dist_chxch_xanio
View(jj$`2005`)
gg<-lapply(jj, apply(jj[], margin, ...))
View(gg$`2005`)
function(x) diag(x)<-NA

(lista_dist_chxch_xanio$`2008`)->kk
diag(kk) <- NA
apply(kk,1,mean, na.rm=TRUE)[1]
names(apply(kk,1,mean, na.rm=TRUE))


