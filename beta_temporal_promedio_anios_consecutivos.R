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

hace_matrices_dist_anioxanio<-function(lista_xch, metodo="bray"){
  out<-list()
  for (i in 1:length(lista_xch)) {
    out[[i]]<-as.matrix(vegdist(t(lista_xch[[i]]),method =metodo))
  }
  names(out)<-names(lista_xch)
  out
}

lista_dist_anioxanio_xch<-hace_matrices_dist_anioxanio(matrices_aniosxspp_para_cada.ch)
View(lista_dist_anioxanio_xch[[1]])

d <- row(dd$`1`) - col(dd$`1`)
# use split to group on these values
jj<-split(dd$`1`, d)
View(dd$`1`)
class(jj)
class(jj$`1`)
jj$`1`
jj$`2`
names(jj$`1`)<-as.numeric(rownames(kkk)[2:nrow(kkk)])
class(names(jj$`1`))

hace_vect_xch_beta_anios_consec<-function(lista_distancia_entre_anios, base_original){
  out<-matrix(NA,nrow = length(unique(base_original[,3])),ncol = length(unique(base_original[,1])))
  rownames(out)<-unique(base_original[,3])
  colnames(out)<-unique(base_original[,1])
  #out.2<-list()
  for (i in 1:length(lista_distancia_entre_anios)) {
    d<-row(lista_distancia_entre_anios[[i]])-col(lista_distancia_entre_anios[[i]])
    diagonales<-split(lista_distancia_entre_anios[[i]],d)
    if(is.null(diagonales$`1`)== FALSE){
      diag_lag.1<-diagonales$`1`  
      names(diag_lag.1)<-rownames(lista_distancia_entre_anios[[i]])[2:nrow(lista_distancia_entre_anios[[i]])]
      }
    else{
      diag_lag.1<-"NA"
    }
    #out.2[[i]]<-diagonales$`1`
    #names(out.2[[i]])<-rownames(lista_distancia_entre_anios[[i]])[2:nrow(lista_distancia_entre_anios[[i]])]
    for (j in 1:colnames(out)) {
      for (k in 1:length(diag_lag.1)) {
      #for (k in 1:length(out.2[[i]])) {
        #if((length(which(colnames(out)[j]==names(out.2[[i]])[k])==TRUE))>0){
        if((length(which(colnames(out)[j]==names(diag_lag.1)[k])==TRUE))>0){
          #out[i,j]<-out.2[[i]][k] 
          out[i,j]<-diag_lag.1[k] 
        }
        else {
          out
        }  
      }
    }
  }
  out
}

lag_1anio<-hace_vect_xch_beta_anios_consec(lista_dist_anioxanio_xch,riqueza)


oo<-function(lista){
  kk<-vector()
    for (i in 1:length(lista)) {
      kk[i]<-ifelse((colnames(lista[[i]])[2]=="2006")==TRUE,1,NA)
          }
  names(kk)<-names(lista)
  kk  
}
jj<-oo(lista= lista_dist_anioxanio_xch)
jj

# REVISAR, YA QUE DEBER'IA MODIFICAR LAS MATRICES DE DISTANCIA, PARA QUE NO ME QUEDEN DOS ANIOS NO CONSECUTIVOS PEGADOS, EN EL CASO DE QUE EL ANIOS DEL MEDIO NO HAYA SIDO MUESTREADO ESE CHARCO...
#habr'ia que modificar el valor de beta de los anios consecutivos a un NA, ejemplo en el ch 14, no fue muestreado ni el anio 2011, ni 2014, por lo tanto el intervalo 2011-2012 (en la columna del 2012 del df generado) no deber'ia existir!

anos_totales[which(anos_totales%in%colnames(lista_dist_anioxanio_xch$`137`)==FALSE)]

#funcion que saca las distancias que no se correspondenanios consecutivos
saca_falsas_dist<-function(M_lag_1){
  out.1<-M_lag_1[,-1]
  #nro.col<-ncol(out.1)
  for (i in 1:(ncol(out.1)-1)) {
    for (j in 1:nrow(out.1)) {
      if(is.na(out.1[j,i])==TRUE){
          out.1[j,(i+1)]<-0 # se puede sustituir por un NA
      }
      else{
        out.1
        #out.1[i,j]<-out
      }
          }
      }
  out.1
}

lag_1anio_corregido<-saca_falsas_dist(lag_1anio)
lag_1anio_corregido<-ifelse(lag_1anio_corregido>0,lag_1anio_corregido,NA)

#save(lag_1anio_corregido,file = "beta_temp_chxanio_lag1.RData")

head(lag_1anio_corregido)
beta_temp_promedio_lag1<-rowMeans(lag_1anio_corregido,na.rm = TRUE) # antes de hacer esto, sustituir los 0 que puse en la base en la funcion anterior por NA
beta_temp_promedio_lag1
save(beta_temp_promedio_lag1,file = "beta_temp_promedio.RData")
#________________________
#para poder generalizar el problema de los años consecutivos para lag.2 y lag.3, 
#debería hacer una modificación a nivel de la matriz de distancia para charco, agegando los anios que faltan como celdas con NA
#...o incluso antes de la matriz de dist. de forma de poder hacer la función hace_vect_xch_beta_anios_consec
# y que ya quede!
# ver dónde sería conceptualmente correcto agregar columna del año faltante, si en matrices_aniosxspp_para_cada_charco, o en la lista con las matrices de distancia anioxanio para cada charco
# para mi lo correcto sería:
## en la matriz de distancia y agregar todo una fila y columna con NA del año faltante ##

View(lista_dist_anioxanio_xch$`54`)
colnames(lista_dist_anioxanio_xch$`54`)
anos_totales%in%colnames(lista_dist_anioxanio_xch$`54`)
which(anos_totales%in%colnames(lista_dist_anioxanio_xch$`54`)==FALSE)
anos_totales[which(anos_totales%in%colnames(lista_dist_anioxanio_xch$`54`)==FALSE)]
dim(lista_dist_anioxanio_xch$`54`)
jj<-matrix(data=NA,nrow = dim(lista_dist_anioxanio_xch$`54`)[1],ncol = length(which(anos_totales%in%colnames(lista_dist_anioxanio_xch$`54`)==FALSE)))
jj
colnames(jj)<-anos_totales[which(anos_totales%in%colnames(lista_dist_anioxanio_xch$`54`)==FALSE)]
colnames(jj)
hh<-lista_dist_anioxanio_xch$`54`
hh
hhh<-cbind(hh,jj)
colnames(hhh)
order(colnames(hhh))
class(hhh[,order(colnames(hhh))])
kk<-hhh[,order(colnames(hhh))]
class(kk)
# mismos pasos pero ahora agregando las filas que faltan, para que la matriz vuelva a ser cuadrada
length(kk)
dim(rr)
rr<-matrix(data=NA,nrow = length(which(anos_totales%in%colnames(lista_dist_anioxanio_xch$`54`)==FALSE)),ncol = length(kk))
#rr<-matrix(data=NA,nrow = length(which(anos_totales%in%colnames(lista_dist_anioxanio_xch$`14`)==FALSE)),ncol = dim(kk)[2])
rownames(rr)<-anos_totales[which(anos_totales%in%colnames(lista_dist_anioxanio_xch$`54`)==FALSE)]
ll<-rbind(kk,rr)
rownames(ll)<-c(anos_totales[which(anos_totales%in%colnames(lista_dist_anioxanio_xch$`54`)==TRUE)],anos_totales[which(anos_totales%in%colnames(lista_dist_anioxanio_xch$`54`)==FALSE)])
#order(rownames(ll))
pp<-ll[order(rownames(ll)),]
diag(pp)<-0

hace_matriz.dist_aniosxanios_todos<-function(M,vector_anios){
  mm<-matrix(data=NA,nrow = dim(M)[1],ncol = length(which(vector_anios%in%colnames(M)==FALSE)))
  colnames(mm)<-vector_anios[which(vector_anios%in%colnames(M)==FALSE)]
  Mm<-cbind(M,mm)
  Mm<-Mm[,order(colnames(Mm))]
  if(is.matrix(Mm)==TRUE){
    rr<-matrix(data=NA,nrow = length(which(vector_anios%in%colnames(M)==FALSE)),ncol = dim(Mm)[2])  
    rownames(rr)<-colnames(mm)#vector_anios[which(vector_anios%in%colnames(M)==FALSE)]
    Mm<-rbind(Mm,rr)
    }
  else{
    rr<-matrix(data=NA,nrow = length(which(vector_anios%in%colnames(M)==FALSE)),ncol = length(Mm))  
    Mm<-rbind(Mm,rr)
    rownames(Mm)<-c(vector_anios[which(vector_anios%in%colnames(M)==TRUE)],vector_anios[which(vector_anios%in%colnames(M)==FALSE)])
      #anos_totales[which(anos_totales%in%colnames(lista_dist_anioxanio_xch$`54`)==TRUE)],anos_totales[which(anos_totales%in%colnames(lista_dist_anioxanio_xch$`54`)==FALSE)])
        }
  #rr<-matrix(data=NA,nrow = length(which(vector_anios%in%colnames(M)==FALSE)),ncol = dim(Mm)[2])
  #rownames(rr)<-colnames(mm)#vector_anios[which(vector_anios%in%colnames(M)==FALSE)]
  #Mm<-rbind(Mm,rr)
  Mm<-Mm[order(rownames(Mm)),]
  diag(Mm)<-0
  Mm
}

gg<-hace_matriz.dist_aniosxanios_todos(lista_dist_anioxanio_xch$`54`,anos_totales)

hace_lista_matriz.dist_anioxanio_todos<-function(lista,anios){
  out<-list()
  #names(out)<-names(lista)
  for (i in 1:length(lista)) {
    out[[i]]<-hace_matriz.dist_aniosxanios_todos(lista[[i]],vector_anios = anios)
  }
  names(out)<-names(lista)
  out
}

dd<-hace_lista_matriz.dist_anioxanio_todos(lista_dist_anioxanio_xch,anos_totales)
class(dd$`14`)
View(dd$`14`)

# dd es la lista que debo ingresar en la funcion...para estimar el beta temporal promedio lag.1,
## podria hacer funci'on analoga para beta temp promedio con lag de 2 anios

lag_1anio_nueva<-hace_vect_xch_beta_anios_consec(dd,riqueza)
beta_temp_promedio_lag1_nuevo<-rowMeans(lag_1anio_nueva,na.rm = TRUE) # antes de hacer esto, sustituir los 0 que puse en la base en la funcion anterior por NA
beta_temp_promedio_lag1_nuevo
save(beta_temp_promedio_lag1_nuevo,file = "beta_temp_promedio_lag1.RData")

### retardo temporal de 2 anios
hace_vect_xch_beta_anios_lag2<-function(lista_distancia_entre_anios, base_original){
  out<-matrix(NA,nrow = length(unique(base_original[,3])),ncol = length(unique(base_original[,1])))
  rownames(out)<-unique(base_original[,3])
  colnames(out)<-unique(base_original[,1])
    for (i in 1:length(lista_distancia_entre_anios)) {
    d<-row(lista_distancia_entre_anios[[i]])-col(lista_distancia_entre_anios[[i]])
    diagonales<-split(lista_distancia_entre_anios[[i]],d)
    if(is.null(diagonales$`2`)== FALSE){
      diag_lag<-diagonales$`2`  
      names(diag_lag)<-rownames(lista_distancia_entre_anios[[i]])[3:nrow(lista_distancia_entre_anios[[i]])]
    }
    else{
      diag_lag<-"NA"
    }
    for (j in 1:colnames(out)) {
      for (k in 1:length(diag_lag)) {
        if((length(which(colnames(out)[j]==names(diag_lag)[k])==TRUE))>0){
          out[i,j]<-diag_lag[k] 
        }
        else {
          out
        }  
      }
    }
  }
  out
}

lag_2anios<-hace_vect_xch_beta_anios_lag2(dd,riqueza)
View(lag_2anios)
beta_temp_promedio_lag2<-rowMeans(lag_2anios,na.rm = TRUE) # antes de hacer esto, sustituir los 0 que puse en la base en la funcion anterior por NA
beta_temp_promedio_lag2
save(beta_temp_promedio_lag2,file = "beta_temp_promedio_lag2.RData")

### retardo temporal de 3 anios
hace_vect_xch_beta_anios_lag3<-function(lista_distancia_entre_anios, base_original){
  out<-matrix(NA,nrow = length(unique(base_original[,3])),ncol = length(unique(base_original[,1])))
  rownames(out)<-unique(base_original[,3])
  colnames(out)<-unique(base_original[,1])
  for (i in 1:length(lista_distancia_entre_anios)) {
    d<-row(lista_distancia_entre_anios[[i]])-col(lista_distancia_entre_anios[[i]])
    diagonales<-split(lista_distancia_entre_anios[[i]],d)
    if(is.null(diagonales$`3`)== FALSE){
      diag_lag<-diagonales$`3`  
      names(diag_lag)<-rownames(lista_distancia_entre_anios[[i]])[4:nrow(lista_distancia_entre_anios[[i]])]
    }
    else{
      diag_lag<-"NA"
    }
    for (j in 1:colnames(out)) {
      for (k in 1:length(diag_lag)) {
        if((length(which(colnames(out)[j]==names(diag_lag)[k])==TRUE))>0){
          out[i,j]<-diag_lag[k] 
        }
        else {
          out
        }  
      }
    }
  }
  out
}

lag_3anios<-hace_vect_xch_beta_anios_lag3(dd,riqueza)
View(lag_3anios)
beta_temp_promedio_lag3<-rowMeans(lag_3anios,na.rm = TRUE) # antes de hacer esto, sustituir los 0 que puse en la base en la funcion anterior por NA
beta_temp_promedio_lag3
save(beta_temp_promedio_lag3,file = "beta_temp_promedio_lag3.RData")

sss<-rbind(beta_temp_promedio_lag1_nuevo,beta_temp_promedio_lag2,beta_temp_promedio_lag3)
### revisar ch 46 y ch 19 que en lag 3 tiene en promedio NA y no en el resto, es raro, fijarse en cauntos y en que anios salio para ver si es coherente

save(df_beta_temp_prom_retardo.temp_123anio,beta_temp_promedio_lag1,beta_temp_promedio_lag2,beta_temp_promedio_lag3,file = "beta_temp_promedio_varios_lag.RData")
df_beta_temp_prom_retardo.temp_123anio<-sss
df_beta_temp_prom_retardo.temp_123anio[,]

df_beta_temp_prom_retardo.temp_123anio<-df_beta_temp_prom_retardo.temp_123anio[,order(as.numeric(colnames(df_beta_temp_prom_retardo.temp_123anio)))]
rownames(df_beta_temp_prom_retardo.temp_123anio)<-c("lag_1anio","lag_2anios","lag_3anios")
df_beta_temp_prom_retardo.temp_123anio<-t(df_beta_temp_prom_retardo.temp_123anio)
#beta_temp_promedio_lag1_nuevo->beta_temp_promedio_lag1

save(df_beta_temp_prom_retardo.temp_123anio,file = "beta_temp_promedio_varios_lag.RData")
