 
#Materia:Laboratorio de Procesamientos de Informacion Meteorologica
####################################################################
library(ggplot2)
library(RColorBrewer)
library(ggmap)
library(ggpubr)
######################################################################################################
# PARTE A 
######################################################################################################
require(here)
a<-read.table("estaciones")
#########################LECTURA DE DATOS##########################
datos=list()
for (i in 1:length(a$V1)) {
  datos[[i]]=read.table(here("datos_cuadales",a$V1[i]),nrows = 4,
                        fill = TRUE,colClasses = c(rep("character",12)))
}
##########################LECTURA DE DATOS#########################
datos_estaciones<-list()
for (i in 1:15) {
  g=readLines(here("datos_cuadales",a$V1[i]))
  nfilas = length(g)
  nro=which(g=="----------------------------------------------------------------------------------------------------------------------------------")
  filas_read=nro[3]-nro[2]
  d<-read.fwf(here("datos_cuadales",a$V1[i]),widths = c(5,-2,rep(c(5,-1),12)),
              skip =nro[2],n =filas_read-1, fill = T,stringsAsFactors = T,
              na.strings = c("     ","    O"," S I ","B S E","R V A","C I O","N E S","NA"),
              colClasses = c(rep("character",13)))
  datos_estaciones[[i]]<-d
}

####################Obtengo los datos faltantes para cada estacion 
faltantes<-c()
for (i in 1:length(datos_estaciones)) {
  faltantes[i]= length(which(is.na(datos_estaciones[[i]][,-1])))#le saco la primer columna 
}

####################Creo un vector "Caracteristica"
ref=datos[[1]]
caract_est=c(ref[1,][1],ref[1,][4],ref[1,][7],
             ref[2,][1],ref[2,][5],ref[3,][1],
             ref[3,][3],ref[4,][1],ref[4,][5])
####################################################
#creo distintas listas vacias 
rio<-c(); cuenca<-c(); sistema_H<-c();provincia<-c()
lat<-c();lon<-c();altura<-c()

for (i in 1:length(datos)) {
  
  rio[[i]]<-paste(datos[[i]][1,][(which(datos[[i]][1,]==datos[[i]][1,1])+1):(which(datos[[i]][1,]==caract_est[2])-1)],collapse = " ")
  
  sistema_H[[i]]<-paste(datos[[i]][1,][(which(datos[[i]][1,]==caract_est[2])+1):(which(datos[[i]][1,]==caract_est[3])-1)],collapse = " ")
  
  l=as.numeric(datos[[i]][1,][(which(datos[[i]][1,]==caract_est[3])+1):(which(datos[[i]][1,]==caract_est[3])+3)])
  
  lat[[i]]<- as.numeric(format(l[1]+(l[2]/60)+(l[3]/3600),digits = 2,nsmall = 2))
  
  lg=as.numeric(datos[[i]][2,][(which(datos[[i]][2,]==caract_est[5])+1):(which(datos[[i]][2,]==caract_est[5])+3)])
  
  lon[[i]]<-as.numeric(format(lg[1]+(lg[2]/60)+(lg[3]/3600),digits = 2,nsmall = 2))
  
  provincia[[i]]<-paste(datos[[i]][3,][(which(datos[[i]][3,]==caract_est[6])+1):(which(datos[[i]][3,]==caract_est[7])-1)],collapse = " ")
  
  altura[[i]]<-paste(datos[[i]][3,][(which(datos[[i]][3,]==caract_est[7])+2)],collapse = " ")
  
  cuenca[[i]]<-paste(datos[[i]][4,][(which(datos[[i]][4,]==caract_est[8])+1):(which(datos[[i]][4,]==caract_est[9])-1)],collapse = " ")
  
}
#############################################################
#Genero un el data frame pedido
midata=data.frame(estaciones=substr(a$V1,1,4))
midata$Rio=rio
midata$Cuenca=cuenca 
midata$Sistema_Hidrico<-sistema_H 
midata$Provincia<-provincia
midata$Latitud<-lat 
midata$Longitud<-lon 
midata$altura<-altura 
midata$cantidad_de_faltantes<-faltantes
################################################################
#Lo guardo en un archivo ascci
#ruta<-"/media/dcao-05/B26C-85B4/tpespecialpp/salidas/"
ruta<-"E:/tpespecialpp/"
ruta_salida<-"E:/tpespecialpp/salidas/"
#ruta_salida<-"/media/dcao-05/B26C-85B4/tpespecialpp/salidas/"
#write.table(midata,file =paste0(ruta_salida,"tabla_informacion"),
 #           append = F,quote = F,row.names=FALSE,col.names = T,sep="\t",dec=",")
#sss=read.table("E:/tpespecialpp/salidas/tabla_informacion",sep="\t",dec=",")
####################################################################################################
#PARTE B 
####################################################################################################
#cargo el mapa y defino la region  
mapa <- get_map(location = c(left = -67, bottom = -33,
                             right=-55, top = -20 ), source = 'stamen',
                maptype = "toner-lite",zoom=6)
####################################################
latitud=(midata$Latitud)*(-1) 
longitud=((midata$Longitud)*(-1)) 
###########genero un data frame
lat_lon=data.frame(estaciones=as.character(midata$estaciones),
                   sistema=midata$Sistema_Hidrico,rio=midata$Rio,longitud,latitud)
#############################defino una paleta de colores y lo identifico con mi
myColors <- brewer.pal(5,"Set1")
names(myColors) <- levels(lat_lon$sistema)
colScale <- scale_colour_manual(name = "sistema",values = myColors)

###las identifico las estaciones por el sistema Hidrico al que pertenecen
ggmap(mapa)+
  geom_point(data=lat_lon,aes(x=longitud,y=latitud,colour= sistema),size=3)+
  colScale+
  ggtitle("Estaciones")+
  theme(plot.title = element_text(hjust = 0.5,size = 25, face = "bold"))
##########################################guardo el mapa
ggsave(filename=here("estaciones_caudales.png"),width=8, height=8.5, dpi=72)
####################################################################################################
#PARTE C
####################################################################################################
#selecciono las estaciones que tienen mas de 30 anios en datos
mas_30<-c()
for (i in 1:length(datos_estaciones)) {
  if(nrow(datos_estaciones[[i]])>=30) mas_30<-c(mas_30,i)
}
###############################################################
dat=list()
merged.data<-list()
for(i in 1:length(mas_30)){
  #convierto a mis datos de caudales en formato numerico y a los anios en caracter
  dat[[i]]=datos_estaciones[[mas_30[i]]]
  dat[[i]][,2:13]=as.numeric(as.matrix(datos_estaciones[[mas_30[i]]][,2:13]))
  dat[[i]][,1]=as.character(datos_estaciones[[mas_30[i]]][,1])
  #genero variables auxiliares 
  aux<-dat[[i]]$V1;
  b<- substr(aux[],start = 1,stop=2); b<-as.Date(b,format = "%Y")

  for (j in(1:length(aux))){
    b[j]<-as.Date(paste0("01/09/",substr(aux[j],1,2)),format = "%m/%d/%y")
  }
  var<-as.Date(ifelse(b >"2009-11-16" ,format(b, "19%y-%m-%d"),format(b)))

  todos.misdatos <- seq(var[1], var[length(var)], by="year")
  
  todo.dates.frame <- data.frame(list(V1=todos.misdatos))
  
  dat[[i]]$V1<-var
  
  merged.data [[i]]<- merge(todo.dates.frame, dat[[i]], all=T) 
}
####################################################################################################
beta=data.frame(row.names=c("sep","oct","nov","dic","ene","feb",
                            "mar","abr","may","jun","jul","agost"))
r=0
sub_selec=c()
for (i in 1:length(merged.data)) {
  if(sum(merged.data[[i]][,1]=="2000-01-09",na.rm = T)==0) next
    periodo=which((merged.data[[i]][,1]=="1981-01-09")):which((merged.data[[i]][,1]=="2000-01-09"))
    r=r+length(i)
    sub_selec=c(sub_selec,i)
    beta[[r]]=apply(merged.data[[i]][periodo,-1],2,mean,na.rm=T) 
}

#################################
#identifico los rios y  sistemas hidricos
sist_estaciones=data.frame(est=colnames(beta),sis.H=midata[mas_30[sub_selec],"Sistema_Hidrico"],
                           rio=midata[mas_30[sub_selec],"Rio"])
beta$meses=1:12
#separo  por sistema hidrico
b=unique(sist_estaciones$sis.H)

auxi1=which(as.character(sist_estaciones$sis.H)==b[1])
auxi2=which(as.character(sist_estaciones$sis.H)==b[3])
auxi3=which(as.character(sist_estaciones$sis.H)==b[2])
#genero un data frame por cada sistema hidrologico
rio_paraguay=data.frame(beta[,c(9,auxi1)]);nombres1=as.character(sist_estaciones$rio[auxi1])
rio_parana=data.frame(beta[,c(9,auxi2)]);nombres2=as.character(sist_estaciones$rio[auxi2])
laguna_mch=data.frame(beta[,c(9,auxi3)]);nombres3=as.character(sist_estaciones$rio[auxi3])
#me genero una lista con vectores de nombres de las estaciones
nombres=list(nombres1,nombres2,nombres3)

##################Graficado
#se genera una lista de data frames para poder plotear en ggplot
x=list(rio_paraguay,rio_parana,laguna_mch)
names(x)=c(b[1],b[3],b[2])

for(i in 1:length(x)){
  nm=names(x[[i]])  
  plots=list()
  for (j in seq_along(nm[-1])) {
    plots[[j]] <-ggplot(x[[i]],aes_string(x = nm[1],y=nm[j+1])) +
      geom_line(color = myColors[j]) + 
      scale_x_continuous(breaks = seq(1:12))+
      ylim(c(0,max(x[[i]])))+
      ylab("Caudales m3/s")+
      theme_bw() + 
      ggtitle(paste("Rio:",nombres[[i]][j],sep = " "))+
      theme(plot.title = element_text(hjust = 0.5))
  }
  figura= ggarrange(plotlist=plots)
  annotate_figure(figura,
                  top = text_grob(paste("Caudales .Sistema Hidrico:",names(x)[i],
                                  "periodo :1981-2000",sep=" "),color = "black",
                                   face = "bold", size = 18))
 
   ggsave(here(paste0("figura",i,".png")),width=12, height=8, dpi=72)
}

####################################################################################################
#PARTE D
#Graficar las series temporales de los maximos, minimos y medios anuales de caudal
#de cada estacion seleccionada en el item c) y calcular las tendencias en el mximo
#periodo en comun.

station<-merged.data

series=list()
for (i in 1:length(sub_selec)) {
  aux1= apply(station[[sub_selec[i]]][,-1],1,mean,na.rm=T)
  aux2=apply(station[[sub_selec[i]]][,-1],1,max,na.rm=T)
  aux3=apply(station[[sub_selec[i]]][,-1],1,min,na.rm=T)
  year=format(station[[sub_selec[i]]]$V1,format="%Y")#el nuevo que agrege
  series[[i]]=data.frame(year,aux1,aux2,aux3)
  series[[i]][mapply(is.infinite, series[[i]])] <- NA
  series[[i]][mapply(is.nan,series[[i]])]<-NA
  colnames(series[[i]])=c("year","CaudalMedia","CaudalMax","CaudalMin")
}
#########################################################################################################

#########Busco el periodo en comun########
#me genero una lista con los anios para cada

lista=list(series[[1]]$year,series[[2]]$year,series[[3]]$year,
           series[[4]]$year,series[[5]]$year,series[[6]]$year,series[[7]]$year,
           series[[8]]$year)

#busco el perido en comun
h=Reduce(intersect, lista)
#genero un array de 3 dimencionesen con mi periodo en comun

miarray=array(NA, dim = c(length(h),4,8))

for (i in 1:length(series)) {
  periodo2=which(series[[i]]$year==h[1]):which(series[[i]]$year==h[length(h)])
  miarray[,,i]=as.numeric(as.matrix(series[[i]][series[[i]]$year[periodo2],]))
}

#Mis arrays tienen NA en varias filas por lo tanto no daran un grafico continuo
#Se decide recortar el periodo a uno en donde no contengan datos faltantes .

#busco las filas que tienen NA 
m=which(is.na(miarray[,,]),arr.ind=TRUE)
row.na=unique(m[,1]);sort(row.na)
##
miarray=miarray[25:35,,]

milista=list()
for (i in 1:8) {
  milista[[i]]=as.data.frame(miarray[,,i]) 
  colnames(milista[[i]])=colnames(series[[1]])
}
names(milista)=midata$Sistema_Hidrico[mas_30[sub_selec]]

#################################################################################
var1=sist_estaciones$estaciones
nms=names(milista)

for (i in 1:length(milista)) {
  p=ggplot(milista[[i]], aes(x = year ))+
    geom_line(aes(y = CaudalMedia, colour = "CaudalMedia"),size=0.7)+
    geom_point(aes(y = CaudalMedia, colour = "CaudalMedia"))+
    stat_smooth(aes(y=CaudalMedia),method = "lm", formula = y ~ x,se = TRUE, level = 0.95) +
    geom_line(aes(y = CaudalMax, colour = "CaudalMax"),size=0.7)+
    geom_point(aes(y = CaudalMax, colour = "CaudalMax"))+
    geom_line(aes(y = CaudalMin, colour = "CaudalMin"),size=0.7)+
    geom_point(aes(y = CaudalMin, colour = "CaudalMin"))+
    scale_colour_manual("", values = c("CaudalMedia"=myColors[3], "CaudalMax"= myColors[1],"CaudalMin"=myColors[2]))+
    scale_x_continuous(breaks = seq(1992, 2002, 2))+
    #ylim(0,)+
    ylab("Caudales m3/s") + xlab("year") + theme_bw() +
    ggtitle(paste("Caudales",nms[i],var1[i],sep = " "))+ 
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(filename=paste0("series.caudales",i,".png"),width = 9,height = 9,dpi = 72)
}


######################################################################################  
