###############################################################
#write.table(all.DataRuta,file='DatosRutaAct17062014.csv',sep=';',row.names=F)
# attr(all.DataRuta,"dim") <- NULL
#constructor Dasboard detalle de ruta Ruta
#require(surveillance)
load('/var/shiny-server/www/Rutas/sources/AllDataRuta.RData') 

AuxNameAlldata <- gsub('[0123456789]','',gsub('%','',gsub('DatosRutas','',all.DataRuta[,1])))
AuxNameAlldata <- gsub('.csv','',gsub('INC','',AuxNameAlldata))

for( i in unique(AuxNameAlldata)){
          if(is.na(i)) next
        NameCliente <- as.character(i)
        prefRuta <-as.character(i)
        DataCel <- grep(NameCliente,AuxNameAlldata)
        
        datosClien <<- all.DataRuta[DataCel,]
        print(unique(datosClien$CliFnam))
        if(NROW(datosClien) < 5){print('No se cuentan con suficiente datos')
        print(i)                         
        next}
        
        ######Elimina prefijo Ruta
        Rutas1 <- gsub(paste("(",prefRuta,")","(\\ )",sep='')
                       ,'',as.character(datosClien$Ruta))
        
        ######## Función Nombres Bien
        .simpleCap <- function(x) {
          s <- strsplit(x, " ")[[1]]
          paste(toupper(substring(s, 1,1)), substring(s, 2),
                sep="", collapse=" ")
        }
        
        unique(Rutas1)
        
        RutasNomDef <- unlist(lapply(tolower(iconv(Rutas1,'UTF8')),.simpleCap))
        
        datosClien$Ruta <- RutasNomDef
        
        RutasUni <- unique(datosClien$Ruta)
        
        YearMon <- (format(datosClien$INICIO.1,'%Y%m'))
        
        rendiRuta <<- aggregate(datosClien$rendimiento,by=list(datosClien$Ruta),FUN="mean")
        colnames(rendiRuta) <- c('Ruta','Rendi')
        
        ########################################################################################################
        ###########################################################################################################
        ###C R E A A P#########################################################
        ###UI.r
        options(useFancyQuotes = FALSE)
        ##Cre dir App
        mainDir <- "/var/shiny-server/www"
        subDir <- paste('RoutesDetail',prefRuta,"App",sep='')
        mainDir2 <- file.path(mainDir, subDir)
        subDir2 <- paste("www",sep='')
        subDir3 <- paste("sources",sep='')
        
        files3 <- list.files("/var/shiny-server/www/RoutesDetail")
        
        
        
        
        if (file.exists(subDir)){
          #setwd(file.path(mainDir, subDir))
        } else {
          dir.create(file.path(mainDir, subDir))
          #dir.create(file.path(mainDir2, subDir2))
          #dir.create(file.path(mainDir2, subDir3))
          file.copy(file.path(mainDir,'RoutesDetail','ui.r')
                    ,file.path(mainDir,subDir,'ui.r'),  recursive = F,
                    copy.mode = TRUE)
          file.copy(file.path(mainDir,'RoutesDetail','server.r')
                    ,file.path(mainDir,subDir,'server.r'),  recursive = F,
                    copy.mode = TRUE)
          
          file.copy(file.path(mainDir,'RoutesDetail','sources')
                    ,file.path(mainDir2),  recursive = T,
                    copy.mode = TRUE)
          file.copy(file.path(mainDir,'RoutesDetail','www')
                    ,file.path(mainDir2),  recursive = T,
                    copy.mode = TRUE)
          #setwd(file.path(mainDir, subDir))
           
        }
        save(datosClien,file=file.path(mainDir2, subDir3,'DatosClien.RData'))
}