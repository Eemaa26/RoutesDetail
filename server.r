######################################
##### minimal example - server.R #####
######################################
library(shiny) # load shiny at beginning at both scripts
library(shinyGridster)
library(surveillance)
library(boot)
library(Hmisc)
library(ggplot2)
library(plotrix)
source('sources/helper.r')
load('sources/DatosClien.RData')
load('sources/NameClien.RData')
shinyServer(function(input, output) { # server is defined within
  # these parentheses
#Machine, date and engine select
  
DataDate <- reactive({ 
fechaIni <- strptime(as.character(input$dates[1]),'%Y-%m-%d')
fechaFin <- strptime(as.character(input$dates[2]),'%Y-%m-%d')
#select customer data
datosClienAux <- subset(datosClien
                        ,Ruta==input$Ruta & (INICIO > fechaIni  & INICIO < fechaFin) & modelo %in% input$Motor
                        ,select=c('PLACA','rendimiento','liGBTime'
                                ,'PerIdle','liSpeedTimePer','Turbo.dat','Torque.dat'
                                ,'venti.dat','venti.man' ))
rendiRuta1 <- aggregate(datosClienAux[,c(-1)],by=list(datosClienAux$PLACA),FUN='mean',na.rm=T)

# rendiRuta1$rendis <- scale(rendiRuta1[,2])
colnames(rendiRuta1)[1] <- c('Placa')
 rendiRuta1 <- rendiRuta1[order(rendiRuta1[,2]),]
rendiRuta1
 })

##############Datos para análisis
DataDate3 <- reactive({ 
  fechaIni <- strptime(as.character(input$dates[1]),'%Y-%m-%d')
  fechaFin <- strptime(as.character(input$dates[2]),'%Y-%m-%d')
  #select customer data
  datosClienAux <- subset(datosClien
                          ,Ruta==input$Ruta & (INICIO > fechaIni  & INICIO < fechaFin) & modelo %in% input$Motor
                          ,select=c('PLACA','rendimiento','liGBTime'
                                    ,'PerIdle','liSpeedTimePer','Turbo.dat','Torque.dat'
                                    ,'venti.dat','venti.man','INICIO','fTripDistance'))

  datosClienAux$Day <- as.Date(round(datosClienAux$INICIO,"days"))
  datosClienAux$Mon <- as.Date(trunc(datosClienAux$INICIO,"months"))
  rendiruta2 <- aggregate(datosClienAux[,-c(1)],by=list(datosClienAux$Day),FUN='mean',na.rm=T)
  list(datos=datosClienAux,acum=rendiruta2)
  
})
 
output$downloadData <- downloadHandler(  
  filename = function() { 
    paste(input$dates[1],'dataRuta', input$dates[2], '.csv', sep='') 
  },               
  content = function(file) {  
    write.csv(DataDate(), file,row.names=F) 
  }  
)


#Route name
output$RutaSel <- renderText(
  input$Ruta)
output$NameCliente <- renderText(as.character(NameCliente))
#Acumulado por ruta
DataDate2 <- reactive({ 
  fechaIni <- strptime(as.character(input$dates[1]),'%Y-%m-%d')
  fechaFin <- strptime(as.character(input$dates[2]),'%Y-%m-%d')
  #select customer data
  datosClienAux <- subset(datosClien
                          ,(INICIO > fechaIni  & INICIO < fechaFin) & modelo %in% input$Motor
                          ,select=c('Ruta','rendimiento','liGBTime'
                                    ,'PerIdle','liSpeedTimePer','Turbo.dat','Torque.dat'
                                    ,'venti.dat','venti.man' ))
  rendiRuta1 <- aggregate(datosClienAux[,c(-1)],by=list(datosClienAux$Ruta),FUN='mean',na.rm=T)
  
  # rendiRuta1$rendis <- scale(rendiRuta1[,2])
  colnames(rendiRuta1)[1] <- c('Ruta')
  rendiRuta1 <- rendiRuta1[order(rendiRuta1[,2]),]
  rendiRuta1
})
 
output$flow <- renderChart({
          
         data <- DataDate2() 
         if(NROW(data) < 10){
           height1 <- 500
         }else {
           
           height1 <- NROW(data)*19
         }
         
         data <- data[,c(1,2)]
         colnames(data)<- c('Ruta','km/gal')
         
        a <- rHighcharts:::Chart$new()
        a$chart(type = "bar",height=height1)
        a$plotOptions(column = list(stacking = "normal"))
        a$title(text = "Rendimiento por ruta en km/gal")
        a$yAxis(title = list(text = "km/gal"))
     
        x <- as.data.frame(data[,2])
         colnames(x) <- 'km/gal'
        a$xAxis(categories = data[,1])
        a$data(x)
        a
})


################################################################################## 
# 
#         DETALLE DE RUTAS
# 
################################################################################### 
#Output gage function for a fuel comsumtion
rendirutaUI.fun <- function(){
  
  rendiRuta1 <- DataDate()
  
  N <- NROW(rendiRuta1)
  
  lapply( rendiRuta1$Placa,
          function(i){
            fixedRow(
              column(2,
                     justgageOutput(paste('live_gauge',i,sep=''), width=130, height=94),
                     h5(paste(i))
              ),
              column(10,
                     plotOutput(paste("plot",i,sep=''),width = "600px", height = "86px")
              ))
          })
  
  
}

#UI output

output$VehicleOut <- renderUI({
  rendirutaUI.fun ()
  
})
  

  # Need local so that each item gets its own number. Without it, the value
  # of i in the renderPlot() will be the same across all instances, because
  # of when the expression is evaluated.


#Create output variable from MachineID
  placas <- isolate(DataDate())
  lapply(placas[,1],function(placa){
            
          local({
            gaugename <- paste('live_gauge', placa, sep="")
            output[[gaugename]] <- reactive({
            data2 <- DataDate()  
          
            round(data2$rendimiento[match(placa,data2$Placa)], 1)
                })
            })
    })

#Create plot from variables

  lapply(placas[,1],function(placa){
    local( {
      plotname <- paste('plot',placa,sep='')
      output[[plotname]] <- renderPlot({
    data1 <- DataDate()
    IndicatorPlot(data1, placa)
   },width=700, height=100)
      
        })
  
  })
###########################################################################################
#
#                                 MODULO ANÀLISIS DE RUTAS
###########################################################################################
output$Analisis1 <- renderPlot({
  
  data1 <-  DataDate()
  data2 <- DataDate3()$acum
  #browser()
  layout(matrix(c(1,2,3,3),2,2,byrow=T))
  ####Rendimiento
  boxplot(data1$rendimiento)
  points(mean(data1$rendimiento),col='blue',pch=19,ylab='km/gal')
  legend('topleft',pch=19,col='blue',legend='Media')
  text(mean(data1$rendimiento),labels=round(mean(data1$rendimiento),2),pos=1,col='blue')
  title(main='Distribución histórica km/gal')
  ##
  boxplot(data2$fTripDistance)
  points(mean(data2$fTripDistance),col='blue',pch=19,ylab='km')
  legend('topleft',pch=19,col='blue',legend='Media')
  legend('topright',legend=paste('de',round(sd(data2$fTripDistance,na.rm=T),2)))
  text(mean(data2$fTripDistance),labels=round(mean(data2$fTripDistance,na.rm=T),2),pos=1,col='blue')
  
  title(main='Distribución histórica distancia ruta km')
    ####Tendencia 
  plot(data2$Day,data2$rendimiento,type='n',ylab='km/gal',xlab='Fecha'
       ,ylim=c(0,max(data2$rendimiento)*1.25))
 
 # polygon(c(rev(ci[,1]), (ci[,1])), c(rev(ci$x[ ,2]), ci$x[ ,1]), col = 'grey80', border = NA)
  # model
  lines(data2$Day,data2$rendimiento)
  title(main='Tendencia de consumo en ruta km/gal')
  
})

HeigthFun <- reactive({
  n <- NROW(DataDate3()$datos)
  if(n < 12){
    400 
  }else {400+(10/12)*(n-12)}
      })
  
  
  


output$Analisis2 <- renderPlot({
  data1 <-  DataDate()
  data2 <- DataDate3()$acum
 bsci <- function(x){  
   if(length(x) == 1)return(rep(x,2))
   if(sd(x)== 0 & length(x)>1) x[1] <- x[2]+rnorm(1,0,0.5)
   mean.boot <-   function(x,ind)return(c(mean(x[ind]),var(x[ind])/length(ind)))
   
   out <- boot(x,mean.boot,999)
   ci <- as.vector(boot.ci(out)$student[,4:5])

   return(ci)
 }
 data3 <- DataDate3()$datos
 ci <- aggregate(data3$rendimiento,by=list(data3$PLACA),FUN=bsci)
  #browser()
  aux1<- do.call(rbind,list(ci$x))
  rownames(aux1) <- 1:NROW(aux1)
  CI1 <- data.frame(ci[,1],aux1[match(rownames(ci), rownames(aux1)),])
  colnames(CI1) <- c('Placa','UL','LL')
  CI2<- merge(CI1,data1[,1:2],by='Placa')
  
  par()
  plotCI(CI2[,4],1:NROW(CI2),ui=CI2[,3], li=CI2[,2]
         ,err="x",lwd=2,col="red",scol="blue",xlim=c(0,16),yaxt='n',ylab='',xlab='km/gal')
  axis(2,at=1:NROW(CI2),labels=CI2[,1],las=1)
  axTick <- sort(c(seq(0,15,0.5),mean(CI2[,4])))
  axis(1,at=axTick,labels=round(axTick,1),cex=0.7)
  abline(v=mean(CI2[,4]),col='red',lty=2)

  
  str(data3)
  cor(na.omit(data3[,c(2:4,6:9)]))
  clust <- kmeans(na.omit(data3[,c(2:4,6:9)]),centers=3)
  table(clust$cluster)
  summary(lm(rendimiento ~ liGBTime+PerIdle+Turbo.dat+Torque.dat+venti.dat,data=data3))
  title(main='Intervalos de confianza al 95 % para el rendimiento',cex=0.9)

}, height=HeigthFun)

  
})