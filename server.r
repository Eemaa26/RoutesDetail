######################################
##### minimal example - server.R #####
######################################
library(shiny) # load shiny at beginning at both scripts
library(shinyGridster)
library(surveillance)
source('sources/helper.r')
load('sources/DatosClien.RData')
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
  rendiruta2 <- aggregate(datosClienAux[,-c(1)],by=list(datosClienAux$Day),FUN='mean',na.rm=T)
  rendiruta2
  
})





#Route name
output$RutaSel <- renderText(
  input$Ruta)
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
         data <- data[,c(1,2)]
         colnames(data)<- c('Ruta','km/gal')
         
        a <- rHighcharts:::Chart$new()
        a$chart(type = "bar",height=600)
        a$plotOptions(column = list(stacking = "normal"))
        a$title(text = "Rendimiento por ruta")
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
  data2 <- DataDate3()
  #browser()
  layout(matrix(c(1,2,3,3),2,2,byrow=T))
  ####Rendimiento
  boxplot(data1$rendimiento)
  points(mean(data1$rendimiento),col='blue',pch=19,ylab='km/gal')
  title(main='Distribución histórica km/gal')
  ##
  boxplot(data2$fTripDistance)
  points(mean(data2$fTripDistance),col='blue',pch=19,ylab='km')
  title(main='Distribución histórica distancia ruta km')
  
  ####Tendencia 
  plot(data2$Day,data2$rendimiento,type='l',ylab='km/gal',xlab='Fecha')
  title(main='Tendencia de consumo en ruta km/gal')
  
  
  
})




  
})