require(grid)
require(plotrix)
load('sources/DatosClien.RData')
statusOutput <- function(outputId) {
  tags$div(id=outputId, class="status_output",
           tags$div(class = 'grid_bigtext'),
           tags$p()
  )
}

justgageOutput <- function(outputId, width, height) {
  tags$div(id = outputId, class = "justgage_output"
           , style = sprintf("width:%dpx; height:%dpx", width, height))
}



# create single figure which is 5cm square
#x11(width=7,height=2)
DatesOut <<- as.Date(c(format(min(datosClien$FIN.1),'%Y-%m-%d'),
                       format(max(datosClien$FIN.1),'%Y-%m-%d'))  )

str(datosClien)
Rutas <- unique(datosClien$Ruta)
Motor <- unique(datosClien$modelo)
# subset(datosClien
#        ,Ruta==Rutas[1],select=c('PLACA','rendimiento','liGBTime'
#                                        ,'PerIdle','liSpeedTimePer','Turbo.dat','Torque.dat'
#                                        ,'venti.dat','venti.man' ))



IndicatorPlot <- function(datafun,placa){
       
  datafun[,-1] <- apply(datafun[,-1],c(1,2),function(x){
                  y <-   ifelse(is.na(x) |is.infinite(x) | is.nan(x),0,x)
                         return(y)}
          )
        
        data1  <- datafun[match(placa,datafun$Placa),] 
        labels <- c('Zona Verde','Tiempo mín','Tiempo svel.','Press Aire','% Torque','% T. Venti','%Venti man.')
          dataPlot <- character(7)
       
        Colors <- matrix(rep(NA,NROW(datafun)*NCOL(datafun)),c(NROW(datafun),NCOL(datafun)))
        colnames(Colors) <- colnames(datafun)
  Colors[,1] <- datafun[,1]
        Colors[,c(4,5,8,9)] <- apply(datafun[,c(4,5,8,9)],2,
        function(x){
          n <- NROW(x)
          return(color.scale(x,c(0,1,1),c(1,1,0),extremes=c('green','red')))
        }
        )
  
  Colors[,c(3,6,7)] <- apply(datafun[,c(3,6,7)],2,
                               function(x){
                                 n <- NROW(x)
                                 n <- NROW(x)
                                 return(color.scale(x,c(1,1,0),c(0,1,1),extremes=c('red','green')))
                               }
  )
  #browser()   
        ColorsRow <- Colors[match(placa,Colors[,1]),-c(1,2)]  
#         bluefunc(5)[findInterval(dat$value, seq(2:6))]
#         #Zona verde   

          dataPlot[1] <- paste(round(data1[3],1),'%')
        dataPlot[2] <- paste(round(data1[4],1),'%')
        dataPlot[3] <- paste(round(data1[5],1),'%')
        dataPlot[4] <- paste(round(data1[6],1),'InHg')
        dataPlot[5] <- paste(round(data1[7],1),'%')
        dataPlot[6] <- paste(round(data1[8],1),'%')
        dataPlot[7] <- paste(round(data1[9],1),'%')
          layout(t(matrix(c(1:7))), widths = 1, heights = 1)
          par(mai=c(0.1,0.1,0.1,0.1))
          for(i in 1:7){
          plot(0,type='n',xaxt='n',yaxt='n')
            points(c(0, 0), cex=20,col=ColorsRow[i],pch=20)
          text(c(-1,0),labels[i],pos = 3,font=2,cex=1)
          text(c(0,0),dataPlot[i],cex=2)
             }

}

# IndicatorPlot(rendiRuta1[1,])