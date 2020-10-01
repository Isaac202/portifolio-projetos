library(shiny)
library(shinythemes)
library(quantmod)
library(forecast)
library(ggplot2)
library(tsbox)
library(zoo)
library(neuralnet)
library(DMwR)

#gera um intervalo de data aleatÃ³rio de 90 dias, entre 2014 e 2018
data = sample(seq(as.Date('2020/01/01'), as.Date('2020/09/11'), by="day"), 1)
data2 = data + 90

#obtem aÃ§Ãµes da IBM, no perÃ­odo gerado aleatÃ³riamente
getSymbols("ENEV3.SA", src='yahoo',from = data, to = data2)

#Fator para tipo de GrÃ¡fico
TipoGrafico = factor(c("candlesticks","line","bars","matchsticks"))
#Fator para indÃ­ce tÃ©cnico
IndiceTecnico = factor(c("ROC","CCI","ATR","TRIX","WPR"))


ui  <- fluidPage(theme = shinytheme("cerulean"),
    titlePanel("Analise tecnica da Eneva"),

    fluidRow(
        column(6,
               #controles
               h3("Analise"), 
               selectInput("TipoGrafico","Tipo de Grafico",choices = TipoGrafico),
               selectInput("IndiceTecnico","Indice Tecnico",choices = IndiceTecnico),
               numericInput("Dias", "Prever em Dias - ST", 10, min = 1),
               actionButton("Processar","Processar"),
               h3(textOutput("Tgrafico")),
               plotOutput("Graf"),
               h3(textOutput("Tmm")),
               plotOutput("Graf2"),
               h3(textOutput("Tdados")),
               tableOutput("Dados")
        ),
         column(6,
                h3(textOutput("Tindicetecnico")),
                tableOutput("Indice")  ,
                h3(textOutput("Forecast")),
                plotOutput("st"),
                h3(textOutput("Espaco")),
                plotOutput("nn"),
                h3(textOutput("Tprecisao")),
                tableOutput("Precisao")
        )
    )
)


server <- function(input, output) {
    observeEvent(input$Processar, {
        #dados da aÃ§Ãµes
        output$Dados <- renderTable({head(ENEV3.SA,10) })
        #Todos os textos
        output$Tgrafico = renderText({"Grafico"})
        output$Forecast = renderText({"Forecast"})
        output$Tmm = renderText({"Media Moveis"})
        output$Tdados = renderText({"Dados"})
        output$Tindicetecnico = renderText({"Indice Tecnico"})

        #dados dos indÃ­ce tÃ©cnico, de acordo com a seleÃ§Ã£o
        if (input$IndiceTecnico=='ROC')
            output$Indice <- renderTable({head(ROC(ENEV3.SA)) })
        else if (input$IndiceTecnico=='CCI')
            output$Indice <- renderTable({head(CCI(ENEV3.SA)) })
        else if (input$IndiceTecnico=='ATR')
            output$Indice <- renderTable({head(ATR(ENEV3.SA)) })
        else if (input$IndiceTecnico=='TRIX')
            output$Indice <- renderTable({head(TRIX(ENEV3.SA)) })
        else output$Indice <- renderTable({head(WPR(ENEV3.SA)) })
        
        #grÃ¡fico   principal     
        output$Graf <- renderPlot({ 
           chartSeries(ENEV3.SA,type=input$TipoGrafico,theme='white', TA=NULL)
            
        #adiciona o tipo de indice tÃ©cnico ao grÃ¡fico, conforme a seleÃ§Ã£o
        if (input$IndiceTecnico=='ROC')
            addROC(n = 1, type = c("continuous"), col = "red")
        else if (input$IndiceTecnico=='CCI')
            addCCI(n = 20, maType="SMA", c=0.015)
        else if (input$IndiceTecnico=='ATR')
            addATR(n=12)
        else if (input$IndiceTecnico=='TRIX')
            addTRIX(n=12)
        else addWPR(n = 14)
        })
        
        #grÃ¡fico de mÃ©dias mÃ³veis    
        output$Graf2 <- renderPlot({ 
            plot(SMA(Cl(ENEV3.SA), n = 26), type = "l")
            lines(WMA(Cl(ENEV3.SA) , n=10), col = "yellow")
            lines(DEMA(Cl(ENEV3.SA) , n=10), col = "pink")
            lines(ZLEMA(Cl(ENEV3.SA) , n=10), col = "red")
            
            #A legenda estÃ¡ comentanda porque desconfigura o grÃ¡fico
            #legend("bottomleft",lty=1,col=c("green","brown","yellow","pink","red"), 
            #legend=c("Dados","SMA","WMA","DEMA","EVWMA","ZLEMA"),bg="black",text.col="white", cex=0.7)
        })
        
        
        #SÃ©rie Tempora com ETS
        mdl = ets(Cl(ENEV3.SA), model = "ZAZ", damped = T, alpha = 0.2)
        prev1 = forecast(mdl, h=as.integer(input$Dias),levels=c(85,90))
        output$st <- renderPlot({
            plot(prev1, type="l" , main="ETS modelo ZAZ")
            lines(as.vector(Cl(ENEV3.SA)), col="red")
        })
        
 
        #Rede Neural com 5 lags
        ibm = OHLC(ENEV3.SA)
        names(ibm)<-c("open","high","low","close")
        dat = data.frame(Cl(ibm))
        dat['closem1'] = Lag(Cl(ENEV3.SA),1)
        dat['closem2'] = Lag(Cl(ENEV3.SA),2)
        dat['closem3'] = Lag(Cl(ENEV3.SA),3)
        dat['closem4'] = Lag(Cl(ENEV3.SA),4)
        dat['closem5'] = Lag(Cl(ENEV3.SA),5)
        dat = na.fill(dat, "extend")
        dat_scale = scale(dat)
        nn = neuralnet(close  ~ closem1 + closem2 + closem3 + closem4 + closem5, data=dat_scale, hidden=c(4,5),threshold =1,stepmax= 10000)
        prev = predict(nn,dat_scale ) 
        prev = unscale(prev,dat_scale)
        output$Tprecisao = renderText({"Precisão do Modelo"})
        output$Precisao <- renderTable({   accuracy(as.vector(prev),ibm$close) })
        output$nn <- renderPlot({ 
           plot(as.vector( ibm$close)  , type='l', main="Rede Neural com 5 Lags Preditoras")
           lines(prev,col='red')
        
        })
        
        
    })

}
shinyApp(ui = ui, server = server)
