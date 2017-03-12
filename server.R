
library(shiny)
library(DT)
library(shinydashboard)
library(leaflet)
library(plotly)
library(dplyr)
library(ggplot2)



# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  
  dataDpto<-read.csv("./data/dataDptoCoords.csv")
  mColDeptPop<-read.csv("./data/mColDeptPop.csv")
  
  data<-reactive({
    read.csv("./data/MalReportDptos_2007_16.csv")  
  })

  output$filmuni<-renderUI({
    data<-data()
    selectInput("depto", "Filter by Department:",c("",sort(unique(as.character(data$Nombre.Dpto)))))
  })
  
  output$distPlot <- renderPlotly({
    data<-data()
    if(input$x_var=="Total"){
      data2<-data%>%filter(Year>=input$Year[1] & Year<=input$Year[2])%>%group_by(Year)%>%summarise(Cases=sum(Cases))
      data2$Year<-factor(data2$Year)
      # Create plot
      plot_ly(data2, type="scatter" , mode="lines",x= ~Year, y= ~Cases)%>% 
        layout(scene = list(xaxis = list(title = "Year")))
      
      
    }else{
      if(input$depto==""){
        data2<-data%>%filter(Year>=input$Year[1] & Year<=input$Year[2])%>%group_by_("Year", Var=input$x_var)%>%summarise(Cases=sum(Cases))
      
      }else{
        data2<-data%>%filter(Year>=input$Year[1] & Year<=input$Year[2] & Nombre.Dpto==input$depto)%>%group_by_("Year", Var=input$x_var)%>%summarise(Cases=sum(Cases))
      
      }

      a<-ggplot(data2,aes(x=Year,y=Cases,col=Var))+geom_point(size=2)+geom_line(size=1)+
        scale_x_continuous(breaks = seq(as.numeric(input$Year[1]),as.numeric(input$Year[2]),1))+
        scale_color_discrete(name=input$x_var)+xlab("Year")+
        theme_classic()+theme(panel.border=element_rect(colour = "black", fill=NA),legend.position="bottom",legend.title=element_blank())+xlab("")
      ggplotly(a)
    }
  })
  
  output$table<-renderDataTable({
    data<-data()
    if(input$x_var=="Total"){
      datatable(data%>%filter(Year>=input$Year[1] & Year<=input$Year[2])%>%group_by(Year)%>%summarise(Cases=sum(Cases))%>%arrange(desc(Cases)))
    }else{
      if(input$depto==""){
        DT::datatable(data%>%filter(Year>=input$Year[1] & Year<=input$Year[2])%>%group_by_("Year", Var=input$x_var)%>%summarise(Cases=sum(Cases))%>%arrange(desc(Cases)))
      }else{
        DT::datatable(data%>%filter(Year>=input$Year[1] & Year<=input$Year[2] & Nombre.Dpto==input$depto)%>%group_by_("Year", Var=input$x_var)%>%summarise(Cases=sum(Cases))%>%arrange(desc(Cases)))
      }
    }
  })
  
  output$map<-renderLeaflet({
    data<-data()
    # oDeptos<-oDeptos()
    data3<-data%>%filter(Year==input$Year[2])%>%group_by(Nombre.Dpto)%>%summarise(Cases=sum(Cases))
    # mColDeptPop$Year<-mColDeptPop$Año
    mColDeptPop<-mColDeptPop%>%filter(Year==input$Year[2])

    data3$Nombre.Dpto<-as.character(data3$Nombre.Dpto)
    dataDpto$Nombre.Dpto<-as.character(dataDpto$Nombre.Dpto)
    a2<-left_join(data3,dataDpto,by=c("Nombre.Dpto"))

    mColDeptPop$DEPTO<-as.character(mColDeptPop$DEPTO)
    a2<-left_join(a2,mColDeptPop,by=c("Nombre.Dpto"="DEPTO"))

    a2$Nombre.Dpto<-factor(a2$Nombre.Dpto)
    a2<-a2%>%filter(!is.na(Longitud) | !is.na(Latitud))

    a2$Longitud<-gsub(",",".",a2$Longitud)
    a2$Latitud<-gsub(",",".",a2$Latitud)

    a2<-a2[!(is.na(a2$Latitud)) & !(is.na(a2$Longitud)),]

    leaflet() %>% setView( -74.2, 3.2 ,zoom=5)%>%
      addTiles() %>%
        addCircles(lng=as.numeric(a2$Longitud),lat=as.numeric(a2$Latitud),radius = a2$Cases,color ="red",popup =paste("Department:",a2$Nombre.Dpto,"<br> Cases:",a2$Cases," Population:",a2$Poblacion))
  })

})