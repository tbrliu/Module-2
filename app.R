#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(readr)
library(shiny)
library(plotly)
library(ggplot2)
library(lubridate)

bodyfat <- read_csv("BodyFat.csv")%>%
  mutate(
    age_strata=ifelse(bodyfat$AGE >= 20 & bodyfat$AGE < 40, '20 -40', ifelse(bodyfat$AGE >= 40 & bodyfat$AGE < 60, '40 - 60', '60 -81')),
    BODYFAT=as.numeric(BODYFAT),
    WEIGHT=as.numeric(WEIGHT),
    )

age_strata <- pull(bodyfat,age_strata)%>%
  unique()
mission_plot <- function(df){
  ggplot(mapping = aes(x=WEIGHT, y=BODYFAT))+
    geom_point(data=df%>%filter(selected),size=3,alpha=1)+
    scale_color_brewer(palette="Set1")+
    theme(legend.position="right")+labs(x="Weight", y="Bodyfat")
}
mission_plot2 <- function(df){
  ggplot(mapping = aes(x=HEIGHT, y=BODYFAT))+
    geom_point(data=df%>%filter(selected),size=3,alpha=1)+
    scale_color_brewer(palette="Set1")+
    theme(legend.position="right")+labs(x="Height", y ="Bodyfat")
}
mission_plot3 <- function(df){
  ggplot(mapping=aes(x=ABDOMEN,y=BODYFAT))+
    geom_point(data=df%>%filter(selected),size=3,alpha=1)+
    scale_color_brewer(palette="set1")+
    theme(legend.position="right")+labs(x="Abdomen", y ="Bodyfat")
}

ui<- fluidPage(
  titlePanel("BodyFat Analysis"),
 selectInput("age_strata","Select the age",age_strata,multiple = FALSE),
  plotOutput("mission_plot"),
  plotOutput("mission_plot2"),
 plotOutput("mission_plot3"),
 dataTableOutput("table")
)

server<-function(input,output){
  bodyfat_subset <- reactive({
    bodyfat %>%
      mutate(selected = (
        (age_strata %in% input$age_strata)
          
      )
            )
  })
  output$mission_plot <- renderPlot({
    mission_plot(bodyfat_subset())
  })
 output$mission_plot2 <- renderPlot({
   mission_plot2(bodyfat_subset())
 })
 output$mission_plot3 <- renderPlot({
   mission_plot3(bodyfat_subset())
 })
 output$table <- renderDataTable(bodyfat_subset()%>%filter(selected==1)%>%select(selected,BODYFAT,DENSITY,ADIPOSITY))
}
    

shinyApp(ui,server)