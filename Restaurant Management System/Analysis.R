library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
Dataset <- read_excel("D:/www/project.xlsx",n_max=420)
print(Dataset)
summary(Dataset)
year <- Dataset$`Year`
income <- Dataset$`Average Income in crores`
expenses <- Dataset$`Expenses in crores`
plot(Dataset$`Year`,Dataset$`Average Income in crores`,type='l')
Year_data <- c(2015,2016,2017,2018,2019,2020,2021)
Income_data <- c(65.5,63.8,72.4,85.5,95.6,55.9,63.3)
time_series <- ts(Income_data)
plot.ts(time_series)
result <- data.frame(Year_data,Income_data)
print(result)
f = Income_data/length(Income_data)
alpha = 2/(length(Income_data)+1)
F_2022 = sum(Income_data)/7
cat("Forecast of the year 2021 : ",F_2022)
Forecast = ((alpha)*63.3)+((1-alpha)*F_2022)        #Ft+1 = alpha * yt + (1-alpha)*Ft
cat("   Forecast for the year 2022 : ",Forecast)
x = c(2015,2016,2017,2018,2019,2020,2021)
y = c(65.5,63.8,72.4,85.5,95.6,55.9,63.3)
dell_y=c(0)

dells_y=c(0,0)

len=0
for (i in lengths(y)){
  len=len+1
}
for (i in 2:len){
  temp=y[i]-y[i-1]
  
  dell_y=append(dell_y,temp)
}
for (i in 3:len){
  temp=dell_y[i]-dell_y[i-1]
  
  dells_y=append(dells_y,temp)
}

dell_y_by_y=dell_y/ y
print(y)
print(dell_y)
print(dells_y)
print(dell_y_by_y)
co_dell_y=mean(abs(mean(dell_y)-dell_y))
co_dells_y=mean(abs(mean(dells_y)-dells_y))
co_dell_y_by_y=mean(abs(mean(dell_y_by_y)-dell_y_by_y))
print(co_dell_y)
print(co_dells_y)
print(co_dell_y_by_y)
constant=min(c(co_dell_y,co_dells_y,co_dell_y_by_y))
print(constant)
med <- median(x)
cat("Med : ",med)
x = Year_data
print(x)
t=c()
Y=c()
num=readline(prompt="Enter the value of year to be calculated : ")
num=as.integer(num)
if(constant == co_dell_y_by_y){
  tsq=c()
  tY=c()
  for(i in 1:7){
    diff=x[i]-med             #t = x - median
    t=append(t,diff)
    i=i+1
  }
  for(i in 1:7){
    ca1 = log(y[i])
    Y=append(Y,ca1)
    i=i+1
  }
  for(i in length(x)){
    ca2 = t*t
    tsq=append(tsq,ca2)
    i=i+1
  }
  for(i in length(x)){
    ca3 = t*Y
    tY=append(tY,ca3)
    i=i+1
  }
  result1 <- data.frame(x,t,y,Y,tsq,tY)
  print(result1)
  summary(result1)
  A=sum(Y)/length(x)
  b=sum(tY)/sum(tsq)
  print(b)
  a = exp(A)
  print(a)
  cat("THE EXPONENTIAL FORM OF THE EQUATION :",a,"*", "e ^",b,"(x-2018) ")       #y = a * e^bt
  c=a*exp(b*(num-2018))
  cat("   The trend value for the year ",num,"is:" ,c)
}


library(shiny)

server <- function(input, output, session) {
  
  output$plot <- renderPlot({
    incomes <- read_excel("D:/www/project.xlsx", header = TRUE, sep = ",")
    
    #Plot
    barplot(incomes$`Average Income in crores`)
    
  })
  
}

ui <- basicPage(
  h1("R Shiny Bar Plot"),
  plotOutput("plot")
  
)

shinyApp(ui = ui, server = server)