library(plotly)
library(tidyverse)

# 什麼年代建立
train<-read.csv("Data/D1/train.csv")%>%
  select("BldgType","YearBuilt")%>%
  mutate(YearBuilt2= floor(YearBuilt/10)*10)%>%
  group_by(YearBuilt2)%>%
  summarise(sum=n())

plot_ly(train, x = ~YearBuilt2, y = ~sum, type = 'scatter', mode = 'lines')%>%
  layout(xaxis = list(title = "Year"),
         yaxis = list (title = "Number"))


# 建築類型會不會因年代有明顯的區別
train<-read.csv("Data/D1/train.csv")%>%
  select("BldgType","YearBuilt")%>%
  mutate(YearBuilt2= floor(YearBuilt/10)*10)%>%
  group_by(BldgType,YearBuilt2)%>%
  summarise(sum=n())%>%
  spread(BldgType,sum, fill=0)
train


# 屬於高危險的山坡地上，有多少棟戶，分別是哪一些建築類型
train<-read.csv("Data/D1/train.csv")%>%
  select("BldgType","LandSlope","YearBuilt")%>%
  filter(LandSlope == "Sev")%>%
  mutate(n= 1)%>%
  group_by(BldgType,YearBuilt)%>%
  summarise(sum=sum(n))

train
# 1 1Fam        12 Single-family Detached 1921~1979年建立的建築
# 2 2fmCon       1 Two-family Conversion 1965年建



