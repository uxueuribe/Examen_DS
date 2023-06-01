library("httr")
library("dplyr")
library("jsonlite")

url <- paste0('https://www.givefood.org.uk/api/2/foodbanks/')
request <- GET(url)
df <- as.data.frame(fromJSON(content(request, "text", encoding="utf-8")))
url2<-paste0("https://www.givefood.org.uk/api/2/needs/")
request2 <- GET(url2)
df2<-as.data.frame(fromJSON(content(request2, "text", encoding="utf-8")))
colnames(df2)
colnames(df)
df2$foodbank<-df2$foodbank["name"]
colnames(df2[,3])<-c("name")
colnames(df2)<-c("id","found","name","needs","excess" ,"self")
#############
#localice a través de un nombre aproximado los ids de los Bancos de Alimentos que hay en esa localidad
df_junto<-merge(df,df2,by="name")
colnames(df_junto)

funcion1<-function(country){
  consulta1<-df_junto%>%
    filter(country==country)%>%
    select(id,found,needs)
  
  consulta2<-consulta1%>%
    arrange((found))%>%
    head(1)
  n<-(consulta2$needs)
  return(n) 
}
funcion1("England")

