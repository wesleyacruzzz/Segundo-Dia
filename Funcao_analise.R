# Pacotes ######################################################################

library(tidyverse)

# Leitura dos dados ############################################################

dados <- read.csv("shopping_behavior_updated.csv")

# Funções ######################################################################

grafico_1 <- function(shipping,payment){
  DF <- dados %>% 
    select(Purchase.Amount..USD.,Shipping.Type,Payment.Method) %>% 
    filter(Shipping.Type %in% shipping & Payment.Method %in% payment)
  
  if(length(shipping) > 1 & length(payment) == 1){
    ggplot(DF)+
      geom_histogram(aes(x=Purchase.Amount..USD.,fill = Shipping.Type),
                     color = "white",
                     bins = 15)+
      theme_light()+
      labs(x= "Valor da Compra (Dólar)", y = "Frequência",fill = "Tipo de Envio",
           title = paste0("Histograma do valor da compra por Tipo de envio = ",
                          shipping," e método de pagamento = ",payment))+
      theme(panel.grid = element_blank(),
            title = element_text(size=9))
  } else if(length(shipping) == 1 & length(payment) > 1){
    ggplot(DF)+
      geom_histogram(aes(x=Purchase.Amount..USD.,fill = Payment.Method),color = "white",
                     bins = 15)+
      theme_light()+
      labs(x= "Valor da Compra (Dólar)", y = "Frequência",fill = "Método",
           title = paste0("Histograma do valor da compra por Tipo de envio = ",
                          shipping," e método de pagamento = ",payment))+
      theme(panel.grid = element_blank(),
            title = element_text(size=9))
  } else if (length(shipping) > 1 & length(payment) > 1) {
    ggplot(DF)+
      geom_histogram(aes(x=Purchase.Amount..USD.,fill = Payment.Method),
                     color = "white",
                     bins = 15)+
      facet_wrap(~Shipping.Type)+
      theme_light()+
      labs(x= "Valor da Compra (Dólar)", y = "Frequência",fill = "Método",
           title = paste0("Histograma do valor da compra por Tipo de envio e método de pagamento"))+
      theme(panel.grid = element_blank(),
            title = element_text(size=9))
  } else {
    ggplot(DF)+
      geom_histogram(aes(x=Purchase.Amount..USD.),color = "white",
                     bins = 15)+
      theme_light()+
      labs(x= "Valor da Compra (Dólar)", y = "Frequência",
           title = paste0("Histograma do valor da compra por Tipo de envio = ",
                          shipping," e método de pagamento = ",payment))+
      theme(panel.grid = element_blank(),
            title = element_text(size=9))
  }
}

grafico_2 <- function(payment){
  dados %>% 
    filter(Payment.Method == payment) %>% 
    select(Shipping.Type,Review.Rating) %>%
    ggplot()+
    geom_boxplot(aes(x=Shipping.Type,y=Review.Rating),fill = "grey50")+
    theme_light()+
    labs(x= "Tipo de Envio", y = "Avaliação",
         title = paste0("Boxplot da avaliação da compra por método de pagamento e de envio."))+
    theme(panel.grid = element_blank())
}

## Exemplos de uso #############################################################

grafico_1("Free Shipping","PayPal")

grafico_1("Free Shipping",c("PayPal","Venmo"))

grafico_1(c("Free Shipping","Next Day Air"),"PayPal")

grafico_1(c("Free Shipping","Next Day Air"),c("PayPal","Venmo"))

grafico_2("PayPal")
