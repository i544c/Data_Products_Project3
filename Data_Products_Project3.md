---
title: "Data Products Project"
author: "Author: Isaac G. Veras - (pt-br version)"
date: "October 05, 2023"
output: ioslides_presentation
runtime: shiny
---

![p1](https://github.com/i544c/Data_Products_Project3/assets/104391905/2a6114dc-7d5b-41cb-b880-d98ed151fbba)  
![p2](https://github.com/i544c/Data_Products_Project3/assets/104391905/fd30bc19-dd8d-46db-a345-956072cc2ce9)  
![p3](https://github.com/i544c/Data_Products_Project3/assets/104391905/bb10d13a-9889-48d2-8242-d58b24fd5d36)  
![p4](https://github.com/i544c/Data_Products_Project3/assets/104391905/9058eef0-aa2d-4ac2-9a78-928e243c77a8)  
![p5](https://github.com/i544c/Data_Products_Project3/assets/104391905/a38daba7-c8db-4fa8-864c-f643c86c8487)  

## Influência do peso do veículo no consumo de combustível:

Nesta apresentação, exploramos como o peso de um veículo, representado como 'peso' (`em mil libras`), afeta o consumo de combustível, representado como 'consumo'.

**`NOTA:`**

Na próxima tela, apresentamos um gráfico interativo. Use o controle deslizante para escolher o peso.

## Gráfico Interativo - Modelo 1 e 2
```{r dir, echo=FALSE}
library(shiny)
shinyApp(
        # Define UI para aplicação plotar
        shinyUI(fluidPage(

                # Titulo da aplicação
                titlePanel("Efeitos do Peso na Economia de Combustível"),

                # Barra lateral com slider para peso do carro
                sidebarLayout(
                        sidebarPanel(
                                sliderInput("sliderWT", "Qual é o peso do veículo em mil libras?", 1, 6, value = 3),
                                checkboxInput("mostrar_modelo1", "Mostrar/Ocultar Modelo 1", value = TRUE),
                                checkboxInput("mostrar_modelo2", "Mostrar/Ocultar Modelo 2", value = TRUE),
                        ),

                        # Show a plot of the generated distribution
                        mainPanel(
                                plotOutput("carPlot"),
                                h3("Predição mpg para o Modelo 1:"),
                                textOutput("pred1"),
                                h3("Predição mpg para o Modelo 2:"),
                                textOutput("pred2")
                        )
                )
        )),

        # Define server logic required to plot
        shinyServer(function(input, output) {
            mtcars$wtif <- ifelse(mtcars$wt - 3 > 0, mtcars$wt - 3, 0)
            modelo1     <- lm(mpg ~ wt,        data = mtcars)
            modelo2     <- lm(mpg ~ wtif + wt, data = mtcars)

            modelo_preditivo1 <- reactive({
                wt_input      <- input$sliderWT
                predict(modelo1,
                        newdata = data.frame(wt = wt_input))
            })

            modelo_preditivo2 <- reactive({
                wt_input      <- input$sliderWT
                predict(modelo2,
                        newdata = data.frame(wt   = wt_input, wtif = ifelse(wt_input - 3 > 0, wt_input - 3, 0)))
            })

            output$carPlot <- renderPlot({
                wt_input   <- input$sliderWT

                plot(mtcars$wt, mtcars$mpg,
                     xlab = "Peso em 1000 lbs",
                     ylab = "Milhas por galão",
                     bty  = "n",
                     pch  = 16,
                     xlim = c(1, 6),
                     ylim = c(10, 40)
                )
                if (input$mostrar_modelo1) {
                    abline(
                            modelo1,
                            col = "orange",
                            lwd = 2
                    )
                }
                if (input$mostrar_modelo2) {
                    model2lines <- predict(modelo2, newdata = data.frame(wt = 1:6, wtif = ifelse(1:6 - 3 > 0, 1:6 - 3, 0)))
                    lines(1:6, model2lines, col = "purple", lwd = 2)
                }

                legend(25, 250, c("Modelo Predição 1", "Modelo Predição 2"),
                                                      pch = 16,
                                                      col = c("orange", "purple"),
                                                      bty = "n",
                                                      cex = 1.2)
                points(wt_input, modelo_preditivo1(), col = "orange",
                                                      pch = 16,
                                                      cex = 2)
                points(wt_input, modelo_preditivo2(), col = "purple",
                                                      pch = 16,
                                                      cex = 2)
            })

            output$pred1 <- renderText({
                modelo_preditivo1()
            })

            output$pred2 <- renderText({
                modelo_preditivo2()
            })
        })
)

```

## O modelo explicado:
Ao selecionar o peso do carro usando o controle deslizante, a saída gera uma previsão de mpg do Modelo 1 `[definido como lm(mpg ~ wt)]` e uma previsão de mpg do Modelo 2 `[definido como lm(mpg ~ wtif + wt)]`. O wtif no Modelo 2 é definido como `[ifelse(wt - 3 > 0, wt - 3, 0)]`.

Enquanto no Modelo 1 apenas um modelo linear é usado, no Modelo 2 é aplicado um cálculo de taco de hóquei onde o ponto de ruptura é definido como 3.000 libras (`1,36 t`) de peso do carro. Portanto, podemos comparar o efeito do peso nas milhas por galão e as diferenças nestes dois modelos.

## Conclusão:
Concluindo, embora os modelos sejam diferentes entre si, o peso do carro e as milhas por galão têm uma correlação negativa entre si. Isso significa que quanto maior o peso `[peso]`, menos milhas por galão `[mpg]` e vice-versa.
