library(dplyr)
library(stringr)
library(rvest)
library(kableExtra)
library(tidyverse)
library(DT)
library(ggplot2)
library(treemapify)
library(zoo)
library(shiny)

function(input, output) {

  buy <- read.csv("buy.csv")
  buy_percent <- read.csv("buy_percent.csv")
  sell <- read.csv("sell.csv")
  sell_percent <- read.csv("sell_percent.csv")
  buy_map <- read.csv("buy_map.csv")
  sell_map <- read.csv("sell_map.csv")
 
  colnames(buy)[5] <- "순매수 규모, 백만달러"
  colnames(buy_percent)[5] <- "순매수 주식수, 백만주"
  colnames(buy_percent)[6] <- "순매수 강도, %"
  colnames(sell)[5] <- "순매도 규모, 백만달러"
  colnames(sell_percent)[5] <- "순매도 주식수, 백만주"
  colnames(sell_percent)[6] <- "순매도 강도, %"
  
  
  output$table1 <- DT::renderDataTable({
    
    data_filtered <- buy
    
    if (input$quarter1 != "All") {
      data_filtered <- filter(data_filtered, quarter == input$quarter1)
    }
    if (input$Industry1 != "All") {
      data_filtered <- filter(data_filtered, Industry == input$Industry1)
    }
    
    data1 <- datatable(data_filtered, options = list(
      columnDefs = list(
        list(
          targets = "Ticker",
          render = JS(
            "function(data, type, row, meta) {",
            "  if (type === 'display') {",
            "    return '<a href=\"https://finance.yahoo.com/quote/' + data + '?p=' + data + '&.tsrc=fin-srch\" target=\"_blank\">' + data + '</a>';",
            "  }",
            "  else {",
            "    return data;",
            "  }",
            "}"
          )
        )
      )
    ))
  })

  output$table2 <- DT::renderDataTable({
    data_filtered2 <- buy_percent
    
    if (input$quarter2 != "All") {
      data_filtered2 <- filter(data_filtered2, quarter == input$quarter2)
    }
    if (input$Industry2 != "All") {
      data_filtered2 <- filter(data_filtered2, Industry == input$Industry2)
    }
    
    data2 <- datatable(data_filtered2, options = list(
      columnDefs = list(
        list(
          targets = "Ticker",
          render = JS(
            "function(data, type, row, meta) {",
            "  if (type === 'display') {",
            "    return '<a href=\"https://finance.yahoo.com/quote/' + data + '?p=' + data + '&.tsrc=fin-srch\" target=\"_blank\">' + data + '</a>';",
            "  }",
            "  else {",
            "    return data;",
            "  }",
            "}"
          )
        )
      )
    ))
  })
  
  output$table3 <- DT::renderDataTable({
    data_filtered3 <- sell
    
    if (input$quarter3 != "All") {
      data_filtered3 <- filter(data_filtered3, quarter == input$quarter3)
    }
    if (input$Industry3 != "All") {
      data_filtered3 <- filter(data_filtered3, Industry == input$Industry3)
    }
    
    data3 <- datatable(data_filtered3, options = list(
      columnDefs = list(
        list(
          targets = "Ticker",
          render = JS(
            "function(data, type, row, meta) {",
            "  if (type === 'display') {",
            "    return '<a href=\"https://finance.yahoo.com/quote/' + data + '?p=' + data + '&.tsrc=fin-srch\" target=\"_blank\">' + data + '</a>';",
            "  }",
            "  else {",
            "    return data;",
            "  }",
            "}"
          )
        )
      )
    ))
  })
  
  output$table4 <- DT::renderDataTable({
    data_filtered4 <- sell_percent
    
    if (input$quarter4 != "All") {
      data_filtered4 <- filter(data_filtered4, quarter == input$quarter4)
    }
    if (input$Industry4 != "All") {
      data_filtered4 <- filter(data_filtered4, Industry == input$Industry4)
    }
    
    data4 <- datatable(data_filtered4, options = list(
      columnDefs = list(
        list(
          targets = "Ticker",
          render = JS(
            "function(data, type, row, meta) {",
            "  if (type === 'display') {",
            "    return '<a href=\"https://finance.yahoo.com/quote/' + data + '?p=' + data + '&.tsrc=fin-srch\" target=\"_blank\">' + data + '</a>';",
            "  }",
            "  else {",
            "    return data;",
            "  }",
            "}"
          )
        )
      )
    ))
  })
  
  output$treemapPlot1 <- renderPlot({
    ggplot(buy_map, aes(area = aaa, fill = percent, label = Ticker, subgroup = Industry)) +
      geom_treemap() +
      geom_treemap_subgroup_border() +
      geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.5, colour = "black", fontface = "italic", min.size = 0) +
      geom_treemap_text(colour = "white", place = "topleft", reflow = T)
  })
  
  output$treemapPlot2 <- renderPlot({
    ggplot(sell_map, aes(area = aaa, fill = percent, label = Ticker, subgroup = Industry)) +
      geom_treemap() +
      geom_treemap_subgroup_border() +
      geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.5, colour = "black", fontface = "italic", min.size = 0) +
      geom_treemap_text(colour = "white", place = "topleft", reflow = T)
  })  
  
}
