library(shiny)
library(tidyverse)
library(xts)
library(dygraphs)

options(stringsAsFactors = F, digits = 2)

function(input, output) {
  
  df <- reactive({
    df_1 <- funds_leveled %>% filter(level_1 == input$level_1_1,
                                     level_2 == input$level_2_1,
                                     level_3 == input$level_3_1) %>% filter(date <= input$dates_1[2]) %>% 
      group_by(level_4) %>% summarise(Stock = n())
    df_2 <- circulation_themed %>% filter(level_1 == input$level_1_1,
                                          level_2 == input$level_2_1,
                                          level_3 == input$level_3_1) %>% filter(date.x <= input$dates_1[2],
                                                                                 date.x >= input$dates_1[1]) %>% 
      group_by(level_4) %>% summarise(Delivery = n())
    
    df <- full_join(df_1, df_2, by = "level_4")
    
    colnames(df) <- c("Тематика", "Объём фонда", "Выдача")
    return(df)
  })
  
  output$pie <- plotly::renderPlotly({
                                     
    plotly::plot_ly() %>% 
      plotly::add_pie(data = df(), labels = ~Тематика, values = ~`Объём фонда`,
                      name = "Фонд", domain = list(row = 0, column = 0)) %>% 
      plotly::add_pie(data = df(), labels = ~Тематика, values = ~Выдача,
                      name = "Выдача", domain = list(row = 0, column = 1)) %>% 
      plotly::layout(title = "Доли тематик в фондах и выдаче", showlegend = T,
                     grid=list(rows=1, columns=2),
                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     paper_bgcolor = "transparent")
  })
  
  output$Coeff_table <- renderDataTable({
    
    df <- df() %>% mutate(`Коэффициент соответствия` =
                      (Выдача / sum(Выдача, na.rm = T)) /
                      (`Объём фонда` / sum(`Объём фонда`, na.rm = T)))
    
    df
  })
  
  xts_full <- reactive({
    stock <- funds_leveled %>% 
      filter(level_1 == input$level_1_2, level_2 == input$level_2_2, level_3 == input$level_3_2,
             level_4 == input$level_4_2) %>% select(date, INVBAR) %>% distinct() %>% 
      group_by(date) %>% summarise(Stock = n()) %>% mutate(Cum_stock = cumsum(Stock)) %>% 
      filter(date >= input$dates_2[1], date <= input$dates_2[2]) %>% 
      full_join(data.frame(
        date = input$dates_2[1] + 0:(as.numeric(input$dates_2[2] - input$dates_2[1])),
        x = 0
      ), ., by = "date") %>% mutate(Stock = if_else(is.na(Stock), 0, as.double(Stock))) %>% 
      mutate(Cum_stock_1 = cumsum(Stock) + Cum_stock[which(!is.na(Cum_stock))[1]] - 
               Stock[which(!is.na(Cum_stock))[1]]) %>% 
      select(date, Stock, Cumulative_stock = Cum_stock_1) %>% 
      full_join(
        .,
        funds_leveled %>% select(date, INVBAR) %>% distinct() %>% 
          group_by(date) %>% summarise(Stock_full = n()) %>% 
          mutate(Cum_stock_full = cumsum(Stock_full)) %>% 
          filter(date >= input$dates_2[1], date <= input$dates_2[2]),
        by = "date"
      ) %>% mutate(Stock_full = if_else(is.na(Stock_full), 0, as.double(Stock_full))) %>% 
      mutate(Cum_stock_full_1 = cumsum(Stock_full) + Cum_stock_full[which(!is.na(Cum_stock_full))[1]] - 
               Stock_full[which(!is.na(Cum_stock_full))[1]]) %>% 
      select(date, Stock, Cumulative_stock, Stock_full, Cumulative_stock_full = Cum_stock_full_1)
    
    full <- circulation_themed %>% filter(level_1 == input$level_1_2,
                                          level_2 == input$level_2_2,
                                          level_3 == input$level_3_2,
                                          level_4 == input$level_4_2) %>% 
      select(date = date.x, INVBAR) %>% distinct() %>% 
      group_by(date) %>% summarise(Delivery = n()) %>% 
      filter(date >= input$dates_2[1], date <= input$dates_2[2]) %>%
      full_join(data.frame(
        date = input$dates_2[1] + 0:(as.numeric(input$dates_2[2] - input$dates_2[1])),
        x = 0
      ), ., by = "date") %>% mutate(Delivery = if_else(is.na(Delivery), 0, as.double(Delivery))) %>% 
      select(date, Delivery) %>% 
      full_join(
        .,
        circulation_themed %>% select(date = date.x, INVBAR) %>% distinct() %>% 
          group_by(date) %>% summarise(Delivery_full = n()) %>% 
          filter(date >= input$dates_2[1], date <= input$dates_2[2]),
        by = "date"
      ) %>% mutate(Delivery_full = if_else(is.na(Delivery_full), 0, as.double(Delivery_full))) %>% 
      left_join(., stock, by = "date")
    
    full$coeff <- ifelse(full$Delivery_full == 0, 1, (full$Delivery / full$Delivery_full)) / 
      (full$Cumulative_stock / full$Cumulative_stock_full)
    
    return(full)
  })
  
  output$full <- renderDygraph({
    sd <- xts(xts_full()[, c(2, 4)], order.by = xts_full()[, 1])
    colnames(sd) <- c("Выдача", "Поступления")
    dygraph(sd) %>% dyRoller(rollPeriod = 7) %>% dyRangeSelector() %>% 
      dyOptions(colors = c("#28637a", "#c06b6c"))
  })
  
  output$coeff <- renderDygraph({
    coeff <- xts(xts_full()[, 8], order.by = xts_full()[, 1])
    coeff <- cbind(rollapply(coeff, input$short, mean), rollapply(coeff, input$long, mean))
    colnames(coeff) <- c("Короткая СС", "Длинная СС")
    dygraph(coeff) %>% dyRangeSelector() %>% dyOptions(colors = c("#82b27f", "#c06b6c"),
                                                       fillGraph = T, fillAlpha = 0.3,
                                                       drawGrid = F) %>% 
      dyCrosshair(direction = "both")
  })
  
  isbn <- reactive({
    if (str_length(input$isbn)[1] > 1) {
      x <- str_split(input$isbn, "\n") %>% unlist()
      return(x)
    } else {
      x <- funds_pub$isbn %>% unique()
      return(x)
    }
  })
  
  publishers <- reactive({
    if (input$check) {
      x <- input$pub
      return(x)
    } else {
      x <- funds_pub$Publisher %>% unique()
      return(x)
    }
  })
  
  data <- reactive({
    x <- funds_pub %>% filter(Publisher %in% publishers(), isbn %in% isbn())
    return(x)
  })
  
  circ_pub <- reactive({
    if (!input$check) {
      x <- circulation_pub
    } else {
      x <- circulation_pub %>% filter(Publisher %in% input$pub) %>%
        filter(date >= as.Date("2018-01-01")) %>% group_by(date) %>% 
        summarise(Выдача = n())
      colnames(x) <- c("Дата", "Выдача")
      x <- xts(x$Выдача, order.by = x$Дата)
      x <- cbind(
        rollapply(x, 30, mean),
        rollapply(x, 90, mean)
      )
      colnames(x) <- c("Короткая СС", "Длинная СС")
      return(x)
    }
  })
  
  output$plot <- renderDygraph({
    dygraph(circ_pub()) %>% dyRangeSelector() %>% dyOptions(colors = c("#82b27f", "#c06b6c"),
                                                            fillGraph = T, fillAlpha = 0.3,
                                                            drawGrid = F) %>% 
      dyCrosshair(direction = "both")
  })
  output$dwn <- downloadHandler(
    filename = function() {
      paste(input$pub[1], ".csv", sep = "")
    },
    content = function(file) {
      write_excel_csv(data(), file, delim = "\t")
    }
  )
}