library(shiny)
library(tidyverse)
library(xts)
library(dygraphs)

options(stringsAsFactors = F, digits = 2)

fluidPage(
  tabsetPanel(
    tabPanel(title = "Диаграммы",
             flowLayout(
               selectInput("level_1_1", "Фонд", choices = unique(funds_leveled$level_1) %>% sort()),
               selectInput("level_2_1", "Каталог", choices = unique(funds_leveled$level_2) %>% sort()),
               selectInput("level_3_1", "Язык", choices = unique(funds_leveled$level_3) %>% sort()),
               dateRangeInput("dates_1", label = "Выберите период", min = "2014-01-01",
                              max = as.character(Sys.Date()),
                              start = "2015-01-01", end = as.character(Sys.Date())),
               submitButton("Вывести")
             ),
             plotly::plotlyOutput("pie", height = 1000),
             dataTableOutput("Coeff_table"))
    ,
    tabPanel(title = "Тренды",
             flowLayout(
               selectInput("level_1_2", "Фонд", choices = unique(funds_leveled$level_1) %>% sort()),
               selectInput("level_2_2", "Каталог", choices = unique(funds_leveled$level_2) %>% sort()),
               selectInput("level_3_2", "Язык", choices = unique(funds_leveled$level_3) %>% sort()),
               selectInput("level_4_2", "Тематика", choices = unique(funds_leveled$level_4) %>% sort()),
               dateRangeInput("dates_2", label = "Выберите период", min = "2014-01-01",
                              max = as.character(Sys.Date()),
                              start = "2019-01-01", end = as.character(Sys.Date())),
               submitButton("Вывести")
             ),
             dygraphOutput("full"),
             dygraphOutput("coeff"),
             flowLayout(
               sliderInput("short", label = "Короткая скользящая средняя", min = 10, max = 90, value = 30),
               sliderInput("long", label = "Длинная скользящая средняя", min = 30, max = 270, value = 90)
             )
    ),
    tabPanel(
      title = "Издательства",
      sidebarLayout(
        sidebarPanel(
          selectInput("pub", "Выберите издательства", choices = sort(unique(funds_pub$Publisher)), multiple = T),
          checkboxInput("check", "Использовать поиск по издательству"),
          textAreaInput("isbn", "Введите ISBN"),
          submitButton("Вывести")
        ),
        mainPanel(
          dygraphOutput("plot"),
          downloadButton("dwn", "Download")
        )
      )
    )
  ))