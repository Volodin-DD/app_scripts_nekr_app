library(shiny)
library(tidyverse)
library(xts)
library(dygraphs)

fluidPage(
  tabsetPanel(
    tabPanel(title = "Диаграммы",
             flowLayout(
               selectInput("level_1_1", "Фонд", choices = unique(funds_leveled$level_1)),
               selectInput("level_2_1", "Каталог", choices = unique(funds_leveled$level_2)),
               selectInput("level_3_1", "Язык", choices = unique(funds_leveled$level_3)),
               dateRangeInput("dates_1", label = "Выберите период", min = "2014-01-01", max = "2019-10-30",
                              start = "2015-01-01", end = "2019-09-05")
             ),
             plotly::plotlyOutput("pie", height = 1000),
             dataTableOutput("Coeff_table"))
    ,
    tabPanel(title = "Тренды",
             flowLayout(
               selectInput("level_1_2", "Фонд", choices = unique(funds_leveled$level_1)),
               selectInput("level_2_2", "Каталог", choices = unique(funds_leveled$level_2)),
               selectInput("level_3_2", "Язык", choices = unique(funds_leveled$level_3)),
               selectInput("level_4_2", "Тематика", choices = unique(funds_leveled$level_4)),
               dateRangeInput("dates_2", label = "Выберите период", min = "2014-01-01", max = "2019-10-30",
                              start = "2019-01-01", end = "2019-09-05")
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