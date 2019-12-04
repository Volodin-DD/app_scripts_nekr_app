library(shiny)
library(tidyverse)
library(xts)
library(dygraphs)

funds_leveled <- read_tsv("~/Documents/Nekrasovka/funds/data/funds_leveled.csv")
circulation <- read_tsv("~/Documents/Nekrasovka/funds/data/circulation.csv")
funds_leveled <- funds_leveled %>% select(-id) %>% 
  gather(bar, inv, key = "type", value = "INVBAR") %>% select(-type)

funds_leveled <- funds_leveled %>% distinct()

funds_leveled <- funds_leveled %>% left_join(.,
                                             data.frame(level_1 = funds_leveled$level_1 %>% unique(),
                                                        level_1_1 = c("Основной фонд", "Документ отстутсвует",
                                                                      "Фонд краеведения", "Редкий фонд",
                                                                      "Электронные ресурсы", "Микроформы")),
                                             by = "level_1") %>% select(date, level_1 = level_1_1, level_2, level_3,
                                                                        level_4, INVBAR) %>% 
  left_join(., data.frame(
    level_2 = funds_leveled$level_2 %>% unique(),
    level_2_1 = c("Основной (5)", "Периодика (56)")
  ), by = "level_2") %>% select(date, level_1, level_2 = level_2_1, level_3, level_4, INVBAR) %>% 
  left_join(., data.frame(
    level_3 = funds_leveled$level_3 %>% unique(),
    level_3_1 = c("Русский", "Национальные и зарубежные языки", "Нет данных")
  ), by = "level_3") %>% select(date, level_1, level_2, level_3 = level_3_1, level_4, INVBAR) %>% 
  left_join(., data.frame(
    level_4 = funds_leveled$level_4 %>% unique(),
    level_4_1 = c("Художественная литература", "Психология", "Нет данных",
                  "Техника и технические науки", "Универсальное содержание",
                  "Экономика", "Физическая культура и спорт", "История",
                  "Естесвенные науки", "Медицина", "Образование", "Искусство",
                  "Лингвистика", "Сельское хозяйство", "Религия", "Междисциплинарные документы",
                  "Фольклор", "Право", "Литературоведение", "Философия", "Военное дело", "Досуг",
                  "Страноведение", "Библиотековедение", "Социология", "Музейное дело", "Политика",
                  "Культура", "Науковедение", "СМИ", "Филология")
  ), by = "level_4") %>% 
  select(date, level_1, level_2, level_3, level_4 = level_4_1, INVBAR)

circulation_themed <- left_join(circulation, funds_leveled, by = "INVBAR") %>%
  filter(!is.na(date.y)) %>% select(-date.y)

funds_pub <- read_tsv("~/Documents/Nekrasovka/funds/data/price.csv")

publishers <- funds_pub %>% select(id, Publisher) %>% filter(!is.na(Publisher)) %>% filter(Publisher != "no $c") %>% 
  distinct() %>% group_by(Publisher) %>% mutate(N = n()) %>% filter(N >= 10) %>% select(Publisher)

funds_pub <- funds_pub %>% filter(Publisher %in% publishers$Publisher)

circulation_pub <- circulation %>% left_join(.,
                                             funds_pub %>% select(Publisher, Barcode, Inv) %>%
                                               gather(Barcode, Inv, key = "Type", value = "INVBAR") %>% 
                                               select(-Type) %>% distinct() %>% filter(!is.na(INVBAR)) %>% 
                                               filter(!(INVBAR %in% c("no $p", "no $x"))), by = "INVBAR") %>% 
  filter(!is.na(Publisher))