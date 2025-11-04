library(dplyr)
library(readxl)
library(writexl)
library(ggplot2)
library(lubridate)
library(tidyr)
library(rstudioapi)
library(stringr)
library(caret)
library(pROC)
library(forcats)
library(glmnet)
library(broom)
library(xgboost)
library(randomForest)
library(rpart)
library(rpart.plot)
library(e1071)
library(nnet)
library(DiagrammeR)
library(PRROC)



# =============================================================================
#  Етап 1: Зареждане и предварителна обработка на данните
# =============================================================================

script_dir <- dirname(getActiveDocumentContext()$path)

file1 <- file.path(script_dir, "СКПЖПС Вагони.xlsx")
file2 <- file.path(script_dir, "СКПЖПС Локомотиви.xlsx")

df1 <- read_excel(file1, skip = 1, col_types = c("date", rep("text", 8)))
df2 <- read_excel(file2, skip = 1, col_types = c("date", rep("text", 8)))

names(df1)[2] <- "№ ПЖПС"
names(df2)[2] <- "№ ПЖПС"

df <- bind_rows(df1, df2)

names(df) <- gsub("[\r\n]", " ", names(df)) 
names(df) <- gsub("\\s+", " ", names(df))
names(df) <- trimws(names(df))

df <- df %>%
  filter(!is.na(Дата))

colnames(df) <- c(
  "Дата",
  "№ ПЖПС",
  "ЖП превозвач",
  "Горен габарит № Влак",
  "Страничен габарит № Влак",
  "Надтоварена колоос № Влак",
  "Осно натоварване/ t.",
  "Окопан бандаж № Влак",
  "Загрята букса/ °C № Влак"
)

df <- df %>%   # Добавяме "Вид ПЖПС" и го местим след "Дата"
  mutate(
    `Вид ПЖПС` = ifelse(
      grepl("^\\d{10,12}$", `№ ПЖПС`) | grepl("неясно показание", `№ ПЖПС`, ignore.case = TRUE),
      "Вагон",
      "Локомотив"
    )
  ) %>%
  relocate(`Вид ПЖПС`, .after = Дата) %>%
  arrange(Дата)

print(head(df))

df <- df %>% # Добавя празни колони "Тип ПЖПС" и "Местоположение чекпоинт"
  mutate(
    `Тип ПЖПС` = NA_character_,
    `Местоположение чекпоинт` = NA_character_
  ) %>%
  relocate(`Тип ПЖПС`, .after = `ЖП превозвач`) %>%
  relocate(`Местоположение чекпоинт`, .after = `Тип ПЖПС`)

source_cols <- c(
  "Горен габарит № Влак",
  "Страничен габарит № Влак",
  "Надтоварена колоос № Влак",
  "Осно натоварване/ t.",
  "Окопан бандаж № Влак",
  "Загрята букса/ °C № Влак"
)

abbreviations <- c("Стм", "Пзк", "Ткл", "Сп") # Списък на съкращенията, които търсим

# Попълване на съществуващата колона „Местоположение чекпоинт“
for (i in 1:nrow(df)) {  
  for (col in source_cols) {
    value <- as.character(df[i, col])
    if (!is.na(value)) {
      found <- abbreviations[abbreviations %in% unlist(strsplit(value, "\\s+"))]
      if (length(found) > 0) {
        df$`Местоположение чекпоинт`[i] <- found[1]
        df[i, col] <- trimws(gsub(found[1], "", value))
        break
      }
    }
  }
}

set.seed(123)

df$`Осно натоварване/ t.` <- sapply(seq_len(nrow(df)), function(i) {
  x <- as.character(df$`Осно натоварване/ t.`[i])
  
  if (is.na(x)) return(NA)
  
  numbers <- unlist(regmatches(x, gregexpr("\\d+,\\d+", x)))
  
  if (length(numbers) == 0) return(NA)
  
  numbers <- sapply(numbers, function(num) {
    num_clean <- gsub("\\s", "", num)
    parts <- unlist(strsplit(num_clean, ","))
    
    total_digits <- nchar(parts[1]) + nchar(parts[2])
    
    if (total_digits == 5) {
      return(num_clean)
    } else if (total_digits < 5) {
      add_digit <- sample(0:9, 1)
      parts[2] <- paste0(parts[2], add_digit)
      return(paste(parts, collapse = ","))
    } else {
      parts[2] <- substr(parts[2], 1, 3)
      return(paste(parts, collapse = ","))
    }
  })
  
  paste(numbers, collapse = "; ")
})

# Оставяме само числото преди "/" в колоната "Надтоварена колоос № Влак"
df$`Надтоварена колоос № Влак` <- sapply(df$`Надтоварена колоос № Влак`, function(x) {
  x <- as.character(x)
  if (is.na(x)) return(NA)
  
  if (grepl("/", x)) {
    return(unlist(strsplit(x, "/"))[1])
  } else {
    return(x)
  }
})

df <- df %>%
  relocate(`ЖП превозвач`, .before = `Вид ПЖПС`)

# Премахваме всичко след "/" (и евентуален интервал) в габаритните и температурната колона
df$`Горен габарит № Влак`     <- sub("\\s*/.*", "", df$`Горен габарит № Влак`)
df$`Страничен габарит № Влак` <- sub("\\s*/.*", "", df$`Страничен габарит № Влак`)
df$`Загрята букса/ °C № Влак` <- sub("\\s*/.*", "", df$`Загрята букса/ °C № Влак`)

# Преместваме съкращенията от габаритните, колоосната и температурната колона в "Местоположение чекпоинт"
for (col in c("Горен габарит № Влак", 
              "Страничен габарит № Влак", 
              "Надтоварена колоос № Влак",
              "Загрята букса/ °C № Влак")) {
  
  suffix <- sub("^\\s*\\d+[\\d,\\.]*\\s*(.*)$", "\\1", df[[col]])
  suffix <- trimws(suffix)
  
  df$`Местоположение чекпоинт` <- ifelse(
    suffix != "" & !is.na(suffix) & 
      (is.na(df$`Местоположение чекпоинт`) | df$`Местоположение чекпоинт` == ""),
    suffix,
    df$`Местоположение чекпоинт`
  )
  
  df[[col]] <- sub("^\\s*([0-9,\\.]+).*$", "\\1", df[[col]])
  df[[col]] <- trimws(df[[col]])
}

df$`Местоположение чекпоинт` <- ifelse(df$`Местоположение чекпоинт` == "Пзк", "Пост 1 гара Пазарджик",
                                       ifelse(df$`Местоположение чекпоинт` == "Сп",  "Пост 1 гара Септември",
                                              ifelse(df$`Местоположение чекпоинт` == "Стм", "Пост 2 гара Стамболийски",
                                                     ifelse(df$`Местоположение чекпоинт` == "Ткл", "Пост 2 гара Тодор Каблешков",
                                                            df$`Местоположение чекпоинт`))))

# Обработка на "Окопан бандаж № Влак" и създаване на новата колона "Съотношение пикова/средна сила"

vals <- as.character(df$`Окопан бандаж № Влак`)
num_part <- sub("^([0-9]+).*", "\\1", vals)
rest_part <- sub("^[0-9]+\\s*", "", vals)

numbers <- gsub("-", "", rest_part)
numbers <- gsub("/", ";", numbers)
numbers <- gsub("[^0-9,;]", "", numbers)
numbers <- gsub(";", "; ", numbers) 

abbr <- gsub("[0-9,./\\-\\s]", "", rest_part)

# Създаваме/вмъкваме новата колона след "Окопан бандаж № Влак"
pos <- which(names(df) == "Окопан бандаж № Влак")
df <- cbind(
  df[, 1:pos],
  `Съотношение пикова/средна сила` = numbers,
  df[, (pos + 1):ncol(df)]
)

df$`Местоположение чекпоинт` <- ifelse(
  (df$`Местоположение чекпоинт` == "" | is.na(df$`Местоположение чекпоинт`)) & abbr != "",
  abbr,
  df$`Местоположение чекпоинт`
)

df$`Окопан бандаж № Влак` <- num_part

# Попълваме празните редове в "Съотношение пикова/средна сила" със случайни подобни стойности
set.seed(123)
df$`Съотношение пикова/средна сила` <- as.character(df$`Съотношение пикова/средна сила`)
df$`Окопан бандаж № Влак`           <- as.character(df$`Окопан бандаж № Влак`)

pool <- df$`Съотношение пикова/средна сила`
pool <- trimws(pool[!is.na(pool) & pool != ""])

tokens <- unlist(strsplit(pool, ";"))
tokens <- trimws(tokens)
nums   <- suppressWarnings(as.numeric(sub(",", ".", tokens, fixed = TRUE)))
nums   <- nums[!is.na(nums)]

make_one <- function() {
  k        <- sample(1:2, 1)
  base     <- sample(nums, k, replace = TRUE)
  jittered <- round(base * runif(k, 0.95, 1.05), 3)
  paste(format(jittered, decimal.mark = ","), collapse = "; ")
}

empty_ratio <- is.na(df$`Съотношение пикова/средна сила`) |
  trimws(df$`Съотношение пикова/средна сила`) == ""
has_band <- !is.na(df$`Окопан бандаж № Влак`) &
  trimws(df$`Окопан бандаж № Влак`) != ""

idx <- which(empty_ratio & has_band)

if (length(idx) > 0 && length(nums) > 0) {
  df$`Съотношение пикова/средна сила`[idx] <- vapply(idx, function(i) make_one(), character(1))
}

col <- as.character(df$`Съотношение пикова/средна сила`)

tokens_all <- unlist(strsplit(col[!is.na(col)], ";"))
tokens_all <- trimws(tokens_all)
nums_all   <- suppressWarnings(as.numeric(sub(",", ".", tokens_all, fixed = TRUE)))
nums_all   <- nums_all[!is.na(nums_all)]

mean_ref <- mean(nums_all[nums_all >= 5], na.rm = TRUE)

fmt_cell <- function(x) {
  if (is.na(x) || trimws(x) == "") {  # ако е празно → генерираме случайно число в диапазона [5, 8]
    vv <- runif(1, 5, 8)
    return(format(round(vv, 2), decimal.mark = ",", nsmall = 2, trim = TRUE))
  }
  
  parts <- trimws(unlist(strsplit(x, ";")))
  nums <- suppressWarnings(as.numeric(sub(",", ".", parts, fixed = TRUE)))
  
  nums_fixed <- pmin(pmax(nums, 5), 8)  # ограничаваме в диапазон [5, 8]
  
  out <- vapply(nums_fixed, function(v) {
    vv <- round(v, 2)
    format(vv, decimal.mark = ",", nsmall = 2, trim = TRUE)
  }, character(1))
  
  out <- out[out != ""]
  if (length(out) == 0) return(NA_character_)
  paste(out, collapse = "; ")
}

fmt_cell <- function(x) {
  if (is.na(x) || trimws(x) == "") {
    vv <- runif(1, 5, 8)
    return(format(round(vv, 2), decimal.mark = ",", nsmall = 2, trim = TRUE))
  }
  
  parts <- trimws(unlist(strsplit(x, ";")))
  nums <- suppressWarnings(as.numeric(sub(",", ".", parts, fixed = TRUE)))
  
  nums_fixed <- sapply(nums, function(v) {
    if (is.na(v) || v < 5 || v > 8) {
      runif(1, 5, 8)
    } else {
      jittered <- v + runif(1, -0.05, 0.05)
      min(max(jittered, 5), 8)
    }
  })

  out <- format(round(nums_fixed, 2), decimal.mark = ",", nsmall = 2, trim = TRUE)
  paste(out, collapse = "; ")
}

df$`Съотношение пикова/средна сила` <- vapply(
  as.character(df$`Съотношение пикова/средна сила`),
  fmt_cell,
  character(1)
)

df$`Съотношение пикова/средна сила` <- vapply(col, fmt_cell, character(1))

df$`Съотношение пикова/средна сила` <- ifelse(
  is.na(df$`Окопан бандаж № Влак`) | trimws(df$`Окопан бандаж № Влак`) == "",
  NA,
  df$`Съотношение пикова/средна сила`
)


# Попълване на "Тип ПЖПС" спрямо "№ ПЖПС"
if (!"Тип ПЖПС" %in% names(df)) df$`Тип ПЖПС` <- NA_character_
to_fill <- is.na(df$`Тип ПЖПС`) | trimws(df$`Тип ПЖПС`) == ""
tip <- df$`Тип ПЖПС`

n_pp <- as.character(df$`№ ПЖПС`)
n_pp_norm <- trimws(n_pp)

# --- Мап за числови кодове (с тире) ---
num_map <- c(
  "43"="Електрически","44"="Електрически","45"="Електрически","46"="Електрически",
  "80"="Електрически","81"="Електрически","40"="Електрически","86"="Електрически",
  "92"="Лаборатория", "85"="Електрически","87"="Електрически","47"="Електрически",
  "55"="Дизелов","52"="Дизелов","06"="Дизелов","07"="Дизелов", "64"="Електрически",
  "05"="Парен локомотив", "30"="Електрически",# според последното уточнение
  "18"="Парен локомотив", "25"="Парен локомотив", 
  "1116"="Електрически"
)

# --- Мап за буквени кодове (с интервал) ---
char_map <- c(
  "ДМВ"="Дизелов мотрисен влак",
  "ЕМВ"="Електрически мотрисен влак",
  "МВ" ="Мотрисен влак",
  "USP"="Подложна машина",
  "СМ" ="Самоходна машина"
)

# --- Логика за числови кодове NN-xxxx ---
has_dash <- grepl("^\\d{2,4}-", n_pp_norm)  # числов код + тире
prefix_num <- sub("^([0-9]{2,4}).*$", "\\1", n_pp_norm)
prefix_num[!grepl("^[0-9]{2,4}$", prefix_num)] <- ""

idx_num <- which(to_fill & has_dash & prefix_num %in% names(num_map))
tip[idx_num] <- num_map[prefix_num[idx_num]]

# --- Логика за буквени кодове (CODE xx) ---
prefix_char <- sub("^([A-Za-zА-Яа-я]+)\\s.*$", "\\1", n_pp_norm)
prefix_char[!grepl("^[A-Za-zА-Яа-я]+$", prefix_char)] <- ""

idx_char <- which(to_fill & prefix_char %in% names(char_map))
tip[idx_char] <- char_map[prefix_char[idx_char]]

df$`Тип ПЖПС` <- tip


# Попълване на "Тип ПЖПС" на база № ПЖПС (12-цифрен EVN)
if (!"Тип ПЖПС" %in% names(df)) df$`Тип ПЖПС` <- NA_character_

nums <- as.character(df$`№ ПЖПС`)
is_evn <- grepl("^[0-9]{12}$", nums)
type_code <- ifelse(is_evn, substr(nums, 1, 2), NA)

# Функция за категоризация по UIC код
map_vid <- function(tc) {
  if (is.na(tc)) return(NA_character_)
  x <- as.integer(tc)
  if (x >= 50 && x <= 79) return("Пътнически")                  # 50–79 → пътнически
  if ((x >= 1 && x <= 49) || (x >= 80 && x <= 89)) return("Товарен") # 01–49 или 80–89 → товарен
  if (x >= 90 && x <= 99) return("Самоподвижна единица")         # 90–99 → Самоподвижна единица
  return(NA_character_)
}

df$`Тип ПЖПС`[is_evn] <- vapply(type_code[is_evn], map_vid, character(1))


# Импутация на "Местоположение чекпоинт" пропорционално на честотата на наличните стойности
set.seed(123)

vals <- na.omit(trimws(df$`Местоположение чекпоинт`))
vals <- vals[vals != ""]

if (length(vals) > 0) {
  probs <- table(vals) / length(vals)
  idx_empty <- which(is.na(df$`Местоположение чекпоинт`) | trimws(df$`Местоположение чекпоинт`) == "")
  
  if (length(idx_empty) > 0) {
    df$`Местоположение чекпоинт`[idx_empty] <- sample(names(probs), length(idx_empty), replace = TRUE, prob = probs)
  }
}

# Нормализация и уеднаквяване на стойностите в колоната „Местоположение чекпоинт"

df$`Местоположение чекпоинт` <- trimws(df$`Местоположение чекпоинт`)

df$`Местоположение чекпоинт` <- case_when(
  # Стамболийски
  df$`Местоположение чекпоинт` %in% c("Пост 2 гара Стамболийски","См","СТм","стм","Стм-") ~ "Пост 2 гара Стамболийски",
  
  # Тодор Каблешков
  df$`Местоположение чекпоинт` %in% c("Пост 2 гара Тодор Каблешков","Tкл","ТКл","Ткл","Ткл-","Тkл","ткл -") ~ "Пост 2 гара Тодор Каблешков",
  
  # Пазарджик
  df$`Местоположение чекпоинт` %in% c("Пост 1 гара Пазарджик","Пзк","Пзк46-007","P9M7K4 Сп-Пзк") ~ "Пост 1 гара Пазарджик",
  
  # Септември
  df$`Местоположение чекпоинт` %in% c("Пост 1 гара Септември","СП","СпЕМВ","Сп-") ~ "Пост 1 гара Септември",
  
  TRUE ~ df$`Местоположение чекпоинт`
)

# Замяна на "неясно показание" в № ПЖПС на база Тип ПЖПС
set.seed(123)

valid_vag <- df %>%
  filter(`Вид ПЖПС` == "Вагон" & grepl("^[0-9]{12}$", `№ ПЖПС`)) %>%
  mutate(code = substr(`№ ПЖПС`, 1, 2),
         тип_възможен = case_when(
           as.integer(code) >= 50 & as.integer(code) <= 79 ~ "Пътнически",
           (as.integer(code) >= 1 & as.integer(code) <= 49) |
             (as.integer(code) >= 80 & as.integer(code) <= 89) ~ "Товарен",
           TRUE ~ NA_character_
         )) %>%
  filter(!is.na(тип_възможен))

# Определяме пропорциите между пътнически и товарни вагони
share_pax <- mean(valid_vag$тип_възможен == "Пътнически", na.rm = TRUE)
share_freight <- 1 - share_pax

# Попълваме липсващия тип за "неясно показание"
df <- df %>%
  mutate(`Тип ПЖПС` = ifelse(
    trimws(`№ ПЖПС`) == "неясно показание" & (is.na(`Тип ПЖПС`) | `Тип ПЖПС` == ""),
    sample(c("Пътнически", "Товарен"), size = n(), replace = TRUE,
           prob = c(share_pax, share_freight))[seq_len(n())],
    `Тип ПЖПС`
  ))

bad_rows <- which(trimws(df$`№ ПЖПС`) == "неясно показание")

for (i in bad_rows) {
  tip <- df$`Тип ПЖПС`[i]
  
  candidates <- df$`№ ПЖПС`[
    df$`Тип ПЖПС` == tip &
      df$`Вид ПЖПС` == "Вагон" &
      df$`№ ПЖПС` != "неясно показание"
  ]
  
  if (length(candidates) > 0) {
    df$`№ ПЖПС`[i] <- sample(candidates, 1)
  }
}

saveRDS(df, "df_clean.rds")



# =============================================================================
# Етап 2: Идентифициране и филтриране на наблюдения с алармени събития                                
# =============================================================================

get_alarm_data <- function(df) {     # Филтрира само редовете с поне една аларма
  alarm_cols <- c("Горен габарит № Влак",
                  "Страничен габарит № Влак",
                  "Надтоварена колоос № Влак",
                  "Осно натоварване/ t.",
                  "Окопан бандаж № Влак",
                  "Съотношение пикова/средна сила",
                  "Загрята букса/ °C № Влак")
  
  df %>%
    mutate(има_аларма = if_any(all_of(alarm_cols),
                               ~ !is.na(.) & trimws(.) != "" & !. %in% c("0", "-", "--"))) %>%
    filter(има_аларма)
}

# Нормализира имената на типове ПЖПС
normalize_type <- function(df) {
  df %>%
    mutate(Тип_пълен = recode(`Тип ПЖПС`,
                              "Пътнически" = "Пътнически вагон",
                              "Товарен" = "Товарен вагон",
                              "Електрически" = "Електрически локомотив",
                              "Дизелов" = "Дизелов локомотив",
                              "Парен локомотив" = "Парен локомотив",
                              "Лаборатория" = "Локомотив лаборатория",
                              "Електрически мотрисен влак" = "Електрически мотрисен влак",
                              "Дизелов мотрисен влак" = "Дизелов мотрисен влак",
                              "Мотрисен влак" = "Мотрисен влак",
                              "Самоходна машина" = "Самоходна машина",
                              "Подложна машина" = "Подложна машина",
                              "Самоподвижна единица" = "Самоподвижна единица",
                              .default = `Тип ПЖПС`))
}

# 1. Визуализация на регистрираните аларми по Вид ПЖПС
counts <- as.data.frame(table(df$`Вид ПЖПС`))

counts$Var1 <- recode(counts$Var1,
                      "Вагон" = "Вагони",
                      "Локомотив" = "Локомотиви")

ggplot(counts, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_col(show.legend = FALSE, width = 0.6) +
  geom_text(aes(label = Freq), vjust = -0.5, size = 5) +
  labs(
    title = "Брой регистрирани подвижен жп състав по вид за периода 01.03.2018 - 28.02.2022",
    x = NULL, y = "Брой"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  )


# 2. Визуализация на броя на регистрираните аларми по показатели
cols <- c("Горен габарит № Влак",
          "Страничен габарит № Влак",
          "Надтоварена колоос № Влак",
          "Окопан бандаж № Влак",
          "Загрята букса/ °C № Влак")

counts_indicators <- df %>%
  select(all_of(cols)) %>%
  pivot_longer(cols = everything(), names_to = "Показател", values_to = "Стойност") %>%
  filter(!is.na(Стойност) & trimws(Стойност) != "") %>%
  group_by(Показател) %>%
  summarise(Брой = n(), .groups = "drop")

labels_clean <- c("Горен габарит",
                  "Страничен габарит",
                  "Надтоварена колоос",
                  "Окопан бандаж",
                  "Загрята букса")

counts_indicators$Показател <- factor(counts_indicators$Показател,
                                      levels = cols,
                                      labels = labels_clean)

ggplot(counts_indicators, aes(x = reorder(Показател, -Брой), y = Брой, fill = Показател)) +
  geom_col(show.legend = FALSE, width = 0.6) +
  geom_text(aes(label = Брой), vjust = -0.5, size = 5) +
  labs(
    title = "Брой регистрирани аларми по вид за периода 01.03.2018 - 28.02.2022",
    x = NULL,
    y = "Брой"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 12, angle = 20, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  )


# 3. Визуализация на броя на регистрираните аларми по Тип ПЖПС
df_alarm <- get_alarm_data(df) %>% normalize_type()

counts_type_full <- as.data.frame(table(df_alarm$Тип_пълен))

ggplot(counts_type_full, aes(x = reorder(Var1, -Freq), y = Freq, fill = Var1)) +
  geom_col(show.legend = FALSE, width = 0.6) +
  geom_text(aes(label = Freq), vjust = -0.5, size = 4) +
  labs(
    title = "Брой регистрирани аларми по тип ПЖПС за периода 01.03.2018 - 28.02.2022",
    x = NULL,
    y = "Брой"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 11, angle = 20, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  )


# 4. Визуализация на броя на регистрираните аларми по Тип ПЖПС и отчетни периоди
df_alarm <- get_alarm_data(df) %>%
  normalize_type() %>%
  mutate(Отчетен_период = case_when(
    Дата >= as.Date("2018-03-01") & Дата <= as.Date("2019-02-28") ~ "01.03.2018 - 28.02.2019",
    Дата >= as.Date("2019-03-01") & Дата <= as.Date("2020-02-29") ~ "01.03.2019 - 29.02.2020",
    Дата >= as.Date("2020-03-01") & Дата <= as.Date("2021-02-28") ~ "01.03.2020 - 28.02.2021",
    Дата >= as.Date("2021-03-01") & Дата <= as.Date("2022-02-28") ~ "01.03.2021 - 28.02.2022",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Отчетен_период))

counts_type_period <- df_alarm %>%  # преброяване по тип и отчетен период
  group_by(Отчетен_период, Тип_пълен) %>%
  summarise(Брой = n(), .groups = "drop")

totals <- counts_type_period %>%  # общ брой по отчетен период
  group_by(Отчетен_период) %>%
  summarise(Общо = sum(Брой), .groups = "drop")

ggplot(counts_type_period, aes(x = Отчетен_период, y = Брой, fill = Тип_пълен)) +
  geom_col(position = "stack") +
  geom_text(data = totals,
            aes(x = Отчетен_период, y = Общо, label = Общо),
            inherit.aes = FALSE,
            vjust = -0.5, size = 4) +
  labs(
    title = "Брой регистрирани аларми по тип ПЖПС и отчетни периоди",
    x = "Отчетен период",
    y = "Брой",
    fill = "Тип ПЖПС"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 10, angle = 20, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  )


# 5. Визуализация на броя на регистрираните аларми по ЖП превозвач
df_alarm <- get_alarm_data(df)

counts_carrier <- as.data.frame(table(df_alarm$`ЖП превозвач`))

ggplot(counts_carrier, aes(x = reorder(Var1, -Freq), y = Freq, fill = Var1)) +
  geom_col(show.legend = FALSE, width = 0.6) +
  geom_text(aes(label = Freq), vjust = -0.5, size = 4) +
  labs(
    title = "Брой регистрирани аларми по ЖП превозвач",
    x = "ЖП превозвач",
    y = "Брой"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 11, angle = 20, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  )


# 6. Визуализация на броя на регистрираните аларми по Местоположение чекпоинт
df_alarm <- get_alarm_data(df)

counts_checkpoint <- as.data.frame(table(df_alarm$`Местоположение чекпоинт`))

ggplot(counts_checkpoint, aes(x = reorder(Var1, -Freq), y = Freq, fill = Var1)) +
  geom_col(show.legend = FALSE, width = 0.6) +
  geom_text(aes(label = Freq), vjust = -0.5, size = 4) +
  labs(
    title = "Брой регистрирани аларми по местоположение на чекпоинт",
    x = "Местоположение чекпоинт",
    y = "Брой"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 11, angle = 20, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  )


# 7. Визуализация на броя пътнически и товарни влакове на база показателите
check_cols <- c("Горен габарит № Влак",
                "Страничен габарит № Влак",
                "Надтоварена колоос № Влак",
                "Окопан бандаж № Влак",
                "Загрята букса/ °C № Влак")

df_alarm <- get_alarm_data(df)   # само редове с аларма

df_numbers <- df_alarm %>%
  select(Дата, all_of(check_cols)) %>%
  tidyr::pivot_longer(cols = -Дата, names_to = "Показател", values_to = "Стойност") %>%
  filter(!is.na(Стойност) & grepl("^[0-9]+$", Стойност)) %>%
  mutate(
    цифри = nchar(Стойност),
    трета_цифра = ifelse(цифри == 5, substr(Стойност, 3, 3), NA),  # взимаме 3-тата цифра
    Тип = case_when(
      цифри == 3 ~ "Международен пътнически влак",
      цифри == 4 ~ "Бърз пътнически влак",
      цифри == 5 & трета_цифра %in% c("0","1","2","3") ~ "Пътнически влак",
      цифри == 5 & трета_цифра %in% c("4","5","6","7","8","9") ~ "Товарен влак",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Тип))

df_unique <- df_numbers %>%
  distinct(Дата, Стойност, Тип)

counts_train_type <- df_unique %>%
  group_by(Тип) %>%
  summarise(Брой = n(), .groups = "drop")

ggplot(counts_train_type, aes(x = Тип, y = Брой, fill = Тип)) +
  geom_col(show.legend = FALSE, width = 0.6) +
  geom_text(aes(label = Брой), vjust = -0.5, size = 5) +
  labs(
    title = "Брой регистрирани влакове по тип",
    x = NULL,
    y = "Брой"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  )


# 8. Визуализация на броя пътнически и товарни влакове по отчетни периоди
df_by_period <- df_unique %>%
  mutate(Отчетен_период = case_when(
    Дата >= as.Date("2018-03-01") & Дата <= as.Date("2019-02-28") ~ "01.03.2018 - 28.02.2019",
    Дата >= as.Date("2019-03-01") & Дата <= as.Date("2020-02-29") ~ "01.03.2019 - 29.02.2020",
    Дата >= as.Date("2020-03-01") & Дата <= as.Date("2021-02-28") ~ "01.03.2020 - 28.02.2021",
    Дата >= as.Date("2021-03-01") & Дата <= as.Date("2022-02-28") ~ "01.03.2021 - 28.02.2022",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Отчетен_период)) %>%
  group_by(Отчетен_период, Тип) %>%
  summarise(Брой = n(), .groups = "drop")

# Визуализация – групирани колони по отчетни периоди + числа върху тях
ggplot(df_by_period, aes(x = Отчетен_период, y = Брой, fill = Тип)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = Брой),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 4) +
  labs(
    title = "Брой регистрирани влакове по тип и отчетни периоди",
    x = "Отчетен период",
    y = "Брой",
    fill = "Тип влак"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 10, angle = 20, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  )


# 9. Визуализация на броя регистрирани случаи по главни жп линии
# mapping: номер на линия -> име
line_names <- c(
  "1" = "Калотина-запад – София \n– Пловдив – Свиленград",
  "2" = "София – Горна Оряховица \n– Варна",
  "3" = "Илиянци – Карлово \n– Варна-фериботна",
  "4" = "Русе – Горна Оряховица \n– Стара Загора – Подкова",
  "5" = "София – Кулата",
  "6" = "Волуяк – Гюешево",
  "7" = "Мездра – Бойчиновци \n– Брусарци – Видин",
  "8" = "Пловдив – Стара Загора \n– Бургас",
  "9" = "Русе – Самуил – Каспичан"
)

# извличаме първата цифра от № ПЖПС като номер на линия
df_lines <- df %>%
  filter(!is.na(`№ ПЖПС`) & grepl("^[0-9]", `№ ПЖПС`)) %>%
  mutate(
    линия = substr(`№ ПЖПС`, 1, 1)
  ) %>%
  filter(линия %in% as.character(1:9))

counts_lines <- df_lines %>%
  group_by(линия) %>%
  summarise(Брой = n(), .groups = "drop") %>%
  mutate(Етикет = paste0(линия, " – ", line_names[линия]))

ggplot(counts_lines, aes(x = Етикет, y = Брой, fill = линия)) +
  geom_col(show.legend = FALSE, width = 0.6) +
  geom_text(aes(label = Брой), vjust = -0.5, size = 4) +
  labs(
    title = "Брой регистрирани случаи по главни жп линии",
    x = "Главна линия",
    y = "Брой"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 10, angle = 20, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  )


# 10. Дял на влаковете с регистрирани аларми по отчетни периоди
df_totals <- tribble(
  ~Период,                    ~Общо,  ~Пътнически, ~Товарни,
  "01.03.2018 - 28.02.2019", 24879, 12410, 12469,
  "01.03.2019 - 29.02.2020", 23875, 12444, 11431,
  "01.03.2020 - 28.02.2021", 24567, 12410, 12157,
  "01.03.2021 - 28.02.2022", 25475, 12410, 13065
)

df_alarms <- df_unique %>%
  mutate(Период = case_when(
    Дата >= as.Date("2018-03-01") & Дата <= as.Date("2019-02-28") ~ "01.03.2018 - 28.02.2019",
    Дата >= as.Date("2019-03-01") & Дата <= as.Date("2020-02-29") ~ "01.03.2019 - 29.02.2020",
    Дата >= as.Date("2020-03-01") & Дата <= as.Date("2021-02-28") ~ "01.03.2020 - 28.02.2021",
    Дата >= as.Date("2021-03-01") & Дата <= as.Date("2022-02-28") ~ "01.03.2021 - 28.02.2022",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Период)) %>%
  group_by(Период, Тип) %>%
  summarise(Брой_аларми = n(), .groups = "drop")

df_rates <- df_alarms %>%
  left_join(df_totals, by = "Период") %>%
  mutate(
    Общо_в_тип = if_else(Тип == "Пътнически влак", Пътнически, Товарни),
    Процент = 100 * Брой_аларми / Общо_в_тип
  )

ggplot(df_rates, aes(x = Период, y = Процент, fill = Тип)) +
  geom_col(position = "dodge", width = 0.6) +
  geom_text(aes(label = paste0(round(Процент, 2), "%")),
            position = position_dodge(width = 0.6),
            vjust = -0.5, size = 4) +
  labs(
    title = "Дял на влаковете с регистрирани аларми по отчетни периоди",
    x = "Отчетен период",
    y = "Процент от преминалите влакове",
    fill = "Тип влак"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 10, angle = 20, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  )


# 11. Стойности в „Осно натоварване/ t.“ по категории: Предупреждение, Аларма – изходен и Аларма – входен.
df_weight_clean <- df %>%
  select(`Осно натоварване/ t.`) %>%
  filter(!is.na(`Осно натоварване/ t.`) & trimws(`Осно натоварване/ t.`) != "") %>%
  separate_rows(`Осно натоварване/ t.`, sep = ";") %>%
  mutate(`Осно натоварване/ t.` = as.numeric(gsub(",", ".", trimws(`Осно натоварване/ t.`))))

df_weight_clean <- df_weight_clean %>%
  mutate(Категория = case_when(
    `Осно натоварване/ t.` >= 22.5 & `Осно натоварване/ t.` < 23.3 ~ "Предупреждение",
    `Осно натоварване/ t.` >= 23.3 & `Осно натоварване/ t.` < 24.2 ~ "Аларма - затваряща\nизходния светофор",
    `Осно натоварване/ t.` >= 24.2                                ~ "Аларма - затваряща\nвходния светофор",
    TRUE ~ "Извън диапазона"
  ))

counts_weight <- df_weight_clean %>%
  group_by(Категория) %>%
  summarise(Брой = n(), .groups = "drop")

counts_weight <- bind_rows(
  counts_weight,
  tibble(Категория = "Общо регистрирани аларми", Брой = sum(counts_weight$Брой))
)

counts_weight$Категория <- factor(
  counts_weight$Категория,
  levels = c("Предупреждение",
             "Аларма - затваряща\nизходния светофор",
             "Аларма - затваряща\nвходния светофор",
             "Извън диапазона",
             "Общо регистрирани аларми")
)

ggplot(counts_weight, aes(x = Категория, y = Брой, fill = Категория)) +
  geom_col(show.legend = FALSE, width = 0.6) +
  geom_text(aes(label = Брой), vjust = -0.5, size = 4) +
  labs(
    title = "Разпределение на регистрираните стойности по \"Осно натоварване/ t.\"",
    x = "Категория",
    y = "Брой"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 11, angle = 0, hjust = 0.5),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  )


# 12. Стойности в „Съотношение пикова/средна сила“ по категории: Предупреждение и Аларма – изходен.
df_ratio_clean <- df %>%
  select(`Съотношение пикова/средна сила`) %>%
  filter(!is.na(`Съотношение пикова/средна сила`) & trimws(`Съотношение пикова/средна сила`) != "") %>%
  separate_rows(`Съотношение пикова/средна сила`, sep = ";") %>%
  mutate(`Съотношение пикова/средна сила` = as.numeric(gsub(",", ".", trimws(`Съотношение пикова/средна сила`))))

df_ratio_clean <- df_ratio_clean %>%
  mutate(Категория = case_when(
    `Съотношение пикова/средна сила` >= 5 & `Съотношение пикова/средна сила` < 6 ~ "Предупреждение",
    `Съотношение пикова/средна сила` >= 6                                       ~ "Аларма - затваряща\nизходния светофор",
    TRUE ~ "Извън диапазона"
  ))

counts_ratio <- df_ratio_clean %>%
  group_by(Категория) %>%
  summarise(Брой = n(), .groups = "drop")

counts_ratio <- bind_rows(
  counts_ratio,
  tibble(Категория = "Общо регистрирани аларми", Брой = sum(counts_ratio$Брой))
)

counts_ratio$Категория <- factor(
  counts_ratio$Категория,
  levels = c("Предупреждение",
             "Аларма - затваряща\nизходния светофор",
             "Извън диапазона",
             "Общо регистрирани аларми")
)

ggplot(counts_ratio, aes(x = Категория, y = Брой, fill = Категория)) +
  geom_col(show.legend = FALSE, width = 0.6) +
  geom_text(aes(label = Брой), vjust = -0.5, size = 4) +
  labs(
    title = "Разпределение на регистрираните стойности по \"Съотношение пикова/средна сила\"",
    x = "Категория",
    y = "Брой"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 11, angle = 0, hjust = 0.5),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  )



# =============================================================================
# Етап 3: Актуализация и обогатяване на набора с допълнителни характеристики                             
# =============================================================================

df <- df %>%
  select(-`Осно натоварване/ t.`, -`Съотношение пикова/средна сила`)

cols_to_convert <- c(
  "Горен габарит № Влак",
  "Страничен габарит № Влак",
  "Надтоварена колоос № Влак",
  "Окопан бандаж № Влак",
  "Загрята букса/ °C № Влак"
)

df[cols_to_convert] <- lapply(df[cols_to_convert], function(x) {
  ifelse(!is.na(x) & trimws(x) != "", 1, 0)
})

df <- df %>%
  mutate(`Година на производство` = NA_integer_) %>%  # създаваме празна колона
  relocate(`Година на производство`, .after = `Тип ПЖПС`)  # местим след "Тип ПЖПС"

# Попълване на "Година на производство" за локомотивите
source(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), "loc_years.R"))

df <- df %>%
  left_join(loc_years, by = "№ ПЖПС", suffix = c("", "_лист")) %>%
  mutate(`Година на производство` = ifelse(`Вид ПЖПС` == "Локомотив",
                                           `Година на производство_лист`,
                                           `Година на производство`)) %>%
  select(-`Година на производство_лист`)

df <- df %>%
  mutate(`№ ПЖПС` = trimws(gsub("-+$", "", `№ ПЖПС`)))

df <- df %>%
  left_join(loc_years, by = "№ ПЖПС", suffix = c("", "_лист")) %>%
  mutate(`Година на производство` = ifelse(`Вид ПЖПС` == "Локомотив",
                                           `Година на производство_лист`,
                                           `Година на производство`)) %>%
  select(-`Година на производство_лист`)

check_summary <- df %>%
  filter(`Вид ПЖПС` == "Локомотив") %>%
  summarise(
    Общо_локомотиви = n(),
    Без_година = sum(is.na(`Година на производство`)),
    Процент_покрити = round(100 * (1 - sum(is.na(`Година на производство`)) / n()), 2)
  )

print(check_summary)

missing_loks <- df %>%
  filter(`Вид ПЖПС` == "Локомотив" & is.na(`Година на производство`)) %>%
  select(`№ ПЖПС`, `Тип ПЖПС`) %>%
  arrange(`№ ПЖПС`)

print(missing_loks)


# Попълване на "Година на производство" за вагоните
source(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), "vag_years.R"))

df <- df %>%
  left_join(vag_years, by = "№ ПЖПС", suffix = c("", "_лист")) %>%
  mutate(`Година на производство` = ifelse(`Вид ПЖПС` == "Вагон",
                                           `Година на производство_лист`,
                                           `Година на производство`)) %>%
  select(-`Година на производство_лист`)

df <- df %>%
  mutate(`№ ПЖПС` = trimws(gsub("-+$", "", `№ ПЖПС`)))

df <- df %>%
  left_join(vag_years, by = "№ ПЖПС", suffix = c("", "_лист")) %>%
  mutate(`Година на производство` = ifelse(`Вид ПЖПС` == "Вагон",
                                           `Година на производство_лист`,
                                           `Година на производство`)) %>%
  select(-`Година на производство_лист`)

check_summary_vag <- df %>%
  filter(`Вид ПЖПС` == "Вагон") %>%
  summarise(
    Общо_вагони = n(),
    Без_година = sum(is.na(`Година на производство`)),
    Процент_покрити = round(100 * (1 - sum(is.na(`Година на производство`)) / n()), 2)
  )

print(check_summary_vag)

missing_vag <- df %>%
  filter(`Вид ПЖПС` == "Вагон" & is.na(`Година на производство`)) %>%
  select(`№ ПЖПС`, `Тип ПЖПС`) %>%
  arrange(`№ ПЖПС`)

print(missing_vag)


# Попълване на "Вид товар" според типа и вида на ПЖПС
df <- df %>%
  mutate(`Вид товар` = NA_character_) %>%
  relocate(`Вид товар`, .after = `Година на производство`)

df <- df %>%
  mutate(`Вид товар` = case_when(
    # Локомотиви – мотрисни влакове
    `Вид ПЖПС` == "Локомотив" & `Тип ПЖПС` %in% 
      c("Дизелов мотрисен влак", "Електрически мотрисен влак", "Мотрисен влак") ~ "Пътници",
    `Вид ПЖПС` == "Локомотив" ~ "Собствено тегло",
    `Вид ПЖПС` == "Вагон" & `Тип ПЖПС` == "Пътнически" ~ "Пътници",
    `Вид ПЖПС` == "Вагон" & `Тип ПЖПС` == "Самоподвижна единица" ~ "Собствено тегло",
    `ЖП превозвач` == "B5ZP8P3" & `Вид ПЖПС` == "Вагон" ~ "Пътници",
    
    TRUE ~ NA_character_
  ))


# Попълване на "Вид товар" за товарните вагони според списъка с вагони и товари
source(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), "loc_goods.R"))

df <- df %>%
  left_join(loc_goods, by = "№ ПЖПС", suffix = c("", "_лист")) %>%
  mutate(`Вид товар` = case_when(
    `Вид ПЖПС` == "Вагон" & `Тип ПЖПС` == "Товарен" ~ coalesce(`Вид товар_лист`, `Вид товар`),
    TRUE ~ `Вид товар`
  )) %>%
  select(-`Вид товар_лист`)

df <- df %>%
  mutate(`Вид товар` = case_when(
    `Вид ПЖПС` == "Вагон" & `Тип ПЖПС` == "Товарен" &
      (is.na(`Вид товар`) | trimws(`Вид товар`) == "") ~ "Стомана",
    TRUE ~ `Вид товар`
  ))


# Проверка за празни или NA стойности в основните колони
cols_to_check <- c(
  "Дата",
  "ЖП превозвач",
  "Вид ПЖПС",
  "№ ПЖПС",
  "Тип ПЖПС",
  "Година на производство",
  "Вид товар",
  "Местоположение чекпоинт"
)

rows_with_missing <- df %>%
  filter(if_any(all_of(cols_to_check), ~ is.na(.) | trimws(as.character(.)) == ""))

if (nrow(rows_with_missing) == 0) {
  message("Няма липсващи стойности в колоните с аларми!")
} else {
  warning(paste("Открити са", nrow(rows_with_missing), 
                "ред(а) с липсващи стойности в основните колони!"))
  print(rows_with_missing)
}

cols_binary <- c(
  "Горен габарит № Влак",
  "Страничен габарит № Влак",
  "Надтоварена колоос № Влак",
  "Окопан бандаж № Влак",
  "Загрята букса/ °C № Влак"
)

rows_invalid <- df %>%
  filter(if_any(all_of(cols_binary), 
                ~ is.na(.) | trimws(as.character(.)) == "" | !. %in% c(0, 1)))

if (nrow(rows_invalid) == 0) {
  message("Няма липсващи стойности в основните колони!")
} else {
  warning(paste("Открити са", nrow(rows_invalid), 
                "ред(а) с празни или невалидни стойности!"))
  print(rows_invalid)
}


# Добавяне на колоната "Скорост km/h" от списъка pjps_speed
source(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), "pjps_speed.R"))

# Добавяме празна колона "Скорост km/h" след "Вид товар"
df <- df %>%
  mutate(`Скорост km/h` = NA_real_) %>%
  relocate(`Скорост km/h`, .after = `Вид товар`)

pjps_speed <- pjps_speed %>%
  distinct(`№ ПЖПС`, .keep_all = TRUE)

df <- df %>%
  left_join(pjps_speed, by = "№ ПЖПС") %>%
  mutate(`Скорост km/h` = coalesce(`Скорост`, `Скорост km/h`)) %>%
  select(-`Скорост`)

missing_speed_check <- df %>%
  filter(is.na(`Скорост km/h`) | trimws(as.character(`Скорост km/h`)) == "") %>%
  select(`№ ПЖПС`, `Вид ПЖПС`, `Тип ПЖПС`, `Вид товар`, `Скорост km/h`) %>%
  arrange(`№ ПЖПС`)

if (nrow(missing_speed_check) == 0) {
  cat("Няма липсващи стойности в колона 'Скорост km/h'.\n")
} else {
  print(missing_speed_check)
}

# write_xlsx(df, file.path(dirname(rstudioapi::getActiveDocumentContext()$path), "df_final.xlsx"))



# =============================================================================
# Етап 4: Моделиране и оценка на точността на класификаторите
# =============================================================================

# Добавяме отчетните периоди по дати
df <- df %>%
  mutate(Дата = as.Date(Дата, format = "%m/%d/%Y")) %>%
  mutate(Отчетен_период = case_when(
    Дата >= as.Date("2018-03-01") & Дата <= as.Date("2019-02-28") ~ "2018-2019",
    Дата >= as.Date("2019-03-01") & Дата <= as.Date("2020-02-29") ~ "2019-2020",
    Дата >= as.Date("2020-03-01") & Дата <= as.Date("2021-02-28") ~ "2020-2021",
    Дата >= as.Date("2021-03-01") & Дата <= as.Date("2022-02-28") ~ "2021-2022",
    TRUE ~ NA_character_
  ))

table(df$Отчетен_период, useNA = "ifany")

# Определяме кои са алармите
alarms <- c(
  "Горен габарит № Влак",
  "Страничен габарит № Влак",
  "Надтоварена колоос № Влак",
  "Окопан бандаж № Влак",
  "Загрята букса/ °C № Влак"
)

# Агрегиране по № ПЖПС и отчетен период
df_period <- df %>%
  group_by(`№ ПЖПС`, Отчетен_период) %>%
  summarise(across(all_of(alarms),
                   ~ as.integer(any(.x == 1, na.rm = TRUE)),
                   .names = "{.col}_има_повреда"),
            .groups = "drop")

# Добавяне на лагове (t–1, t–2, t–3) по всеки ПЖПС
df_period <- df_period %>%
  arrange(`№ ПЖПС`, Отчетен_период) %>%
  group_by(`№ ПЖПС`) %>%
  mutate(across(ends_with("_има_повреда"),
                list(t1 = ~lag(.x, 1),
                     t2 = ~lag(.x, 2),
                     t3 = ~lag(.x, 3)),
                .names = "{.col}_{.fn}")) %>%
  ungroup()

df_period %>%  # Проверка
  filter(`№ ПЖПС` %in% head(unique(df_period$`№ ПЖПС`), 3)) %>%
  arrange(`№ ПЖПС`, Отчетен_период)


# Създаване на финален df_model (t₀ = 2021–2022)
get_mode <- function(x) { # Функция за най-честа стойност (Mode)
  x <- na.omit(x)
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Извличаме всички ПЖПС, които са активни през 2021–2022
pjeps_t0 <- df %>%
  filter(Отчетен_период == "2021-2022") %>%
  distinct(`№ ПЖПС`) %>%
  pull(`№ ПЖПС`)

# Филтрираме df_period само за 2021–2022 и налични вагони
df_model <- df_period %>%
  filter(Отчетен_период == "2021-2022",
         `№ ПЖПС` %in% pjeps_t0)

# Подготвяме описателните променливи (с Mode и mean)
descr_vars <- c("№ ПЖПС", "ЖП превозвач", "Вид ПЖПС", "Тип ПЖПС",
                "Година на производство", "Вид товар",
                "Местоположение чекпоинт", "Скорост km/h")

df_descr_t0 <- df %>%
  filter(Отчетен_период == "2021-2022") %>%
  group_by(`№ ПЖПС`) %>%
  summarise(
    `ЖП превозвач` = get_mode(`ЖП превозвач`),
    `Вид ПЖПС` = get_mode(`Вид ПЖПС`),
    `Тип ПЖПС` = get_mode(`Тип ПЖПС`),
    `Година на производство` = get_mode(`Година на производство`),
    `Вид товар` = get_mode(`Вид товар`),
    `Местоположение чекпоинт` = get_mode(`Местоположение чекпоинт`),
    `Скорост km/h` = mean(`Скорост km/h`, na.rm = TRUE),
    .groups = "drop"
  )

df_model <- df_model %>%
  left_join(df_descr_t0, by = "№ ПЖПС")

alarm_order <- c()  # Пренареждаме колоните
for (a in alarms) {
  alarm_base <- paste0(a, "_има_повреда")
  alarm_order <- c(alarm_order,
                   alarm_base,
                   paste0(alarm_base, "_t1"),
                   paste0(alarm_base, "_t2"),
                   paste0(alarm_base, "_t3"))
}

final_order <- c(
  "№ ПЖПС", "ЖП превозвач", "Вид ПЖПС", "Тип ПЖПС",
  "Година на производство", "Вид товар", "Местоположение чекпоинт",
  "Скорост km/h", alarm_order, "Отчетен_период"
)

df_model <- df_model[, final_order]

# Преименуване на колоните (t0, t−1, t−2, t−3)
df_model <- df_model %>%
  rename_with(~ str_replace_all(., "_има_повреда$", "_t0")) %>%
  rename_with(~ str_replace_all(., "_има_повреда_t1$", "_t-1")) %>%
  rename_with(~ str_replace_all(., "_има_повреда_t2$", "_t-2")) %>%
  rename_with(~ str_replace_all(., "_има_повреда_t3$", "_t-3"))

cat("Създаден е df_model с Mode: t0, t-1, t-2, t-3\n")
cat("Брой редове:", nrow(df_model), "\n")
cat("Брой колони:", ncol(df_model), "\n\n")

glimpse(df_model)

# write_xlsx(df_model, "df_model_forecast_2021_2022_v3.xlsx")


# Премахване на ПЖПС без история в нито една аларма
alarms <- c(
  "Горен габарит № Влак",
  "Страничен габарит № Влак",
  "Надтоварена колоос № Влак",
  "Окопан бандаж № Влак",
  "Загрята букса/ °C № Влак"
)

# Всички лагови колони (t−1, t−2, t−3)
lag_cols <- unlist(lapply(alarms, function(a) {
  paste0(a, c("_t-1", "_t-2", "_t-3"))
}))

# Премахваме редовете, които НЯМАТ история в НИТО една аларма
df_model <- df_model %>%
  filter(if_any(all_of(lag_cols), ~ !is.na(.x)))

cat("Остават редове:", nrow(df_model), "\n\n")


# Анализ на дисбаланса по всички 5 аларми
alarms <- c(
  "Горен габарит № Влак",
  "Страничен габарит № Влак",
  "Надтоварена колоос № Влак",
  "Окопан бандаж № Влак",
  "Загрята букса/ °C № Влак"
)

# Извличаме данните за t0 от df_model
imbalance_summary <- lapply(alarms, function(a) {
  col <- paste0(a, "_t0")
  
  if (!col %in% colnames(df_model)) {
    return(data.frame(
      Аларма = a,
      `Брой 0` = NA,
      `Брой 1` = NA,
      `% 0` = NA,
      `% 1` = NA
    ))
  }
  
  counts <- table(df_model[[col]], useNA = "ifany")
  zeros <- counts["0"]
  ones  <- counts["1"]
  
  data.frame(
    Аларма = a,
    `Брой 0` = ifelse(is.na(zeros), 0, zeros),
    `Брой 1` = ifelse(is.na(ones), 0, ones),
    `% 0` = round(100 * ifelse(is.na(zeros), 0, zeros) / sum(counts), 1),
    `% 1` = round(100 * ifelse(is.na(ones), 0, ones) / sum(counts), 1)
  )
}) %>% bind_rows()

print(imbalance_summary)


# ============================================================
# ЕТАП 4.1: Logistic Regression model
# ============================================================

alarms_to_model <- c(
  "Страничен габарит № Влак",
  "Надтоварена колоос № Влак",
  "Окопан бандаж № Влак"
)

categorical_vars <- c(
  "ЖП превозвач", "Вид ПЖПС", "Тип ПЖПС",
  "Вид товар", "Местоположение чекпоинт"
)

results <- list()

for (a in alarms_to_model) {
  cat("\n\n=============================================\n")
  cat("🔧 Моделиране за аларма:", a, "\n")
  cat("=============================================\n")
  
  target <- paste0(a, "_t0")
  if (!(target %in% names(df_model))) {
    cat("Пропускаме", a, "- няма такава колона.\n")
    next
  }
  
  # Стратифицирано разделяне на train/test
  strata_vars <- c("ЖП превозвач", "Вид ПЖПС", "Тип ПЖПС")
  df_temp <- df_model %>%
    mutate(across(all_of(strata_vars), ~ifelse(is.na(.x), "Unknown", as.character(.x))))
  df_temp$Strata <- apply(df_temp[, strata_vars], 1, function(x) paste(x, collapse = "_"))
  
  set.seed(123)
  train_index <- createDataPartition(df_temp$Strata, p = 0.8, list = FALSE)
  train_data <- df_temp[train_index, ]
  test_data  <- df_temp[-train_index, ]
  
  # Проверка дали всички категории се срещат и в двете подизвадки
  for (v in strata_vars) {
    missing_in_train <- setdiff(unique(df_temp[[v]]), unique(train_data[[v]]))
    missing_in_test  <- setdiff(unique(df_temp[[v]]), unique(test_data[[v]]))
    if (length(missing_in_train) > 0) {
      cat("Warning", v, "- липсват в TRAIN:", paste(missing_in_train, collapse = ", "), "→ добавяме Unknown\n")
      tmp_rows <- df_temp %>% filter(.data[[v]] %in% missing_in_train)
      train_data <- bind_rows(train_data, tmp_rows %>% mutate(across(all_of(strata_vars), ~ "Unknown")))
    }
    if (length(missing_in_test) > 0) {
      cat("Warning", v, "- липсват в TEST:", paste(missing_in_test, collapse = ", "), "→ добавяме Unknown\n")
      tmp_rows <- df_temp %>% filter(.data[[v]] %in% missing_in_test)
      test_data <- bind_rows(test_data, tmp_rows %>% mutate(across(all_of(strata_vars), ~ "Unknown")))
    }
  }
  
  cat("Train редове:", nrow(train_data), "| Test редове:", nrow(test_data), "\n")
  
  # Проверка за класове
  if (n_distinct(train_data[[target]]) < 2) {
    cat("Пропускаме", a, "- само един клас в train.\n")
    next
  }
  if (n_distinct(test_data[[target]]) < 2) {
    cat("Пропускаме", a, "- само един клас в test.\n")
    next
  }
  
  # Премахване на ненужни и константни колони
  constant_cols <- names(train_data)[sapply(train_data, function(x) n_distinct(x) <= 1)]
  other_targets <- paste0(setdiff(alarms_to_model, a), "_t0")
  remove_cols <- unique(c(constant_cols, "№ ПЖПС", "Отчетен_период", "Strata", other_targets))
  
  if (length(remove_cols) > 0) {
    cat("Премахнати колони:", paste(remove_cols, collapse = ", "), "\n")
    train_data <- select(train_data, -any_of(remove_cols))
    test_data  <- select(test_data, -any_of(remove_cols))
  }
  
  # Подравняване на категориалните нива + Unknown
  for (col in categorical_vars) {
    if (col %in% names(train_data)) train_data[[col]] <- as.factor(train_data[[col]])
    if (col %in% names(test_data)) {
      test_data[[col]] <- as.factor(test_data[[col]])
      unseen_lvls <- setdiff(levels(test_data[[col]]), levels(train_data[[col]]))
      if (length(unseen_lvls) > 0) {
        cat("Замяна на нови нива в", col, ":", paste(unseen_lvls, collapse = ", "), "\n")
        test_data[[col]] <- fct_collapse(test_data[[col]],
                                         !!!setNames(as.list(rep("Unknown", length(unseen_lvls))),
                                                     unseen_lvls))
      }
      all_lvls <- union(levels(train_data[[col]]), levels(test_data[[col]]))
      train_data[[col]] <- factor(train_data[[col]], levels = all_lvls)
      test_data[[col]]  <- factor(test_data[[col]],  levels = all_lvls)
    }
  }
  
  # Финално фиксиране на факторите
  for (col in categorical_vars) {
    if (col %in% names(train_data)) train_data[[col]] <- droplevels(as.factor(train_data[[col]]))
    if (col %in% names(test_data))  test_data[[col]]  <- droplevels(as.factor(test_data[[col]]))
  }
  
  # Почистване от NA (за glmnet)
  train_data <- train_data %>% mutate(across(everything(), ~ifelse(is.na(.x), 0, .x)))
  test_data  <- test_data  %>% mutate(across(everything(), ~ifelse(is.na(.x), 0, .x)))
  
  # Балансиране (дублираме малцинствения клас)
  prop_ones <- mean(train_data[[target]] == 1, na.rm = TRUE)
  cat("Дял на 1:", round(prop_ones * 100, 2), "%\n")
  if (prop_ones < 0.4) {
    ones <- train_data %>% filter(!!sym(target) == 1)
    zeros <- train_data %>% filter(!!sym(target) == 0)
    ratio <- ceiling(nrow(zeros) / max(1, nrow(ones)))
    train_data <- bind_rows(zeros, bind_rows(replicate(ratio, ones, simplify = FALSE)))
    cat("Oversampling: дублирани", ratio, "пъти наблюденията с 1\n")
  }
  
  # Филтър на прекалено корелирани лагове
  num_vars <- names(train_data)[sapply(train_data, is.numeric)]
  too_high_corr <- sapply(num_vars, function(col) {
    if (col != target) {
      cor_val <- suppressWarnings(cor(train_data[[col]], train_data[[target]],
                                      use = "pairwise.complete.obs"))
      if (is.na(cor_val)) return(FALSE)
      return(abs(cor_val) > 0.95)
    } else FALSE
  })
  if (any(too_high_corr, na.rm = TRUE)) {
    cat("Премахваме прекалено корелирани променливи:",
        paste(names(train_data)[too_high_corr], collapse = ", "), "\n")
    train_data <- train_data[, !too_high_corr]
    test_data  <- test_data[, names(train_data)]
  }
  
  # Обучение с glm (fallback glmnet)
  formula_str <- as.formula(paste0("`", target, "` ~ ."))
  fit_success <- TRUE
  model_logit <- tryCatch({
    glm(formula_str, data = train_data, family = binomial)
  }, error = function(e) {
    cat("glm не успя, ще пробваме glmnet.\n")
    fit_success <<- FALSE
    NULL
  })
  
  if (!fit_success || any(is.na(coef(model_logit)))) {
    x <- model.matrix(formula_str, data = train_data)[, -1]
    y <- as.numeric(train_data[[target]])
    if (nrow(x) != length(y)) {
      cat("Несъответствие X/Y редове – премахваме NA наблюдения.\n")
      common_rows <- complete.cases(x, y)
      x <- x[common_rows, ]
      y <- y[common_rows]
    }
    model_logit <- cv.glmnet(x, y, family = "binomial", alpha = 0.5)
    pred_probs <- predict(model_logit,
                          newx = model.matrix(formula_str, data = test_data)[, -1],
                          s = "lambda.min", type = "response")
  } else {
    pred_probs <- predict(model_logit, newdata = test_data, type = "response")
  }
  
  pred_class <- ifelse(pred_probs > 0.3, 1, 0)
  
  # Оценка
  if (nrow(test_data) == 0 || n_distinct(test_data[[target]]) < 2) {
    cat("Няма достатъчно данни за оценка.\n")
    next
  }
  
  conf_matrix <- confusionMatrix(as.factor(pred_class),
                                 as.factor(test_data[[target]]), positive = "1")
  roc_obj <- roc(as.numeric(test_data[[target]]), as.numeric(pred_probs))
  auc_value <- auc(roc_obj)
  
  cat("AUC =", round(auc_value, 3), "\n")
  print(conf_matrix$table)
  
  results[[a]] <- list(Model = model_logit,
                       ConfMatrix = conf_matrix, AUC = auc_value)
}

# Обобщение на резултатите
cat("\n\n Резюме по аларми:\n")

summary_df <- data.frame(
  Аларма = names(results),
  AUC = sapply(results, function(x) round(x$AUC, 3)),
  Accuracy = sapply(results, function(x) round(x$ConfMatrix$overall["Accuracy"], 3)),
  F1 = sapply(results, function(x) round(x$ConfMatrix$byClass["F1"], 3)),
  Precision = sapply(results, function(x) round(x$ConfMatrix$byClass["Precision"], 3)),
  Recall = sapply(results, function(x) round(x$ConfMatrix$byClass["Recall"], 3)),
  Specificity = sapply(results, function(x) round(x$ConfMatrix$byClass["Specificity"], 3))
)

print(summary_df)

# excel_filename <- "regression_results_summary.xlsx"
# write_xlsx(summary_df, excel_filename)
# cat(paste0("Резултатите по аларми са записани в '", excel_filename, "'\n"))

# Разпределение на класовете (t₀)
alarms_t0 <- c("Страничен габарит № Влак_t0",
               "Надтоварена колоос № Влак_t0",
               "Окопан бандаж № Влак_t0")

imbalance_df <- df_model %>%
  select(any_of(alarms_t0)) %>%
  pivot_longer(cols = everything(),
               names_to = "Аларма", values_to = "Стойност")

p_imbalance <- ggplot(imbalance_df, aes(x = Аларма, fill = factor(Стойност))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Разпределение на класовете (t₀)",
       x = "Аларма", y = "Процент",
       fill = "Стойност") +
  theme_minimal(base_size = 13)
print(p_imbalance)

# Feature Importance за glmnet
importance_full <- bind_rows(lapply(names(results), function(a) {
  model <- results[[a]]$Model
  if ("cv.glmnet" %in% class(model)) {
    coefs <- as.data.frame(as.matrix(coef(model, s = "lambda.min")))
    data.frame(term = rownames(coefs), estimate = coefs[,1], Аларма = a)
  }
})) %>%
  filter(term != "(Intercept)", estimate != 0) %>%
  mutate(abs_val = abs(estimate)) %>%
  arrange(Аларма, desc(abs_val))

importance_top15 <- importance_full %>%  # Подготвяме само топ 15 за визуализация
  group_by(Аларма) %>%
  slice_max(abs_val, n = 15)

# Графика с топ 15 + числови стойности
p_imp_glmnet <- ggplot(importance_top15, aes(x = reorder(term, estimate), y = estimate, fill = estimate > 0)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(estimate, 2)),
            hjust = ifelse(importance_top15$estimate > 0, -0.2, 1.2),
            color = "black", size = 3) +
  facet_wrap(~Аларма, scales = "free_y") +
  coord_flip() +
  labs(title = "Feature Importance (glmnet, топ 15 по аларма)",
       x = "Променлива (вкл. dummy описателни фактори)",
       y = "Коефициент (влияние върху вероятността)") +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "firebrick")) +
  theme_minimal(base_size = 13)

print(p_imp_glmnet)

# ВИЗУАЛИЗАЦИЯ 2: Логистична (сигмоидна) регресионна функция
sigmoid <- function(z) {
  1 / (1 + exp(-z))
}

z_vals <- seq(-10, 10, by = 0.1)
p_vals <- sigmoid(z_vals)

sigmoid_df <- data.frame(
  z = z_vals,
  p = p_vals
)

ggplot(sigmoid_df, aes(x = z, y = p)) +
  geom_line(color = "#1565C0", size = 1.3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = 0.5, linetype = "dotted", color = "gray50") +
  annotate("text", x = 6, y = 0.9, label = "P(y=1) → 1", color = "#2E7D32", size = 4.2) +
  annotate("text", x = -6, y = 0.1, label = "P(y=1) → 0", color = "#B71C1C", size = 4.2) +
  labs(
    title = "Логистична регресия: сигмоидна функция",
    subtitle = "Вероятност за аларма спрямо линейния предиктор z = β₀ + β₁x₁ + …",
    x = "Линеен предиктор (z)",
    y = "Вероятност P(y = 1)"
  ) +
  theme_minimal(base_size = 13)



# ============================================================
# ЕТАП 4.2: XGBOOST model
# ============================================================

xgb_results <- list()

alarms_to_model <- c(
  "Страничен габарит № Влак",
  "Надтоварена колоос № Влак",
  "Окопан бандаж № Влак"
)

for (a in alarms_to_model) {
  cat("\n=============================================\n")
  cat("XGBoost модел за аларма:", a, "\n")
  cat("=============================================\n")
  
  target <- paste0(a, "_t0")
  if (!(target %in% names(df_model))) {
    cat("Пропускаме", a, "- няма такава колона.\n")
    next
  }
  
  # Подготовка на данните
  df_temp <- df_model %>%
    select(-`№ ПЖПС`, -Отчетен_период)
  
  df_temp[is.na(df_temp)] <- 0
  names(df_temp) <- make.names(names(df_temp))
  target_safe <- make.names(target)
  
  # One-Hot Encoding: sparse.model.matrix създава разредена матрица с dummy-променливи
  formula_xgb <- as.formula(paste0(target_safe, " ~ . - 1"))  # -1 = без интерсепт
  X_sparse <- sparse.model.matrix(formula_xgb, data = df_temp)
  y <- df_temp[[target_safe]]
  
  # rain/Test разделяне
  set.seed(123)
  train_idx <- createDataPartition(y, p = 0.8, list = FALSE)
  X_train <- X_sparse[train_idx, ]
  y_train <- y[train_idx]
  X_test  <- X_sparse[-train_idx, ]
  y_test  <- y[-train_idx]
  
  # Обучение на XGBoost
  dtrain <- xgb.DMatrix(data = X_train, label = y_train)
  dtest  <- xgb.DMatrix(data = X_test, label = y_test)
  
  params <- list(
    objective = "binary:logistic",
    eval_metric = "auc",
    eta = 0.1,
    max_depth = 4,
    subsample = 0.8,
    colsample_bytree = 0.8,
    min_child_weight = 3
  )
  
  watchlist <- list(train = dtrain, eval = dtest)
  xgb_model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 200,
    watchlist = watchlist,
    verbose = 0
  )
  
  preds_prob <- predict(xgb_model, X_test)
  preds_class <- ifelse(preds_prob > 0.5, 1, 0)
  
  conf <- confusionMatrix(
    as.factor(preds_class),
    as.factor(y_test),
    positive = "1"
  )
  roc_obj <- roc(y_test, preds_prob)
  auc_val <- as.numeric(auc(roc_obj))
  
  xgb_results[[a]] <- list(
    Model = xgb_model,
    ConfMatrix = conf,
    AUC = auc_val
  )
  
  cat("AUC =", round(auc_val, 3), "\n")
  print(conf$table)
}

# Обобщение на резултатите
xgb_summary <- data.frame(
  Аларма = names(xgb_results),
  AUC = sapply(xgb_results, function(x) round(x$AUC, 3)),
  Accuracy = sapply(xgb_results, function(x) round(x$ConfMatrix$overall["Accuracy"], 3)),
  F1 = sapply(xgb_results, function(x) round(x$ConfMatrix$byClass["F1"], 3)),
  Precision = sapply(xgb_results, function(x) round(x$ConfMatrix$byClass["Precision"], 3)),
  Recall = sapply(xgb_results, function(x) round(x$ConfMatrix$byClass["Recall"], 3)),
  Specificity = sapply(xgb_results, function(x) round(x$ConfMatrix$byClass["Specificity"], 3))
)

print(xgb_summary)

# write_xlsx(xgb_summary, "xgb_model_metrics_summary.xlsx")
# cat("XGBoost метриките са записани в xgb_model_metrics_summary.xlsx\n")

# Визуализация 1: Метрики на XGBoost моделите по аларми
xgb_plot <- xgb_summary %>%
  pivot_longer(cols = c(AUC, Accuracy, F1),
               names_to = "Метрика", values_to = "Стойност")

ggplot(xgb_plot, aes(x = Аларма, y = Стойност, fill = Метрика)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = round(Стойност, 3)),
            position = position_dodge(width = 0.7),
            vjust = -0.4, size = 3.5) +
  theme_minimal(base_size = 13) +
  labs(title = "XGBoost: AUC, Accuracy и F1 по аларми (с one-hot encoding)",
       x = "Аларма", y = "Стойност", fill = "Метрика")

# Визуализация 2: Схематична визуализация на принципа на XGBoost
grViz("
digraph xgboost_flow {
  graph [layout = dot, rankdir = LR]
  node [shape = rectangle, style = filled, fillcolor = '#E3F2FD', color = '#1565C0', fontname = Helvetica]

  data [label = 'Входни данни']
  tree1 [label = 'Дърво 1\\n(първоначална прогноза)']
  tree2 [label = 'Дърво 2\\n(корекция на грешките)']
  tree3 [label = 'Дърво 3\\n(допълнителна корекция)']
  ensemble [label = 'Краен ансамблов модел\\n(сумарна прогноза)', fillcolor = '#BBDEFB']

  data -> tree1 -> tree2 -> tree3 -> ensemble

  node [shape = note, color = '#1A237E', fillcolor = '#C5CAE9']
  note1 [label = 'Всеки модел се обучава\\nвърху остатъчните грешки\\nна предишния']
  note2 [label = 'Резултатът е взвешена\\nсума от всички дървета']

  tree2 -> note1 [style = dashed, color = '#1A237E']
  ensemble -> note2 [style = dashed, color = '#1A237E']
}
")

# Визуализация 3: Произволен модел от списъка на дърво от обучен модел
example_model <- xgb_results[[1]]$Model

xgb.plot.tree(
  model = example_model,
  trees = 0,              # първото дърво
  show_node_id = TRUE
)



# ============================================================
# ЕТАП 4.3: RANDOM FOREST model
# ============================================================

rf_results <- list()

alarms_to_model <- c(
  "Страничен габарит № Влак",
  "Надтоварена колоос № Влак",
  "Окопан бандаж № Влак"
)

for (a in alarms_to_model) {
  cat("\n=============================================\n")
  cat("Random Forest модел за аларма:", a, "\n")
  cat("=============================================\n")
  
  target <- paste0(a, "_t0")
  if (!(target %in% names(df_model))) {
    cat("Пропускаме", a, "- няма такава колона.\n")
    next
  }
  
  # Подготовка на данните
  df_temp <- df_model %>%
    select(-`№ ПЖПС`, -Отчетен_период)
  
  df_temp[is.na(df_temp)] <- 0
  names(df_temp) <- make.names(names(df_temp))
  target_safe <- make.names(target)
  
  formula_rf <- as.formula(paste0(target_safe, " ~ ."))
  mm <- model.matrix(formula_rf, data = df_temp)[, -1]
  mm[is.na(mm)] <- 0
  
  y <- df_temp[[target_safe]]
  y[is.na(y)] <- 0
  X <- as.data.frame(mm)
  
  # Train/Test разделяне
  set.seed(123)
  train_idx <- createDataPartition(y, p = 0.8, list = FALSE)
  X_train <- X[train_idx, ]
  y_train <- y[train_idx]
  X_test  <- X[-train_idx, ]
  y_test  <- y[-train_idx]
  
  # Обучение на Random Forest
  rf_model <- randomForest(
    x = X_train, 
    y = as.factor(y_train),
    ntree = 500,
    mtry = floor(sqrt(ncol(X_train))),
    importance = TRUE
  )
  
  # Прогнози
  preds_prob <- predict(rf_model, X_test, type = "prob")[, 2]
  preds_class <- ifelse(preds_prob > 0.5, 1, 0)
  
  # Метрики
  conf <- confusionMatrix(
    as.factor(preds_class),
    as.factor(y_test),
    positive = "1"
  )
  roc_obj <- roc(y_test, preds_prob)
  auc_val <- as.numeric(auc(roc_obj))
  
  rf_results[[a]] <- list(
    Model = rf_model,
    ConfMatrix = conf,
    AUC = auc_val
  )
  
  cat("AUC =", round(auc_val, 3), "\n")
  print(conf$table)
}

# Извличане и обединяване на Feature Importance за всички аларми
rf_importance_summary <- bind_rows(lapply(names(rf_results), function(a) {
  imp <- as.data.frame(importance(rf_results[[a]]$Model))
  imp$Променлива <- rownames(imp)
  rownames(imp) <- NULL
  imp <- imp %>%
    mutate(
      Аларма = a,
      Значимост = MeanDecreaseGini
    ) %>%
    select(Аларма, Променлива, Значимост)
  return(imp)
}))


# Обобщение на резултатите (метрики)
rf_summary <- data.frame(
  Аларма = names(rf_results),
  AUC = sapply(rf_results, function(x) round(x$AUC, 3)),
  Accuracy = sapply(rf_results, function(x) round(x$ConfMatrix$overall["Accuracy"], 3)),
  F1 = sapply(rf_results, function(x) round(x$ConfMatrix$byClass["F1"], 3)),
  Precision = sapply(rf_results, function(x) round(x$ConfMatrix$byClass["Precision"], 3)),
  Recall = sapply(rf_results, function(x) round(x$ConfMatrix$byClass["Recall"], 3)),
  Specificity = sapply(rf_results, function(x) round(x$ConfMatrix$byClass["Specificity"], 3))
)

print(rf_summary)

# write_xlsx(rf_summary, "rf_model_metrics_summary.xlsx")
# cat("Random Forest метриките са записани в rf_model_metrics_summary.xlsx\n")

# Визуализация 1: Метрики на Random Forest моделите по аларми
rf_plot <- rf_summary %>%
  pivot_longer(cols = c(AUC, Accuracy, F1),
               names_to = "Метрика", values_to = "Стойност")

p_metrics <- ggplot(rf_plot, aes(x = Аларма, y = Стойност, fill = Метрика)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = round(Стойност, 3)),
            position = position_dodge(width = 0.7),
            vjust = -0.4, size = 3.5) +
  theme_minimal(base_size = 13) +
  labs(title = "Random Forest: AUC, Accuracy и F1 по аларми",
       x = "Аларма", y = "Стойност", fill = "Метрика")

dev.new(); print(p_metrics)


# Визуализация 2: Схематична структура на Random Forest алгоритъма
p_scheme <- grViz("
digraph random_forest {
  graph [layout = dot, rankdir = LR]
  node [shape = rectangle, style = filled, fillcolor = '#E8F5E9', color = '#1B5E20', fontname = Helvetica]
  data [label = 'Входни данни']
  sample1 [label = 'Извадка 1\\n(bootstrap)']
  sample2 [label = 'Извадка 2\\n(bootstrap)']
  sample3 [label = 'Извадка 3\\n(bootstrap)']
  tree1 [label = 'Дърво 1']
  tree2 [label = 'Дърво 2']
  tree3 [label = 'Дърво 3']
  ensemble [label = 'Комбиниран резултат\\n(гласуване / усредняване)', fillcolor = '#C8E6C9']
  data -> sample1 -> tree1
  data -> sample2 -> tree2
  data -> sample3 -> tree3
  tree1 -> ensemble
  tree2 -> ensemble
  tree3 -> ensemble
  node [shape = note, color = '#2E7D32', fillcolor = '#DCEDC8']
  note1 [label = 'Всяко дърво се обучава\\nвърху различна случайна извадка']
  note2 [label = 'Крайният резултат е\\nсредна прогноза (усредняване)']
  sample2 -> note1 [style = dashed, color = '#2E7D32']
  ensemble -> note2 [style = dashed, color = '#2E7D32']
}
")
dev.new(); print(p_scheme)


# Визуализация 3: Feature Importance
rf_viz <- rf_importance_summary %>%
  filter(Аларма %in% c("Страничен габарит № Влак", "Надтоварена колоос № Влак")) %>%
  group_by(Аларма) %>%
  mutate(Значимост = Значимост / max(Значимост)) %>%
  slice_max(Значимост, n = 15) %>%
  ungroup()

p_feat <- ggplot(rf_viz, aes(x = reorder(Променлива, Значимост), y = Значимост, fill = Аларма)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ Аларма, ncol = 2, scales = "free_y") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Feature Importance (Random Forest, топ 15 по аларма)",
    subtitle = "Нормализирана значимост на променливите според Mean Decrease Gini",
    x = "Променлива (вкл. dummy категориални фактори)",
    y = "Нормализирана значимост"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 13),
    strip.text = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12)
  )

dev.new(); print(p_feat)

# ============================================================
# Финален анализ на избраните модели (Random Forest)
# ============================================================

final_alarms <- c("Страничен габарит № Влак", "Надтоварена колоос № Влак")

for (final_alarm in final_alarms) {
  cat("\n=============================================\n")
  cat("Финален анализ за аларма:", final_alarm, "\n")
  cat("=============================================\n")
  
  final_model <- rf_results[[final_alarm]]$Model
  
  # Подготвяме тестови данни
  target <- make.names(paste0(final_alarm, "_t0"))
  df_temp <- df_model %>% select(-`№ ПЖПС`, -Отчетен_период)
  df_temp[is.na(df_temp)] <- 0
  names(df_temp) <- make.names(names(df_temp))
  mm <- model.matrix(as.formula(paste0(target, " ~ .")), data = df_temp)[, -1]
  mm[is.na(mm)] <- 0
  y <- df_temp[[target]]
  
  set.seed(123)
  train_idx <- createDataPartition(y, p = 0.8, list = FALSE)
  X_test <- as.data.frame(mm[-train_idx, ])
  y_test <- y[-train_idx]
  
  # Прогнози от финалния модел
  preds_prob <- predict(final_model, X_test, type = "prob")[, 2]
  preds_class <- ifelse(preds_prob > 0.5, 1, 0)
  
  # ROC крива
  roc_obj <- roc(y_test, preds_prob)
  auc_val <- round(auc(roc_obj), 3)
  
  p1 <- ggplot(data.frame(
    fpr = 1 - roc_obj$specificities,
    tpr = roc_obj$sensitivities
  ), aes(x = fpr, y = tpr)) +
    geom_line(color = "#1565C0", size = 1.3) +
    geom_abline(linetype = "dashed", color = "gray50") +
    labs(
      title = paste("ROC крива за модел Random Forest –", final_alarm),
      subtitle = paste("AUC =", auc_val),
      x = "False Positive Rate (1 - Specificity)",
      y = "True Positive Rate (Sensitivity)"
    ) +
    theme_minimal(base_size = 13)
  
  print(p1)
  
  # Precision–Recall крива
  pr <- pr.curve(
    scores.class0 = preds_prob[y_test == 1],
    scores.class1 = preds_prob[y_test == 0],
    curve = TRUE
  )
  
  pr_df <- data.frame(Recall = pr$curve[, 1], Precision = pr$curve[, 2])
  
  p2 <- ggplot(pr_df, aes(x = Recall, y = Precision)) +
    geom_line(color = "#2E7D32", size = 1.3) +
    labs(
      title = paste("Precision–Recall крива –", final_alarm),
      subtitle = paste("AUC (PR) =", round(pr$auc.integral, 3)),
      x = "Recall (чувствителност)",
      y = "Precision (прецизност)"
    ) +
    theme_minimal(base_size = 13)
  
  print(p2)
  
  # Confusion Matrix
  conf <- confusionMatrix(
    as.factor(preds_class),
    as.factor(y_test),
    positive = "1"
  )
  
  conf_df <- as.data.frame(conf$table)
  colnames(conf_df) <- c("Предсказан", "Реален", "Честота")
  
  p3 <- ggplot(conf_df, aes(x = Реален, y = Предсказан, fill = Честота)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Честота), size = 6, color = "black") +
    scale_fill_gradient(low = "#E3F2FD", high = "#1565C0") +
    labs(
      title = paste("Confusion Matrix – Random Forest (", final_alarm, ")", sep = ""),
      subtitle = paste("Accuracy =", round(conf$overall["Accuracy"], 3),
                       "| F1 =", round(conf$byClass["F1"], 3))
    ) +
    theme_minimal(base_size = 14)
  
  print(p3)
}

dev.new(); print(p1)
dev.new(); print(p2)
dev.new(); print(p3)



# ============================================================
# ЕТАП 4.4: DECISION TREE model
# ============================================================

names(df_model) <- make.names(names(df_model))

dtree_results <- list()

alarms_to_model <- c(
  "Страничен.габарит...Влак",
  "Надтоварена.колоос...Влак",
  "Окопан.бандаж...Влак"
)

for (a in alarms_to_model) {
  cat("\n=============================================\n")
  cat("Decision Tree модел за аларма:", a, "\n")
  cat("=============================================\n")
  
  target <- paste0(a, "_t0")
  if (!(target %in% names(df_model))) {
    cat("Пропускаме", a, "- няма такава колона.\n")
    next
  }
  
  # Подготовка на данните
  df_temp <- df_model %>%
    select(-matches("ПЖПС", ignore.case = TRUE), -any_of("Отчетен_период"))
  
  df_temp[is.na(df_temp)] <- 0
  
  df_temp <- df_temp %>%
    mutate(across(where(is.character), as.factor))
  
  y <- df_temp[[target]]
  X <- df_temp %>% select(-all_of(target))
  
  set.seed(123)
  train_idx <- createDataPartition(y, p = 0.8, list = FALSE)
  X_train <- X[train_idx, ]
  y_train <- y[train_idx]
  X_test  <- X[-train_idx, ]
  y_test  <- y[-train_idx]
  
  # Обучение на Decision Tree
  tree_data <- data.frame(X_train, y = as.factor(y_train))
  dt_model <- rpart(
    y ~ .,
    data = tree_data,
    method = "class",
    control = rpart.control(cp = 0.01, minsplit = 10, maxdepth = 6)
  )
  
  preds_prob <- predict(dt_model, X_test, type = "prob")[, 2]
  preds_class <- ifelse(preds_prob > 0.5, 1, 0)
  
  conf <- confusionMatrix(as.factor(preds_class), as.factor(y_test), positive = "1")
  roc_obj <- roc(y_test, preds_prob)
  auc_val <- as.numeric(auc(roc_obj))
  
  dtree_results[[a]] <- list(
    Model = dt_model,
    ConfMatrix = conf,
    AUC = auc_val
  )
  
  cat("AUC =", round(auc_val, 3), "\n")
  print(conf$table)
  
  rpart.plot(dt_model, main = paste("Decision Tree за", a), type = 2, extra = 101)
}

# Обобщение на резултатите
dtree_summary <- data.frame(
  Аларма = names(dtree_results),
  AUC = sapply(dtree_results, function(x) round(x$AUC, 3)),
  Accuracy = sapply(dtree_results, function(x) round(x$ConfMatrix$overall["Accuracy"], 3)),
  F1 = sapply(dtree_results, function(x) round(x$ConfMatrix$byClass["F1"], 3)),
  Precision = sapply(dtree_results, function(x) round(x$ConfMatrix$byClass["Precision"], 3)),
  Recall = sapply(dtree_results, function(x) round(x$ConfMatrix$byClass["Recall"], 3)),
  Specificity = sapply(dtree_results, function(x) round(x$ConfMatrix$byClass["Specificity"], 3))
)

print(dtree_summary)

# write_xlsx(dtree_summary, "dtree_model_metrics_summary.xlsx")
# cat("Decision Tree метриките са записани в dtree_model_metrics_summary.xlsx\n")

# Визуализация 1: Метрики на DECISION TREE по аларми
dtree_plot <- dtree_summary %>%
  pivot_longer(cols = c(AUC, Accuracy, F1),
               names_to = "Метрика", values_to = "Стойност") %>%
  drop_na(Стойност)

ggplot(dtree_plot, aes(x = Аларма, y = Стойност, fill = Метрика)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = round(Стойност, 3)),
            position = position_dodge(width = 0.7),
            vjust = -0.4, size = 3.5) +
  theme_minimal(base_size = 13) +
  labs(title = "Decision Tree: AUC, Accuracy и F1 (всички променливи)",
       x = "Аларма", y = "Стойност", fill = "Метрика")

# Визуализация 2: Схематична структура на Decision Tree алгоритъма
grViz("
digraph decision_tree {
  graph [layout = dot, rankdir = TB]
  node [shape = rectangle, style = filled, fillcolor = '#FFF8E1', color = '#F57F17', fontname = Helvetica]

  data [label = 'Входни данни']
  split1 [label = 'Признак A < праг']
  split2_left [label = 'Признак B < праг']
  split2_right [label = 'Признак C < праг']
  leaf1 [label = 'Клас 0\\n(няма аларма)', fillcolor = '#C8E6C9']
  leaf2 [label = 'Клас 1\\n(аларма)', fillcolor = '#FFCDD2']
  leaf3 [label = 'Клас 0\\n(няма аларма)', fillcolor = '#C8E6C9']
  leaf4 [label = 'Клас 1\\n(аларма)', fillcolor = '#FFCDD2']

  data -> split1
  split1 -> split2_left [label = 'Да']
  split1 -> split2_right [label = 'Не']
  split2_left -> leaf1 [label = 'Да']
  split2_left -> leaf2 [label = 'Не']
  split2_right -> leaf3 [label = 'Да']
  split2_right -> leaf4 [label = 'Не']

  node [shape = note, fillcolor = '#FFFDE7', color = '#F9A825']
  note1 [label = 'Всеки възел съдържа условие за разделяне\\nспоред стойност на признак']
  note2 [label = 'Листата съдържат прогнозния клас (0 или 1)']

  split1 -> note1 [style = dashed, color = '#F57F17']
  leaf2 -> note2 [style = dashed, color = '#F57F17']
}
")



# ============================================================
# ЕТАП 4.5: Support Vector Machine (SVM) model
# ============================================================

names(df_model) <- make.names(names(df_model))

svm_opt_results <- list()

alarms_to_model <- c(
  "Страничен.габарит...Влак",
  "Надтоварена.колоос...Влак",
  "Окопан.бандаж...Влак"
)

for (a in alarms_to_model) {
  cat("\n=============================================\n")
  cat("Оптимизиран SVM модел за аларма:", a, "\n")
  cat("=============================================\n")
  
  target <- paste0(a, "_t0")
  if (!(target %in% names(df_model))) {
    cat("Пропускаме", a, "- няма такава колона.\n")
    next
  }
  
  # Подготовка на данните
  df_temp <- df_model %>%
    select(-matches("ПЖПС"), -matches("Отчетен_период")) %>%
    mutate(across(where(is.character), as.factor))
  
  df_temp <- df_temp %>%
    mutate(across(where(is.factor), ~ as.numeric(as.factor(.x)) - 1)) %>%
    mutate(across(everything(), ~ ifelse(is.na(.x), 0, .x)))
  
  y <- as.factor(df_temp[[target]])
  X <- df_temp %>% select(-all_of(target))
  
  # Разделяне на train/test
  set.seed(123)
  train_idx <- createDataPartition(y, p = 0.8, list = FALSE)
  X_train <- X[train_idx, ]
  y_train <- y[train_idx]
  X_test  <- X[-train_idx, ]
  y_test  <- y[-train_idx]
  
  # Скалиране (SVM изисква нормализирани данни)
  preproc <- preProcess(X_train, method = c("center", "scale"))
  X_train_scaled <- predict(preproc, X_train)
  X_test_scaled  <- predict(preproc, X_test)
  
  # Настройки за крос-валидация и оптимизация
  ctrl <- trainControl(
    method = "cv",
    number = 5,             # 5-fold cross-validation
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    savePredictions = TRUE
  )
  
  tune_grid <- expand.grid(
    sigma = c(0.001, 0.005, 0.01, 0.05),
    C = c(0.1, 1, 5, 10)
  )
  
  # Обучение с caret::train (автоматична оптимизация)
  set.seed(123)
  svm_tuned <- train(
    x = X_train_scaled,
    y = factor(ifelse(y_train == 1, "Yes", "No")),  # caret иска фактор с нива Yes/No
    method = "svmRadial",
    trControl = ctrl,
    tuneGrid = tune_grid,
    metric = "ROC"
  )
  
  cat("Най-добри параметри:", "\n")
  print(svm_tuned$bestTune)
  
  preds_prob <- predict(svm_tuned, X_test_scaled, type = "prob")[, "Yes"]
  preds_class <- ifelse(preds_prob > 0.5, 1, 0)

  conf <- confusionMatrix(
    as.factor(preds_class),
    as.factor(y_test),
    positive = "1"
  )
  roc_obj <- roc(y_test, preds_prob)
  auc_val <- as.numeric(auc(roc_obj))
  
  svm_opt_results[[a]] <- list(
    Model = svm_tuned,
    ConfMatrix = conf,
    AUC = auc_val
  )
  
  cat("AUC =", round(auc_val, 3), "\n")
  print(conf$table)
}

# Обобщение на резултатите
svm_opt_summary <- data.frame(
  Аларма = names(svm_opt_results),
  AUC = sapply(svm_opt_results, function(x) round(x$AUC, 3)),
  Accuracy = sapply(svm_opt_results, function(x) round(x$ConfMatrix$overall["Accuracy"], 3)),
  F1 = sapply(svm_opt_results, function(x) round(x$ConfMatrix$byClass["F1"], 3)),
  Precision = sapply(svm_opt_results, function(x) round(x$ConfMatrix$byClass["Precision"], 3)),
  Recall = sapply(svm_opt_results, function(x) round(x$ConfMatrix$byClass["Recall"], 3)),
  Specificity = sapply(svm_opt_results, function(x) round(x$ConfMatrix$byClass["Specificity"], 3))
)

print(svm_opt_summary)

# write_xlsx(svm_opt_summary, "svm_optimized_model_metrics_summary.xlsx")
# cat("Оптимизираният SVM е записан в svm_optimized_model_metrics_summary.xlsx\n")

# Визуализация 1: Метрики на Support Vector Machine по аларми
svm_opt_plot <- svm_opt_summary %>%
  pivot_longer(cols = c(AUC, Accuracy, F1),
               names_to = "Метрика", values_to = "Стойност") %>%
  drop_na(Стойност)

ggplot(svm_opt_plot, aes(x = Аларма, y = Стойност, fill = Метрика)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = round(Стойност, 3)),
            position = position_dodge(width = 0.7),
            vjust = -0.4, size = 3.5) +
  theme_minimal(base_size = 13) +
  labs(title = "Оптимизиран SVM: AUC, Accuracy и F1 по аларми",
       x = "Аларма", y = "Стойност", fill = "Метрика")

# Визуализация 2: Принципна схема на SVM
grViz("
digraph svm_real {
  graph [layout = dot, rankdir = TB, splines = ortho]
  node [shape = rectangle, style = filled, fillcolor = '#E8EAF6',
        color = '#1A237E', fontname = Helvetica, fontsize = 11, width = 3, height = 0.8]

  # Ниво 1
  data [label = 'Входни данни\\n(df_model)']
  
  # Ниво 2
  preprocess [label = 'Предобработка на данните\\n(кодиране, запълване на NA, премахване на ненужни променливи)']
  scaling [label = 'Скалиране\\n(центриране и нормализация)']
  
  # Ниво 3
  cv [label = 'Крос-валидация (5-fold CV)\\nи избор на оптимални параметри C и σ']
  
  # Ниво 4
  kernel [label = 'Ядрова трансформация\\n(RBF Kernel)']
  hyperplane [label = 'Оптимална хиперплоскост\\n(максимален марж)']
  
  # Ниво 5
  support [label = 'Опорни вектори\\n(критични наблюдения)']
  classify [label = 'Класификация на нови данни\\n(Аларма / Без аларма)', fillcolor = '#C5CAE9']

  # Връзки
  data -> preprocess -> scaling -> cv -> kernel -> hyperplane -> support -> classify

  # Бележка
  node [shape = note, color = '#283593', fillcolor = '#E8EAF6', fontsize = 10]
  note1 [label = 'Моделът използва оптимални параметри\\n(C, σ) за постигане на максимален ROC (AUC)']
  cv -> note1 [style = dashed, color = '#283593']
}
")



# ============================================================
# ЕТАП 4.6: k-NEAREST NEIGHBORS (kNN) model 
# ============================================================

names(df_model) <- make.names(names(df_model))

knn_results <- list()

alarms_to_model <- c(
  "Страничен.габарит...Влак",
  "Надтоварена.колоос...Влак",
  "Окопан.бандаж...Влак"
)

for (a in alarms_to_model) {
  cat("\n=============================================\n")
  cat("kNN модел за аларма:", a, "\n")
  cat("=============================================\n")
  
  target <- paste0(a, "_t0")
  if (!(target %in% names(df_model))) {
    cat("Пропускаме", a, "- няма такава колона.\n")
    next
  }
  
  # Подготовка на данните
  df_temp <- df_model %>%
    select(-matches("ПЖПС", ignore.case = TRUE), -any_of("Отчетен_период")) %>%
    mutate(across(where(is.character), as.factor),
           across(where(is.factor), ~ as.numeric(as.factor(.x)) - 1),
           across(everything(), ~ ifelse(is.na(.x), 0, .x)))
  
  y <- factor(df_temp[[target]], levels = c(0, 1))
  X <- df_temp %>% select(-all_of(target))
  
  # -Train/Test разделяне
  set.seed(123)
  train_idx <- createDataPartition(y, p = 0.8, list = FALSE)
  X_train <- X[train_idx, ];  y_train <- y[train_idx]
  X_test  <- X[-train_idx, ]; y_test  <- y[-train_idx]
  
  # --- Скалиране ---
  preproc <- preProcess(X_train, method = c("center", "scale"))
  X_train_scaled <- predict(preproc, X_train)
  X_test_scaled  <- predict(preproc, X_test)
  
  # Крос-валидация и оптимизация
  ctrl <- trainControl(method = "cv", number = 5,
                       classProbs = TRUE, summaryFunction = twoClassSummary)
  tune_grid <- expand.grid(k = c(3, 5, 7, 9, 11))
  
  set.seed(123)
  knn_model <- train(
    x = X_train_scaled,
    y = factor(ifelse(y_train == 1, "Yes", "No")),
    method = "knn",
    trControl = ctrl,
    tuneGrid = tune_grid,
    metric = "ROC"
  )
  
  cat("🔧 Най-добър брой съседи:", knn_model$bestTune$k, "\n")
  
  preds_prob <- predict(knn_model, X_test_scaled, type = "prob")[, "Yes"]
  preds_class <- ifelse(preds_prob > 0.5, 1, 0)
  
  conf <- confusionMatrix(as.factor(preds_class), as.factor(y_test), positive = "1")
  roc_obj <- roc(y_test, preds_prob)
  
  knn_results[[a]] <- list(Model = knn_model,
                           ConfMatrix = conf,
                           AUC = as.numeric(auc(roc_obj)))
  
  cat("AUC =", round(knn_results[[a]]$AUC, 3), "\n")
  print(conf$table)
}

# Обобщение на резултатите
knn_summary <- data.frame(
  Аларма = names(knn_results),
  AUC = sapply(knn_results, \(x) round(x$AUC, 3)),
  Accuracy = sapply(knn_results, \(x) round(x$ConfMatrix$overall["Accuracy"], 3)),
  F1 = sapply(knn_results, \(x) round(x$ConfMatrix$byClass["F1"], 3)),
  Precision = sapply(knn_results, \(x) round(x$ConfMatrix$byClass["Precision"], 3)),
  Recall = sapply(knn_results, \(x) round(x$ConfMatrix$byClass["Recall"], 3)),
  Specificity = sapply(knn_results, \(x) round(x$ConfMatrix$byClass["Specificity"], 3))
)

print(knn_summary)

# write_xlsx(knn_summary, "knn_model_metrics_summary.xlsx")
# cat("Резултатите от kNN модела са записани в 'knn_model_metrics_summary.xlsx'\n")

# Визуализация 1: Метрики на k-NEAREST NEIGHBORS по аларми
knn_plot <- knn_summary %>%
  pivot_longer(cols = c(AUC, Accuracy, F1), names_to = "Метрика", values_to = "Стойност") %>%
  drop_na(Стойност)

ggplot(knn_plot, aes(x = Аларма, y = Стойност, fill = Метрика)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = round(Стойност, 3)),
            position = position_dodge(width = 0.7),
            vjust = -0.4, size = 3.5) +
  theme_minimal(base_size = 13) +
  labs(title = "kNN: AUC, Accuracy и F1 по аларми",
       x = "Аларма", y = "Стойност", fill = "Метрика")


# Визуализация 2: Принципна схема на kNN
grViz("
digraph knn_real {
  graph [layout = dot, rankdir = TB]
  node [shape = rectangle, style = filled, fillcolor = '#E8F5E9',
        color = '#1B5E20', fontname = Helvetica, fontsize = 11, width = 3, height = 0.8]

  # Ниво 1
  data [label = 'Входни данни\\n(df_model)']

  # Ниво 2
  preprocess [label = 'Предобработка и скалиране\\n(центриране и нормализация)']
  cv [label = 'Крос-валидация\\nи избор на оптимално k']

  # Ниво 3
  distance [label = 'Изчисляване на разстояния\\nдо всички наблюдения\\n(Евклидово разстояние)']
  neighbors [label = 'Избор на k най-близки съседи\\n(оптимално k от CV)']

  # Ниво 4
  voting [label = 'Гласуване и определяне на класа\\n(по мнозинство или тегла)']

  # Ниво 5
  result [label = 'Краен резултат\\n(Аларма / Без аларма)', fillcolor = '#C8E6C9']

  # Връзки
  data -> preprocess -> {cv distance}
  cv -> neighbors
  distance -> neighbors -> voting -> result

  # Бележка
  node [shape = note, color = '#33691E', fillcolor = '#DCEDC8', fontsize = 10]
  note1 [label = 'Всеки тестов обект се сравнява с\\nвсички наблюдения от обучаващата извадка']
  distance -> note1 [style = dashed, color = '#33691E']
}
")



# ============================================================
# ЕТАП 4.7: MULTILAYER PERCEPTRON (MLP, caret::nnet)
# ============================================================

set.seed(123)
names(df_model) <- make.names(names(df_model))

mlp_results <- list()

alarms_to_model <- c(
  "Страничен.габарит...Влак",
  "Надтоварена.колоос...Влак",
  "Окопан.бандаж...Влак"
)

for (a in alarms_to_model) {
  cat("\n=============================================\n")
  cat("🔹 MLP модел за аларма:", a, "\n")
  cat("=============================================\n")
  
  target <- paste0(a, "_t0")
  lag_vars <- paste0(a, c("_t.1", "_t.2", "_t.3"))
  
  if (!(target %in% names(df_model)) || !all(lag_vars %in% names(df_model))) {
    cat("Пропускаме", a, "- липсват нужните лагови променливи.\n")
    next
  }
  
  # Подготовка на данните
  df_temp <- df_model %>%
    select(all_of(c(target, lag_vars))) %>%
    mutate(across(everything(), ~ ifelse(is.na(.x), 0, .x)))
  
  y <- as.factor(df_temp[[target]])
  X <- df_temp %>% select(-all_of(target))
  
  # Разделяне train/test
  train_idx <- createDataPartition(y, p = 0.8, list = FALSE)
  X_train <- X[train_idx, ]
  y_train <- y[train_idx]
  X_test  <- X[-train_idx, ]
  y_test  <- y[-train_idx]
  
  # Скалиране
  preproc <- preProcess(X_train, method = c("center", "scale"))
  X_train_scaled <- predict(preproc, X_train)
  X_test_scaled  <- predict(preproc, X_test)
  
  # Настройки за cross-validation
  ctrl <- trainControl(
    method = "cv",
    number = 5,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    savePredictions = TRUE
  )
  
  # Грид с параметри
  tune_grid <- expand.grid(
    size = c(3, 5, 7, 10),   # брой неврони
    decay = c(0.01, 0.1, 0.5) # регуляризация
  )
  
  # Обучение
  mlp_model <- train(
    x = X_train_scaled,
    y = factor(ifelse(y_train == 1, "Yes", "No")),
    method = "nnet",
    trControl = ctrl,
    tuneGrid = tune_grid,
    metric = "ROC",
    trace = FALSE,
    maxit = 500
  )
  
  cat("Най-добри параметри:\n")
  print(mlp_model$bestTune)
  
  preds_prob <- predict(mlp_model, X_test_scaled, type = "prob")[, "Yes"]
  preds_class <- ifelse(preds_prob > 0.5, 1, 0)
  
  conf <- confusionMatrix(as.factor(preds_class), as.factor(y_test), positive = "1")
  roc_obj <- roc(y_test, preds_prob)
  auc_val <- as.numeric(auc(roc_obj))
  
  mlp_results[[a]] <- list(
    Model = mlp_model,
    ConfMatrix = conf,
    AUC = auc_val
  )
  
  cat("AUC =", round(auc_val, 3), "\n")
  print(conf$table)
}

# бобщение на резултатите
mlp_summary <- data.frame(
  Аларма = names(mlp_results),
  AUC = sapply(mlp_results, function(x) round(x$AUC, 3)),
  Accuracy = sapply(mlp_results, function(x) round(x$ConfMatrix$overall["Accuracy"], 3)),
  F1 = sapply(mlp_results, function(x) round(x$ConfMatrix$byClass["F1"], 3)),
  Precision = sapply(mlp_results, function(x) round(x$ConfMatrix$byClass["Precision"], 3)),
  Recall = sapply(mlp_results, function(x) round(x$ConfMatrix$byClass["Recall"], 3)),
  Specificity = sapply(mlp_results, function(x) round(x$ConfMatrix$byClass["Specificity"], 3))
)

print(mlp_summary)

# write_xlsx(mlp_summary, "mlp_model_metrics_summary.xlsx")
# cat("MLP метриките са записани в mlp_model_metrics_summary.xlsx\n")

# Визуализация 1: Метрики на MLP по аларми
mlp_plot <- mlp_summary %>%
  pivot_longer(cols = c(AUC, Accuracy, F1),
               names_to = "Метрика", values_to = "Стойност")

ggplot(mlp_plot, aes(x = Аларма, y = Стойност, fill = Метрика)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = round(Стойност, 3)),
            position = position_dodge(width = 0.7),
            vjust = -0.4, size = 3.5) +
  theme_minimal(base_size = 13) +
  labs(title = "MLP (Neural Network): AUC, Accuracy и F1 по аларми",
       x = "Аларма", y = "Стойност", fill = "Метрика")

# Визуализация 2: Принципна архитектура на MLP невронната мрежа
grViz("
digraph mlp {
  graph [layout = dot, rankdir = LR]
  node [shape = circle, fixedsize = true, width = 0.9, fontsize = 10, style = filled, fillcolor = '#E3F2FD', color = '#1565C0']

  # Input layer
  i1 [label = 't-1']
  i2 [label = 't-2']
  i3 [label = 't-3']

  # Hidden layer (примерно 5 неврона)
  node [fillcolor = '#C8E6C9', color = '#2E7D32']
  h1 [label = 'h1']
  h2 [label = 'h2']
  h3 [label = 'h3']
  h4 [label = 'h4']
  h5 [label = 'h5']

  # Output layer
  node [fillcolor = '#FFCDD2', color = '#B71C1C']
  o1 [label = 'Аларма / Без аларма']

  # Connections
  i1 -> {h1 h2 h3 h4 h5}
  i2 -> {h1 h2 h3 h4 h5}
  i3 -> {h1 h2 h3 h4 h5}
  {h1 h2 h3 h4 h5} -> o1

  # Layer labels
  node [shape = plaintext, fontsize = 11]
  ilabel [label = 'Входен слой (лагови признаци)']
  hlabel [label = 'Скрит слой (3–10 неврона, sigmoid активация)']
  olabel [label = 'Изходен слой (вероятност за аларма)']

  {rank = same; i1 i2 i3 ilabel}
  {rank = same; h1 h2 h3 h4 h5 hlabel}
  {rank = same; o1 olabel}
}
")



# ============================================================
# ЕТАП 5: Хипотезно тестване на значимостта на входните характеристики (2 аларми)
# ============================================================

alarms_to_test <- c("Страничен габарит № Влак", "Надтоварена колоос № Влак")
combined_t_test <- list()

for (alarm in alarms_to_test) {
  target_col <- paste0(alarm, "_t0")
  names(df_model) <- make.names(names(df_model))
  target_safe <- make.names(target_col)
  
  # Избираме числовите променливи + целевата
  df_num <- df_model %>%
    select(where(is.numeric)) %>%
    mutate(target = df_model[[target_safe]]) %>%
    filter(!is.na(target))
  
  # Провеждаме t-тест за всяка променлива
  t_res <- lapply(names(df_num)[names(df_num) != "target"], function(var) {
    if (length(unique(df_num[[var]])) > 1) {
      test <- tryCatch(t.test(df_num[[var]] ~ df_num$target), error = function(e) NULL)
      if (!is.null(test)) {
        data.frame(
          Аларма = alarm,
          Променлива = var,
          Средна_стойност_0 = mean(df_num[[var]][df_num$target == 0], na.rm = TRUE),
          Средна_стойност_1 = mean(df_num[[var]][df_num$target == 1], na.rm = TRUE),
          p_value = test$p.value
        )
      }
    }
  }) %>% bind_rows()
  
  combined_t_test[[alarm]] <- t_res
}

# Обединяваме резултатите в една таблица
t_test_all <- bind_rows(combined_t_test)

# Филтрираме само значимите променливи (p < 0.05)
t_test_significant <- t_test_all %>%
  filter(p_value < 0.05) %>%
  arrange(Аларма, p_value)

write_xlsx(
  list(
    "Всички променливи" = t_test_all,
    "Само значими (p<0.05)" = t_test_significant
  ),
  "t_test_results_RF_две_аларми.xlsx"
)


