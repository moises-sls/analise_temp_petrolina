library(tidyverse)
library(janitor)

df <- read.csv("dados/petrolina.csv",
    header = TRUE, sep = ";",
    check.names = FALSE,
    fileEncoding = "Latin1", dec = ","
)

# queremos apenas as rows que possuem valores da variavel resposta
df <- df |>
    clean_names() |>
    select(c(1:4, 8:9, 16:19)) |>
    mutate(data = as.Date(data, format = "%Y/%m/%d")) |>
    filter(!is.na(temperatura_do_ar_bulbo_seco_horaria_c))

# queremos construir os vetores que possuem todas as datas de uma determinada estacao
min(df$data)
max(df$data)
verao <- seq(as.Date("2024-01-01"), as.Date("2024-03-19"), by = "day")
outono <- seq(as.Date("2024-03-20"), as.Date("2024-06-19"), by = "day")
inverno <- seq(as.Date("2024-06-20"), as.Date("2024-09-21"), by = "day")
primavera <- seq(as.Date("2024-09-22"), as.Date("2024-09-26"), by = "day")

df <- df |>
    mutate(estacao = case_when(
        data %in% verao ~ "verao",
        data %in% outono ~ "outono",
        data %in% inverno ~ "inverno",
        data %in% primavera ~ "primavera"
    )) |>
    mutate(estacao = as.factor(estacao))


df |>
    group_by(estacao) |>
    summarise(temp_media = mean(temperatura_do_ar_bulbo_seco_horaria_c), n = n())

df |>
    summarise(n = n())

# proporçao de NA em todas as colunas
df |>
    summarise(
        across(everything(),
            ~ mean(is.na(.x), na.rm = TRUE)
        )
    )


###### Inputação de dados #####
# primeiro imputar por dia
# depois pela media do dia anterior + prox dia
# media da estacao
#
#
# verificar se a inferencia muda para estes diferentes tipos de inputaçao




df |>
    filter(is.na(precipitacao_total_horario_mm)) |>
    group_by(data) |>
    summarise(n = n())

precip_na <- c(
    "2024-04-01",
    "2024-04-02",
    "2024-04-03",
    "2024-04-04"
)

df |>
    filter(data %in% precip_na) |>
    group_by(data) |>
    summarise(media = mean(precipitacao_total_horario_mm, na.rm = TRUE))

df |>
    group_by(estacao) |>
    summarise(media = mean(precipitacao_total_horario_mm, na.rm = TRUE), n = n())


df |>
    filter(data == "2024-04-01") |>
    select(estacao)







############
df |>
    summarise(across(everything(),
                     ~ mean(is.na(.x), na.rm = TRUE))
    )


# temp e umi:
# como em ambas as variaveis assumem valores em 22 das 24 horas de um dia, iremos imputar a media
# diaria das variaveis
#
# as variaveis nao possuem valores no dia 25/09 e dia 26/09, como o dia 26 é o ultimo dia do banco de dados, repetiremos a media do dia anterior
#
#
# temp
df |>
    filter(is.na(temperatura_do_ponto_de_orvalho_c))

media_tmp_orvalho_25_09 <- df |>
    filter(data == "2024-09-25") |>
    summarise(media = mean(temperatura_do_ponto_de_orvalho_c, na.rm = TRUE)) |>
    pull(media)

df <- df |>
    mutate(
        temperatura_do_ponto_de_orvalho_c = case_when(
            data == "2024-09-25" & is.na(temperatura_do_ponto_de_orvalho_c) ~ media_tmp_orvalho_25_09,
            data == "2024-09-26" & is.na(temperatura_do_ponto_de_orvalho_c) ~ media_tmp_orvalho_25_09,
            .default = temperatura_do_ponto_de_orvalho_c
        )
    )
#
#
#
# umi
media_umi_25_09 <- df |>
    filter(data == "2024-09-25") |>
    summarise(media = mean(umidade_relativa_do_ar_horaria_percent, na.rm = TRUE)) |>
    pull(media)

df <- df |>
    mutate(umidade_relativa_do_ar_horaria_percent = case_when(
        data == "2024-09-25" & is.na(umidade_relativa_do_ar_horaria_percent) ~ media_umi_25_09,
        data == "2024-09-26" & is.na(umidade_relativa_do_ar_horaria_percent) ~ media_umi_25_09,
        .default = umidade_relativa_do_ar_horaria_percent
    ))

df |>
    summarise(across(everything(),
                     ~ mean(is.na(.x), na.rm = TRUE)))

#
#
#
# precip
df |>
    filter(is.na(precipitacao_total_horario_mm)) |>
    select(data)

c("2024-04-01",
  "2024-04-02",
  "2024-04-03",
  "2024-04-04")

# dia 31-03 precipitacao <- 0
# dia 01-04 precipitacao <- 0
# dia 04-04 precipitacao <- quase 0
# dia 05-04 precipitacao <- quase 0

df <- df |>
    mutate(precipitacao_total_horario_mm = if_else(is.na(precipitacao_total_horario_mm), 0, precipitacao_total_horario_mm))
######################################################

df_final <- df |>
    select(!1:2)

mod1 <- lm(temperatura_do_ar_bulbo_seco_horaria_c ~ . - (vento_direcao_horaria_gr_gr + vento_rajada_maxima_m_s) , data = df_final)
summary(mod1)
plot(mod1)

anova(mod1)

x <- 1:100
y <- 2 * x + 2 * rnorm(x)

lm(y ~ x)

plot(y ~ x)

library(car)
vif()
install.packages("car")

vif(mod1)

cor(df[, !(names(df) %in% c("data", "hora", "temperatura_do_ar_bulbo_seco_horaria_c"))])

df |>
    select(!c(data, hora_utc, temperatura_do_ar_bulbo_seco_horaria_c)) |>
    cor()


!(names(df) %in% c("data", "hora", "temperatura_do_ar_bulbo_seco_horaria_c"))
