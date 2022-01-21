
  # Download e tratamento da tabela de resultados da MEGA SENA

# OBSERVAÇÕES -----------------------------------------------------------------

  # Este script foi desenvolvido dentro de um projeto. Logo, a referência aos
  # objetos consideram a existência de um diretório padronizado pelo projeto,
  # por isso a referência aos objetos é direta.

# PACOTES ---------------------------------------------------------------------

library(tidyverse)
library(scales)
library(readxl)
library(lubridate)
library(rvest)

# LINK DA TABELA ----------------------------------------------------------####

  # Página do link
# p0 <- "http://loterias.caixa.gov.br/wps/portal/loterias/landing/megasena"
# p0 <- rvest::read_html(x = p0, encoding = "UTF-8")

  # Raspagem do link
# p1 <- rvest::html_element(x = p0, xpath = '//*[@id="resultados"]/div[2]/ul/li/a')
# p1 %>% as.character()
# p1 <- gsub('.*href=["\"]', "", gsub('["\"] target.*', "", p1))

   # Até o presente momento, 17/01/2022, não resolvi o fato de que o link está
   # hospedado em uma href de classe zeta que só fornece metade do link, impos-
   # sibilitando a raspagem do link completo.

  # Link direto da tabela
x <- 'http://loterias.caixa.gov.br/wps/portal/loterias/landing/megasena/!ut/p/a
1/04_Sj9CPykssy0xPLMnMz0vMAfGjzOLNDH0MPAzcDbwMPI0sDBxNXAOMwrzCjA0sjIEKIoEKnN0dP
UzMfQwMDEwsjAw8XZw8XMwtfQ0MPM2I02-AAzgaENIfrh-FqsQ9wNnUwNHfxcnSwBgIDUyhCvA5EawA
jxsKckMjDDI9FQE-F4ca/dl5/d5/L2dBISEvZ0FBIS9nQSEh/pw/Z7_HGK818G0K8DBC0QPVN93KQ10
G1/res/id=historicoHTML/c=cacheLevelPage/=/'

  # Retirada de quebras de linha intencionais
x <- gsub("\n", "", x)

# DOWNLOAD DA TABELA ------------------------------------------------------####

  # Os seguintes comandos baixam no modo list os dados da tabela.
tb <- read_html(x = x, encoding = "UTF-8") %>% html_elements(xpath = '//td')

# MONTANDO A TABELA -----------------------------------------------------------

  # Os objetos a seguir condicionam as informações em formato html para tornar
  # possível a transformação em tabela.

    # o vetor concurso marca o local onde cada número de concurso está no 
    # objeto tb.
concurso <- grep(">../../....<", tb %>% as.character())-2
    # Assim, a diferença na quantidade de células entre um concurso e outro po-
    # de ser calculada com a subtração da posição de um concurso e a posição do
    # concurso anterior.
celulas <- concurso[2:length(concurso)]-concurso[1:(length(concurso)-1)]
    # Como a subtração entre as posições dos números dos concursos não contem-
    # o último concurso, o seguinte código calcula esta quantidade de células.
celulas <- c(celulas, c(length(tb)-concurso[length(concurso)])+1)

  # Objeto do conteúdo da tabela.
t0 <- NULL

  # Loop de extração por linha (concurso) e montagem da tabela.
for (i in 1:length(concurso)) {
    # O fluxo if verifica se o prêmio foi sorteado. Dessa forma, duplica a
    # quantidade de linhas em caso de mais de um ganhador.
  if (celulas[i] == 22) {
    # o objeto 'a' cria uma matriz de 1 linha com as informações do concurso i
    # que será adicionada à tabela principal. as células de número 22 se refer-
    # em a prêmios sem ganhadores no prêmio principal.
    a <- gsub('.*>', "",
              gsub('</.*', '',
                   tb[concurso[i]:(concurso[i]+(celulas[i]-1))])) %>% t
    # Como a coluna 'cidade' se divide em cidade e UF, a nossa tabela terá 23
    # colunas, ao invés de 22 como no site da CAIXA. Logo, é necessário trans-
    # por as informações de forma que as colunas 16 e 17 fiquem livres para as
    # informações da coluna 'cidade'.
    a[17:23] <- a[16:22]
    a[16:17] <- NA # Transformação das células cidade e uf em NA.
  } else {
    # o else deste fluxo trata das linhas de concursos sorteados. O objeto li-
    # nhas se refere à quantidade de ganhadores por prêmio.
    linhas <- (celulas[i]-22)/2
    # A matriz a é criada para ajustar as informações da matriz 'b', que contém
    # informações cruas advindas do objeto tb.
    a = matrix(data = NA,
               nrow = linhas,
               ncol = 23)
    b <- gsub('.*>', "",
              gsub('</.*', '',
                   tb[concurso[i]:(concurso[i]+(celulas[i]-1))])) %>% t
    # Este loop ordena as informações de 'b' em 'a'.
    for (k in 1:linhas) {
      a[k, 1:15] <- b[1, 1:15]
      colunas <- 15+c(k*2, (k*2)+1)
      a[k, 16:17] <- b[1, colunas]
      a[k, 18:23] <- b[1, (length(b)-5):length(b)]
      rm(k)
    }
    rm(linhas, b)
  }
  t0 <- rbind(t0, a)
  rm(i, a)
}
t0 <- data.frame(t0)

names(t0) <- c("Concurso",	"Local",	"Data do Sorteio",	"Coluna 1",
               "Coluna 2", "Coluna 3",	"Coluna 4",	"Coluna 5",
               "Coluna 6", "Ganhadores Faixa 1",	"Ganhadores Faixa 2",
               "Ganhadores Faixa 3", "Rateio Faixa 1",	"Rateio Faixa 2",
               "Rateio Faixa 3",	"Cidade", "UF", "Valor Arrecadado",
               "Estimativa para o próximo concurso",
               "Valor Acumulado Próximo Concurso",	"Acumulado",
               "Sorteio Especial", "Observação")

  # Área de teste da tabela
any(nchar(t0$`Data do Sorteio`) != 10) # O resultado deve ser FALSE
any(nchar(t0$UF) > 2, na.rm = T)       # O resultado deve ser FALSE
any(nchar(t0$Concurso) > 4)            # O resultado deve ser FALSE

# TRATAMENTO DA TABELA --------------------------------------------------------

  # Transformação de células vazias ("") am NA.
for (i in 1:length(t0)) {
  t0[which(t0[, i] == ""), i] <- NA
  rm(i)
}

  # Os códigos a seguir tratam da transformação dos dados, que estão no
  # formato 'character' para o formato desejado.
    # Datas
t0$`Data do Sorteio` <- dmy(t0$`Data do Sorteio`)
    # Colunas 4 a 12 transformadas em 'numeric'.
for (i in 4:12) {
  t0[, i] <- as.numeric(t0[, i])
  rm(i)
}
    # Colunas 13 a 15 e 18 a 20 transformadas em 'numeric'. São valores mo-
    # netários.
for (i in c(13:15, 18:20)) {
  t0[, i] <- as.numeric(gsub(",", ".",
                             gsub("\\.", "",
                                  t0[, i])))
}

  # Números das linhas
rownames(t0) <- 1:nrow(t0)

  # Carregando funções necessárias
    # repet, regiao e crmega
knitr::spin_child("0_funcoes.R")

  # Coluna 'Repetidos': identifica com 1 o caso em que há mais de um ga-
  # nhador. Logo, uma das linhas receberá 0 e as outras, repetidas, re-
  # ceberão 0. O caso de apenas um ganhador ou de não haver ganhador tam-
  # bém receberão 0. Esta coluna serve para detectar valores repetidos.
t0 <- cbind(t0,Repetidos = repet(t0$Concurso))

  # Coluna ano: estabelece o ano do concurso.
t0 <- cbind(t0,ano = year(t0$`Data do Sorteio`))

# SALVAR A TABELA -------------------------------------------------------------

  # A tabela é salva na pasta principal do projeto, com a data do último
  # sorteio no nome do arquivo. Logo, ao longo de vários concursos, criam-se
  # inúmeros arquivos.
saveRDS(object = t0, paste0("bd_resultados_", last(t0$`Data do Sorteio`), ".RDS"))

rm(celulas, colunas, concurso, i, tb, x)
