
  # Funções para o tratamento da tabela de resultados MEGA SENA


# Função repet ------------------------------------------------------------####
  # Esta função atribui 0 a linhas não repetidas e 1 a linhas repetidas.
repet <- function(x) {
  a <- numeric(length(x))
  for (i in 2:length(x)) {
    if(x[i] == x[i-1]) {a[i] = 1} else {a[i] = 0}
    rm(i)
  }
  print(a)
}

# Função regiao -----------------------------------------------------------####

  # Esta função padroniza estados, municipios, regiões e seus códigos para fa-
  # cilitar a plotagem ou visualização dos dados.
regiao <- function(locais = "tabela de locais",
                   guia   = "estado, UF, cod.estado, regiao, cod.regiao",
                   des    = "dados desejados") {
  
  estado <- data.frame(estado     = c("RONDÔNIA", "ACRE", "AMAZONAS", "RORAIMA",
                                      "PARÁ", "AMAPÁ", "TOCANTINS", "MARANHÃO",
                                      "PIAUÍ", "CEARÁ", "RIO GRANDE DO NORTE",
                                      "PARAÍBA", "PERNAMBUCO", "ALAGOAS",
                                      "SERGIPE", "BAHIA", "MINAS GERAIS",
                                      "ESPÍRITO SANTO", "RIO DE JANEIRO",
                                      "SÃO PAULO", "PARANÁ", "SANTA CATARINA",
                                      "MATO GROSSO DO SUL", "MATO GROSSO",
                                      "GOIÁS", "DISTRITO FEDERAL",
                                      "RIO GRANDE DO SUL"),
                       UF         = c("RO", "AC", "AM", "RR", "PA", "AP", "TO",
                                      "MA", "PI", "CE", "RN", "PB", "PE", "AL",
                                      "SE", "BA", "MG", "ES", "RJ", "SP", "PR",
                                      "SC", "MS", "MT", "GO", "DF", "RS"),
                       cod.estado = c(11, 12, 13, 14, 15, 16, 17, 21, 22, 23,
                                      24, 25, 26, 27, 28, 29, 31, 32, 33, 35,
                                      41, 42, 50, 51, 52, 53, 43),
                       regiao     = c(rep("Norte", 7), rep("Nordeste", 9),
                                      rep("Sudeste",4), rep("Sul", 2),
                                      rep("Centro-Oeste", 4),"Sul"),
                       cod.regiao   = c(rep(1, 7), rep(2, 9), rep(3, 4),
                                        rep(4, 2), rep(5, 4), 4))
  
  if (guia == "estado" | guia == "1") {
    guia = estado$estado
  } else if (guia == "UF" | guia == "2") {
    guia = estado$UF
  } else if (guia == "cod.estado" | guia == "3") {
    guia = estado$cod.estado
  } else if (guia == "regiao" | guia == 4) {
    guia = estado$regiao
  } else {
    guia = estado$cod.regiao
  }
  
  if (des == "estado" | des == 1) {
    des = estado$estado
  } else if (des == "UF" | des == 2) {
    des = estado$UF
  } else if (des == "cod.estado" | des == 3) {
    des = estado$cod.estado
  } else if (des == "regiao" | des == 4) {
    des = estado$regiao
  } else {
    des = estado$cod.regiao
  }
  
  a = numeric(length(locais))
  for (i in 1:length(locais)) {
    if (locais[i] == "MATO GROSSO" | locais[i] == "Mato Grosso") {
      a[i] = as.character(des[grep(locais[i], guia, ignore.case = T)[2]])
    } else {
      a[i] = as.character(des[grep(locais[i], guia, ignore.case = T)[1]])
    }
    rm(i)
  }
  print(a)
}

# Função crmega -----------------------------------------------------------####

  # Função de correção monetária. Não tenho certeza se esta função é feita ape-
  # nas para a tabela da mega sena.
crmega <- function(nominal,
                   mes,
                   cod.mes,
                   indice,
                   base) {
  a <- data.frame(mes = numeric(length(nominal)),
                  valor <- numeric(length(nominal)))
  for (i in 1:length(nominal)) {
    a[i, 1] <- as.character(mes[i])
    a[i, 2] <- nominal[i] * (indice[cod.mes == base] / indice[cod.mes == as.character(mes[i])])
  }
  print(a)
}