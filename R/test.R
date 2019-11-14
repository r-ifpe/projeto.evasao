library(dplyr)
questionario <- read.csv("extdata/Seg_Trab_2019.csv")
qacademico <- openxlsx::read.xlsx("extdata/Q_Academico_2009_1 a 2019_1.xlsx", 1) %>%
sistec <- openxlsx::read.xlsx("extdata/sistec.xlsx")

qacademico
Cpf, Matricula, Desc_turno, Sexo_1, Coeficiente_Rendimento

sistec
Atestado.Baixa.Renda, Dt.Nascimento.Aluno

stringr::str_remove_all(a$Matricula.ou.CPF, "[.-]") %>%
  stringr::str_to_upper() %>%
  stringr::str_length()


#' @importFrom lubridate decimal_date dmy
idade <- function(data_nasc){
  idade <- decimal_date(ymd_hms(Sys.time())) - decimal_date(dmy(data_nasc))
  floor(idade)
}

# dados relevantes do sistec
a <- sistec %>%
  transmute(Idade = idade(Dt.Nascimento.Aluno),
            `Baixa Renda` = Atestado.Baixa.Renda,
            Cpf = Numero.Cpf) %>%
  filter(stringr::str_length(Cpf) == 11)


# dados relevantes do qacademico
b <- qacademico %>%
  transmute(Cpf = stringr::str_remove_all(Cpf, "[.-]"),
            Matricula =  stringr::str_remove_all(Matricula, "[-]"),
            Turno = Desc_Turno, Sexo = Sexo_1,
            `Coeficiente de Redimento` = Coeficiente_Rendimento)

#dados relevantes do questionario
q <- questionario %>%
  dplyr::transmute(cpf_mat = cpf_mat_tidy(Matricula.ou.CPF),
                   Dificuldade = Qual.a.sua.principal.dificuldade.aqui.no.IFPE.) %>%
  dplyr::group_split(stringr::str_detect(cpf_mat, "[A-Z]"), keep = FALSE)

ifpe_dados <- dplyr::inner_join(b, a , by = "Cpf")

q_cpf <- q[[1]] %>%
  inner_join(ifpe_dados, by = "Cpf")
q_mat <- q[[2]]


cpf_mat_tidy <- function(x){
  stringr::str_remove_all(x, "[.-]") %>%
  stringr::str_to_upper()
}

