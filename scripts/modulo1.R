
# Curso de R 
# Mod. 1 - Conceitos básicos 
# Desenvolvido por Daniel do Prado Pagotto 

# Os "#" representam um comentário. O R pula as linhas que tiverem esse #
# Para rodar cada linha, coloque o cursor sobre a linha do comando e 
# aperte ctrl + Enter (ou cmd + Enter, para o usuários de Mac)

# Os pacotes abaixo serão usados ao longo da nossa aula. Caso não tenha eles 
# instalados no seu R, é necessário instalar usando os comandos abaixo: 
# 1) retire os # da frente de cada linha; 2) rode cada comando (ctrl + Enter)

# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("skimr")
# install.packages("forcats")
# install.packages("gapminder")
# install.packages("DT")
# install.packages("plotly")

library(dplyr)
library(ggplot2)
library(skimr)
library(forcats)
library(gapminder)

# Parte 1 - Operacoes, atribuicoes e objetos ------------------------------

# Operações básicas

5 + 5
10 - 6
10*2
5/2
5**2
sqrt(16)
5*(50-45)

#Atribuições

x <- 5 + 5
y <- 10 - 16
a <- 9
soma <- a + x
nome <- "daniel"
certo <- TRUE

pesoDaniel <- 79
alturaDaniel <- 1.78
imcDaniel <- pesoDaniel/alturaDaniel**2


# Trabalhando com vetores
pesos <- c(65, 95, 75, 77, 80, 68)
alturas <- c(1.60, 1.78, 1.80, 1.68, 1.72, 1.65)
imc <- pesos/alturas**2
imc

help(round)
round(imc, 2)

imc <- round(imc, 2) #estou sobrescrevendo um vetor
                     # arredondado sobre ele mesmo
imc

# Trabalhando com matrizes 
Matriz<-cbind(pesos,alturas,imc)
Matriz

rownames(Matriz) <- c("Alice","Gilmar","Cecilia","Bianca","Valentina","Augusto")
Matriz


# Manipulação de dados  ---------------------------------------------------

basePaises <- gapminder

# inspecionando a estrutura da base
str(basePaises)

# inspecionando as 6 primeiras observações 
head(basePaises)

# inspecionando as 10 últimas observações
tail(basePaises, n = 10)

# estatísticas descritivas da base
summary(basePaises)

# Acessando uma variável da base
basePaises$continent

# Acessando elementos únicos
unique(basePaises$year)

# Média de um vetor
mean(basePaises$lifeExp)


# O pacote dplyr ----------------------------------------------------------

glimpse(basePaises)

# Filter 

basePaises %>% 
  filter(continent == "Asia")

basePaises%>%
  filter(continent == "Americas" & year>1990)

# != diferente 
basePaises%>%
  filter(continent != "Oceania")

# Você pode armazenar sua consulta em outro objeto
baseAsia <- basePaises%>%
  filter(continent == "Asia")

# Select 
basePaises%>%
  select(year,country,gdpPercap)

basePaises%>%
  select(-lifeExp)

basePaises %>%
  filter(continent == "Americas" & year>1990) %>%
  select(year,country,gdpPercap)

# Mutate 

basePaises <- basePaises %>%
  mutate(GDP = gdpPercap * pop)

basePorte <- basePaises %>%
  filter(year == 1992) %>%
  mutate(porte = if_else(pop>median(pop),"G","P"))

head(basePorte)

# Group_by + summarize 

basePaises %>%
  group_by(country) %>%
  summarize(meanLE=mean(lifeExp),meanPop=mean(pop),
            meanGpc=mean(gdpPercap))


basePaises %>%
  group_by(continent,year) %>%
  summarize(meanLE=mean(lifeExp),meanPop=mean(pop),
            meanGpc=mean(gdpPercap))

# Top_n + arrange 

basePaises %>% 
  filter(year == 2007) %>% 
  top_n(5,pop) %>% 
  arrange(desc(pop))

# Joins 

# lendo a base munic a partir do meu repositorio do Github
munic <- read.csv("https://raw.githubusercontent.com/danielppagotto/R_empreendedorismo1/main/arquivos%20de%20bases/politicas_empreendedorismo.csv",
                  sep = ";", encoding = "UTF-8")


# Visualização de dados ---------------------------------------------------

# base usada
base <- gapminder %>% 
  filter(year == 2007) 

glimpse(base)

# Análise descritiva 
skimr::skim(base)

# Histograma - Rbase 
hist(base$lifeExp)

# Histograma - ggplot 
ggplot(base, aes(x = lifeExp)) + geom_histogram()

# Densidade - ggplot 
base %>% 
  ggplot(aes(x = lifeExp)) + geom_density()

# Boxplot 
base %>% 
  ggplot(aes(y = lifeExp)) + geom_boxplot()

base %>% 
  ggplot(aes(x = continent, y = lifeExp)) + geom_boxplot()

# Coluna 
base %>% 
  filter(continent == "Asia") %>% 
  top_n(n = 10, wt = lifeExp) %>% 
  ggplot(aes(x = country, y = lifeExp)) + geom_col()

base %>% 
  filter(continent == "Asia") %>% 
  top_n(n = 10, wt = lifeExp) %>% 
  ggplot(aes(x = country, y = lifeExp)) + geom_col() + coord_flip()

base %>% 
  filter(continent == "Asia") %>% 
  top_n(n = 10, wt = lifeExp) %>% 
  ggplot(aes(x = fct_reorder(country,lifeExp), y = lifeExp)) + geom_col() + coord_flip()

base %>% 
  filter(continent == "Asia") %>% 
  top_n(n = 10, wt = lifeExp) %>% 
  ggplot(aes(x = fct_reorder(country,lifeExp), y = lifeExp)) + geom_col() + 
  geom_label(aes(label=round(lifeExp))) +  coord_flip()

gapminder %>% 
  filter(country == "Brazil") %>% 
  ggplot(aes(x = year, y = lifeExp)) + geom_line()

paises <- c("Brazil","Argentina")

gapminder %>% 
  filter(country %in% paises) %>% 
  ggplot(aes(x = year, y = lifeExp, col = country)) + geom_line()

base %>% 
  top_n(10, gdpPercap) %>% 
  ggplot(aes(x = fct_reorder(country,gdpPercap), 
             y=gdpPercap, fill = continent)) + 
  geom_col() + coord_flip()

ggplot(base,aes(x=lifeExp)) + geom_density(fill="darkblue") +
  labs(title = "Histograma da expectativa de vida", 
       x = "Expectativa de vida", y = "Frequência") + theme_minimal()

ggplot(base,aes(x=log(gdpPercap),y=lifeExp)) + geom_point() +
  labs(x = "PIB per capita (log)", y = "Expectativa de vida") + theme_bw()

ggplot(base,aes(y=lifeExp, x=log(gdpPercap))) + geom_point() +
  geom_smooth(method = "lm", se=FALSE) + 
  labs(x = "PIB per capita (log)",
       y = "Expectativa de vida") + theme_bw()

ggplot(base,aes(y=lifeExp, x=log(gdpPercap), col=continent)) + geom_point() +
  geom_smooth(method = "lm", se=FALSE) + 
  labs(x = "PIB per capita (log)",
       y = "Expectativa de vida") + theme_bw()

ggplot(base,aes(y=lifeExp, x=log(gdpPercap), col=continent,size = pop)) + geom_point() +
  geom_smooth(method = "lm", se=FALSE) + 
  labs(x = "PIB per capta (log)",
       y = "Expectativa de vida") + theme_bw()

ggplot(base,aes(x=log(gdpPercap), y=lifeExp, col=continent,
                size = pop)) + geom_point() + geom_smooth(method = "lm", se=FALSE) + 
  labs(x = "PIB per capita (log)", y = "Expectativa de vida") + theme_bw() + 
  facet_grid(~continent)

# Extra datatable 
library(DT)

DT::datatable(base, options = list(pageLength = 5), class = 'cell-border stripe')

# Extra esquisse 
library(esquisse)

esquisser(viewer = "browser")

# Extra plotly 

library(plotly)

ggplot(base,aes(y=lifeExp, x=log(gdpPercap), col=continent)) + geom_point() +
  labs(x = "PIB per capita (log)",
       y = "Expectativa de vida") + theme_bw()

grafico <- ggplot(base,aes(y=lifeExp, x=log(gdpPercap), col=continent)) + geom_point() +
  labs(x = "PIB per capita (log)",
       y = "Expectativa de vida") + theme_bw()

ggplotly(grafico)

