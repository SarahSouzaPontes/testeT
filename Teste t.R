# Repositório
getwd()


# Pacotes instalados e carregados
install.packages("dplyr")
install.packages('nycflights13')
library('ggplot2')
library('dplyr')
library('nycflights13')

#Visualizando banco
View(flights)

#Dicionário de Dados
?flights

# Definindo o Problema de Negócio
# Teste de hipótese Verificar vôos Delta Airlines (DL) x UA (United Airlines) --> Atraso 

#Verificando e omitindo Missing
is.na(flights)
pop_data = na.omit(flights) %>% 
  
#Filtro para uma coluna apenas por CIA aérea:
  filter(carrier == 'UA' | carrier == 'DL', arr_delay >= 0) %>%
  select(carrier, arr_delay) %>%
  group_by(carrier) %>%
  
#Amostragem
  sample_n(17000) %>%
  
#Agrupando
  ungroup()

#Visualizando dataset
View(pop_data)

# Incluindo coluna: sample_id preenchida com número 1 para a primeira (amostra)
# 2 para a segunda amostra
#Omitindo missing
amostra1 = na.omit(pop_data) %>% 
  select(carrier, arr_delay) %>%
  filter(carrier == 'DL') %>%
  mutate(sample_id = '1') %>%
  
#Amostragem para 1000
  sample_n(1000)

#Visualizando amostra 1
View(amostra1)

#Amostragem 2
amostra2 = na.omit(pop_data) %>% 
  select(carrier, arr_delay) %>%
  filter(carrier == 'UA') %>%
  mutate(sample_id = '2') %>%
  sample_n(1000)

#Visualizando amostragem 2
View(amostra2)


# Unindo amostras pela linha
samples = rbind(amostra1,amostra2)
View(samples)

# Erro padrão
erro_padrao_amostra1 = sd(amostra1$arr_delay) / sqrt(nrow(amostra1))

# Limites inferior e superior
# 1.96 é o valor de z score para 95% de confiança
lower = mean(amostra1$arr_delay) - 1.96 * erro_padrao_amostra1  
upper = mean(amostra1$arr_delay) + 1.96 * erro_padrao_amostra1

# Intervalo de confiança
ic_1 = c(lower,upper)
mean(amostra1$arr_delay)
#Visualizando
ic_1

# Intervalo de confiança (95%) da amostra2
erro_padrao_amostra2 = sd(amostra2$arr_delay) / sqrt(nrow(amostra2))
lower = mean(amostra2$arr_delay) - 1.96 * erro_padrao_amostra2
upper = mean(amostra2$arr_delay) + 1.96 * erro_padrao_amostra2
ic_2 = c(lower,upper)
mean(amostra2$arr_delay)
ic_2


# Visualizando IC gráfica
toPlot = summarise(group_by(samples, sample_id), mean = mean(arr_delay))
toPlot = mutate(toPlot, lower = ifelse(toPlot$sample_id == 1,ic_1[1],ic_2[1]))
toPlot = mutate(toPlot, upper = ifelse(toPlot$sample_id == 1,ic_1[2],ic_2[2]))
ggplot(toPlot, aes(x = sample_id, y=mean, colour = sample_id )) + 
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1)


#TESTE DE HIPOTESE __________________________________________________
# H0 = Não há diferença estatisticamente significativa entre os atrasos da DL e UA.
# H1 = Delta atrasa mais.

#Criando as 2 subamostras (Apenas dado da X e arr_delay for maior que zero, 1000 observações)
dl <- sample_n(filter(pop_data, carrier == "DL", arr_delay > 0), 1000)
ua <- sample_n(filter(pop_data, carrier == "UA", arr_delay > 0), 1000)

# Calcula erro padrão e média
se = sd(dl$arr_delay) / sqrt(nrow(dl))
mean(dl$arr_delay)
#Média 37.64 da DL - Amostragem aleatória a cada execução pode apresentar valores distintos

# Limites inferior e superior - IC p/ DL
lower = mean(dl$arr_delay) - 1.96 * se
upper = mean(dl$arr_delay) + 1.96 * se
ic_dl = c(lower,upper)
ic_dl
# IC ==> 33.89704 41.38296

# Outra CIA repetindo:
se = sd(ua$arr_delay) / sqrt(nrow(ua))
mean(ua$arr_delay)

lower = mean(ua$arr_delay) - 1.96 * se
upper = mean(ua$arr_delay) + 1.96 * se
ic_ua = c(lower,upper)
ic_ua
#Média 37.013
# IC ==> 34.02277 40.00323


#--------Teste T Student --------------------------------------------
#2 amostras - atraso da DL e atraso da UA. Calcular as médias e aplicar teste T.
#greater --> Maior
t.test(dl$arr_delay, ua$arr_delay, alternative="greater")

#DADOS: data:  dl$arr_delay and ua$arr_delay
# VALOR DE t = 0.25652,  GRAUS DE LIBERDADE df = 1905.1, SIGNIFICANCIA p-value = 0.3988
# HIPOTESE ALTERNATIVA:alternative hypothesis: true difference in means is greater than 0
#95 percent confidence interval
# MÉDIAS CALCULADAS PELO TESTE T 37.640    37.013 

#####Conclusão: valor p é a quantificação da probrabilidade, sendo assim esse sendo 0,126
# alfa = 0,05 IC = 95%
# p= 0,126
#FALHA EM REJEITAR A HIPOTESE NULA, MAIOR QUE O NÍVEL DE SIGNIFICÂNCIA.
# NÃO HÁ EVIDÊNCIA ESTATÍSTICA QUE DL E UA  ATRASE MAIS QUE A OUTRA.

