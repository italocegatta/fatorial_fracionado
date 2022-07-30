
# pacotes -----------------------------------------------------------------

library(tidyverse)
library(ggrepel)

# Exemplo 14-9, Montgomery (EAPE): encolhimento excessivo em peças de moldagem por injeção. 
# Analisando 6 fatores em um experimento fatorial fracionado $2^{6-2}$, 
# foram realizadas 16 corridas para estes seis fatores. O planejamento foi construído 
# escrevendo um planejamento básico $2^4$ nos fatores $A$, $B$, $C$, e $D$, 
# e então estabelecendo $E = ABC$, e $F = BCD$, pois as relações de definição escolhidas
# foram $I = ABCE$, e $I = BCDF$. O objetivo do planejamento era identificar os fatores
# que causavam o encolhimento excessivo, portanto as combinações de fatores que gerem 
# **menores** valores da variável resposta (encolhimento) deverão ser utilizadas no processo 
# de fabricação. 


# input -------------------------------------------------------------------

tab_dados <- read_csv2("dados/exemplo_fr6-2.csv")

tab_matriz_completa <- model.matrix(~A * B * C * D * E * F, data = tab_dados)


# confundimentos ----------------------------------------------------------

vec_efeitos <- attr(tab_matriz_completa, "dimnames")[[2]]

lst_out <- vector("list", length = length(vec_efeitos))

for (i in seq_along(vec_efeitos)) {
  
  names(lst_out)[i] <- vec_efeitos[i]
  
  i_efeito <- tab_matriz_completa %>% 
    as_tibble() %>% 
    pull(!!i)
  
  lst_out[i][[1]] <- tab_matriz_completa %>% 
    as_tibble() %>% 
    select_if(~ all(. == i_efeito))


}

head(lst_out)


# matriz ------------------------------------------------------------------

tab_matriz_frac <- tab_matriz_completa %>% 
  unique(MARGIN = 2) %>% 
  as_tibble()

tab_matriz_frac


# contrastes --------------------------------------------------------------

mat_contrastes <- t(tab_matriz_frac[, -1]) %*% tab_dados$y

k <- 4
r <- 1
mat_efeitos <- {mat_contrastes/(r * 2^(k - 1))} %>% 
  as_tibble(rownames = "efeito") 


# qqplot ------------------------------------------------------------------

mat_efeitos %>% 
  ggplot(aes(sample = V1)) +
  geom_qq() + 
  geom_text_repel(label = arrange(mat_efeitos, V1)$efeito, stat="qq") +
  stat_qq_line() +
  theme_bw()


# anova -------------------------------------------------------------------

m0 <- lm(y ~ A + B + C + D + A:B + A:D + A:C:D, data = tab_dados)
anova(m0)

m1 <- lm(y ~ A + B + A:B, data = tab_dados)
anova(m1)

anova(m1, m0)


# residuos ----------------------------------------------------------------

res <- residuals(m0)

residuals(m0) %>% 
  as_tibble() %>% 
  ggplot(aes(sample = value)) +
  geom_qq() + 
  stat_qq_line() +
  theme_bw()


# Esse exemplo ilustra a importância do fatorial fracionado. Se tivessemos
# feito o experimento $2^6 = 64$ corridas, poderíamos estimar interações
# de ordens altas, mas com a fração $1/4$, de 16 corridas, verificamos que
# só 2 efeitos principais, duas interações de segunda ordem e uma de
# terceira ordem são importantes. Com economia, selecionamos de 6
# variáveis, apenas 2 principais, que são importantes, e que agora serão
# investigadas com maior rigor em experimento mais elaborados
# subsequentes. O fundamento do planejamento de experimentos é exatamente
# esse: obter maior informação possível, tirar o máximo de vantagem e da
# forma mais econômica, ou seja, ser eficiente na investigação.