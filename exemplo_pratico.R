library(tidyverse)
library(ggrepel)

tab_dados <- read_csv2("dados/exemplo_fr6-2.csv")

tab_matriz_completa <- model.matrix(~A * B * C * D * E * F, data = tab_dados)

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

tab_matriz_frac <- tab_matriz_completa %>% 
  unique(MARGIN = 2) %>% 
  as_tibble()

mat_contrastes <- t(tab_matriz_frac[, -1]) %*% tab_dados$y


k <- 4
r <- 1
mat_efeitos <- {mat_contrastes/(r * 2^(k - 1))} %>% 
  as_tibble(rownames = "efeito") 


@Arun has a good solution in the comment above, but this works with R 4.0.3:
  
  ggplot(data = df, aes(sample = x)) + geom_qq() + geom_text_repel(label=df$name[order(df$x)], stat="qq") + stat_qq_line()

mat_efeitos %>% 
  ggplot(aes(sample = V1)) +
  geom_qq() + 
  geom_text_repel(label = arrange(mat_efeitos, V1)$efeito, stat="qq") +
  stat_qq_line() +
  theme_bw()

m0 <- lm(y ~ A + B + C + D + A:B + A:D + A:C:D, data = dados)
anova(m0)

m1 <- lm(y ~ A + B + A:B, data = dados)
anova(m1)

anova(m1, m0)

## Residuos do modelo
res <- residuals(m0)
## Quantis normais

residuals(m0) %>% 
  as_tibble() %>% 
  ggplot(aes(sample = value)) +
  geom_qq() + 
  stat_qq_line() +
  theme_bw()

