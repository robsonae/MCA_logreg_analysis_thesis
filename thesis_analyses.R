library(FactoMineR)
library(explor)
library(tidyverse)
library(dichromat)

df <- read.table(file.choose(), header = T, sep = "\t", stringsAsFactors = T)

summary(df)
MCA1 <- MCA(df, graph = F)
summary(MCAFac)
explor(MCAFac)

# version of the data with: analyse the dataset logically and figure out which subsets of data would be best to analyse
# one without Entity concerned - too codependent with agency ent conc and we need it to test the hypothesis.

# df without: Entity concerned category, Fairness Non-applicable level, Language
df2 <- df %>% 
  select(-Entity_concerned, -Language) %>%
  filter(Fairness != "Non-applicable")
MCA2 <- MCA(df2, graph = F)
MCA_var_plot(MCA)
explor(MCA2)

unique(dff$Agency.FATE.)

# df without: Entity concerned category, Fairness cateogry
df3 <- df %>% 
  select(-Entity_concerned, -Language) %>% 
  select(-Fairness)
MCA3 <- MCA(df3, graph = F)
explor(MCA3)

# df without: Entity concerned category (Fairness retained, 1 co-dependency present), Language (cant be there due to dependence on lexemes)

df4 <- df %>% 
  select(-Entity_concerned)
MCA4 <- MCA(df4, graph = F)
explor(MCA4)





#---------------------------- BINARY LOGISTIC REGRESSION -----------------------

library(rms)
library(MASS)
df <- read.table(file.choose(), header=T, sep="\t", stringsAsFactors = T)
summary(df)


# All predictors - significant is the entity concerned and metaphor is almost significant
M = lrm(formula = Language ~ Metaphor +
          Determinism +
          Entity_concerned +
          Agency.Entity_concerned. +
          Agency.FATE. +
          Fairness +
          Sentiment,
        data = df,
        x = T, y = T)
summary(M)
M
# ALl predictors without Agency.ent conc - its collinear with entity conc and we want to keep the latter
M = lrm(formula = Language ~
          Determinism +
          Entity_concerned,
        data = df,
        x = T, y = T)

L = lrm(formula = Language ~ Metaphor +
          Determinism +
          Entity_concerned +
          Agency.FATE. +
          Fairness +
          Sentiment, 
        data = df,
        x = T, y = T)
L


library(tidyverse)
df_en <- read.table(file.choose(), header = T, sep = "\t", stringsAsFactors = T)
df_pl <- read.table(file.choose(), header = T, sep = "\t", stringsAsFactors = T)


PL <- glm(formula = Lexeme ~ 
            Determinism +
            Agency.Entity_concerned. +
            Sentiment,
          data = df_pl,
          family = "binomial")
summary(PL)

PLL <- lrm(formula = Lexeme ~ Metaphor +
            Determinism +
            Agency.Entity_concerned. +
            Agency.FATE. +
            Sentiment,
          data = df_pl,
          x = T, y = T)
PLL







