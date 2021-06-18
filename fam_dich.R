library(tidyverse)
library(TAM)
library(WrightMap)
library(eRm)
library(psych)
library(gridExtra)
library(grid)

rm(list = ls())

df <- read_csv("familiarity_r.csv")

### Model 1 ###
  
  j=1 # Check no ceiling scores on rows
  for(j in j:nrow(df)){
    x <- sum(df[j,2:ncol(df)])
    if(x == ncol(df)){
      df <- df[-j,]
    }
    j=j+1
  }
  
  j=1 # Check no zero scores on rows
  for(j in j:nrow(df)){
    x <- sum(df[j,2:ncol(df)])
    if(x < 1){
      print(df[j,1])
      df <- df[-j,]
    }
    j=j+1
  }
  
  k=2 # Check no ceiling scores on cols
  for(k in k:ncol(df)){
    y <- sum(df[,k])
    if(y == nrow(df)){
      df <- df[,-k]
    }
    k=k+1
  }
  
  k=2 # Check no zero scores on cols
  for(k in k:(ncol(df)-1)){
    y <- sum(df[,k])
    if(y == 0){
      print(colnames(df[,k]))
      df <- df[,-k]
    }
    k=k+1
  }

model1 <- tam.mml(df[,2:ncol(df)])
logits <- model1[["item_irt"]]

i.fit <- msq.itemfit(model1)$itemfit
subset(i.fit, i.fit$Outfit > 1.5)
subset(i.fit, i.fit$Infit > 1.5)
subset(i.fit, i.fit$Outfit_t > 2)
subset(i.fit, i.fit$Infit_t > 2)

p.fit <- data.frame(df$person, tam.personfit(model1))
subset(p.fit, p.fit$outfitPerson > 1.5)[,c(1,2,4)]
subset(p.fit, p.fit$infitPerson > 1.5)[,1]
subset(p.fit, p.fit$outfitPerson_t > 2)[,1]
subset(p.fit, p.fit$infitPerson_t > 2)[,1]

# 27 oufit MNSQ > 1.5 were examined and 14 persons deemed detrimental to model were removed 

p_elim <- c("a001", "a024", "a025", "a031", "a059", "a065", "a087", "a116", "a122", "a153", "a180", "b219", "b250")

i=1
for(i in i:length(p_elim)){
  df <- subset(df, df$person!=p_elim[i])
  i=i+1
}

# 3 items reached 0 and were removed

df <- subset(df, select = -c(Bledsoe, Drummle, Wopsle))


### Model 2 ###

model2 <- tam.mml(df[,2:ncol(df)])
logits <- model2[["item_irt"]]

# Item fit
i.fit <- msq.itemfit(model2)$itemfit
subset(i.fit, i.fit$Outfit > 1.5)
subset(i.fit, i.fit$Infit > 1.5)
subset(i.fit, i.fit$Outfit_t > 2)
subset(i.fit, i.fit$Infit_t > 2)

# Person fit
p.fit <- data.frame(df$person, tam.personfit(model2))
subset(p.fit, p.fit$outfitPerson > 1.5)[,c(1,2,4)]
subset(p.fit, p.fit$infitPerson > 1.5)[,c(1,2,4)]
subset(p.fit, p.fit$outfitPerson_t > 2)[,1]
subset(p.fit, p.fit$infitPerson_t > 2)[,1]

# 11 oufit MNSQ > 1.5 were examined and 5 persons deemed detrimental to model were removed 

p_elim <- c("a010", "a070", "a120", "a160", "b249")

i=1
for(i in i:length(p_elim)){
  df <- subset(df, df$person!=p_elim[i])
  i=i+1
}

# 1 item reached 0 and was removed

df <- subset(df, select = -c(Fosco))


### Model 3 ###

model3 <- tam.mml(df[,2:ncol(df)])
logits <- model3[["item_irt"]]

# Item fit
i.fit <- msq.itemfit(model3)$itemfit
subset(i.fit, i.fit$Outfit > 1.5)
subset(i.fit, i.fit$Infit > 1.5)
subset(i.fit, i.fit$Outfit_t > 2)
subset(i.fit, i.fit$Infit_t > 2)

# Person fit
p.fit <- data.frame(df$person, tam.personfit(model3))
subset(p.fit, p.fit$outfitPerson > 1.5)[,c(1,2,4)]
subset(p.fit, p.fit$infitPerson > 1.5)[,c(1,2,4)]
subset(p.fit, p.fit$outfitPerson_t > 2)[,1]
subset(p.fit, p.fit$infitPerson_t > 2)[,1]

# 9 oufit & infit MNSQ > 1.5 were examined and 6 persons deemed detrimental to model were removed 

p_elim <- c("a003", "a011", "a171", "a174", "b227", "b228")

i=1
for(i in i:length(p_elim)){
  df <- subset(df, df$person!=p_elim[i])
  i=i+1
}

# 1 item reached 0 and was removed

df <- subset(df, select = -c(Murray))


### Model 4 ###

model4 <- tam.mml(df[,2:ncol(df)])
logits <- model4[["item_irt"]]

# Item fit
i.fit <- msq.itemfit(model4)$itemfit
subset(i.fit, i.fit$Outfit > 1.5)
subset(i.fit, i.fit$Infit > 1.5)
subset(i.fit, i.fit$Outfit_t > 2)
subset(i.fit, i.fit$Infit_t > 2)

# Person fit
p.fit <- data.frame(df$person, tam.personfit(model4))
subset(p.fit, p.fit$outfitPerson > 1.5)[,c(1,2,4)]
subset(p.fit, p.fit$infitPerson > 1.5)[,c(1,2,4)]
subset(p.fit, p.fit$outfitPerson_t > 2)[,1]
subset(p.fit, p.fit$infitPerson_t > 2)[,1]

# 5 oufit & infit MNSQ > 1.5 were examined and 2 persons deemed detrimental to model were removed 

p_elim <- c("a084", "a158")

i=1
for(i in i:length(p_elim)){
  df <- subset(df, df$person!=p_elim[i])
  i=i+1
}

# 1 item reached 0 and was removed

df <- subset(df, select = -c(Heaslop, Haigha))


### Model 5 ###

model5 <- tam.mml(df[,2:ncol(df)])
logits <- model5[["item_irt"]]

# Item fit
i.fit <- msq.itemfit(model5)$itemfit
subset(i.fit, i.fit$Outfit > 1.5)
subset(i.fit, i.fit$Infit > 1.5)
subset(i.fit, i.fit$Outfit_t > 2)
subset(i.fit, i.fit$Infit_t > 2)

# Person fit
p.fit <- data.frame(df$person, tam.personfit(model5))
subset(p.fit, p.fit$outfitPerson > 1.5)[,c(1,2,4)]
subset(p.fit, p.fit$infitPerson > 1.5)[,c(1,2,4)]
subset(p.fit, p.fit$outfitPerson_t > 2)[,1]
subset(p.fit, p.fit$infitPerson_t > 2)[,1]

# 1 oufit & infit MNSQ > 1.5 was examined and deemed detrimental to model were removed 

p_elim <- c("a002")

i=1
for(i in i:length(p_elim)){
  df <- subset(df, df$person!=p_elim[i])
  i=i+1
}


### Model 6 ###

model6 <- tam.mml(df[,2:ncol(df)])
logits <- model6[["item_irt"]]

# Item fit
i.fit <- msq.itemfit(model6)$itemfit
subset(i.fit, i.fit$Outfit > 1.5)
subset(i.fit, i.fit$Infit > 1.5)
subset(i.fit, i.fit$Outfit_t > 2)
subset(i.fit, i.fit$Infit_t > 2)

# Person fit
p.fit <- data.frame(df$person, tam.personfit(model6))
subset(p.fit, p.fit$outfitPerson > 1.5)[,c(1,2,4)]
subset(p.fit, p.fit$infitPerson > 1.5)[,c(1,2,4)]
subset(p.fit, p.fit$outfitPerson_t > 2)[,1]
subset(p.fit, p.fit$infitPerson_t > 2)[,1]

# 1 oufit & infit MNSQ > 1.5 was examined and deemed detrimental to model were removed 

p_elim <- c("a002")

i=1
for(i in i:length(p_elim)){
  df <- subset(df, df$person!=p_elim[i])
  i=i+1
}


### Model 6 ###

model6 <- tam.mml(df[,2:ncol(df)])
logits <- model6[["item_irt"]]

# Item fit
i.fit <- msq.itemfit(model6)$itemfit
subset(i.fit, i.fit$Outfit > 1.5)
subset(i.fit, i.fit$Infit > 1.5)
subset(i.fit, i.fit$Outfit_t > 2)
subset(i.fit, i.fit$Infit_t > 2)

# Person fit
p.fit <- data.frame(df$person, tam.personfit(model6))
subset(p.fit, p.fit$outfitPerson > 1.5)[,c(1,2,4)]
subset(p.fit, p.fit$infitPerson > 1.5)[,c(1,2,4)]
subset(p.fit, p.fit$outfitPerson_t > 2)[,1]
subset(p.fit, p.fit$infitPerson_t > 2)[,1]

# 1 oufit & infit MNSQ > 1.5 was examined and deemed detrimental to model were removed 

p_elim <- c("a002")

i=1
for(i in i:length(p_elim)){
  df <- subset(df, df$person!=p_elim[i])
  i=i+1
}


### Model 6 ###

model6 <- tam.mml(df[,2:ncol(df)])
logits <- model6[["item_irt"]]

# Item fit
i.fit <- msq.itemfit(model6)$itemfit
subset(i.fit, i.fit$Outfit > 1.5)
subset(i.fit, i.fit$Infit > 1.5)
subset(i.fit, i.fit$Outfit_t > 2)
subset(i.fit, i.fit$Infit_t > 2)

# Person fit
p.fit <- data.frame(df$person, tam.personfit(model6))
subset(p.fit, p.fit$outfitPerson > 1.5)[,c(1,2,4)]
subset(p.fit, p.fit$infitPerson > 1.5)[,c(1,2,4)]
subset(p.fit, p.fit$outfitPerson_t > 2)[,1]
subset(p.fit, p.fit$infitPerson_t > 2)[,1]

# 1 oufit & infit MNSQ > 1.5 was examined and deemed detrimental to model were removed 

p_elim <- c("a111")

i=1
for(i in i:length(p_elim)){
  df <- subset(df, df$person!=p_elim[i])
  i=i+1
}


### Model 7 ###

model7 <- tam.mml(df[,2:ncol(df)])
logits <- model7[["item_irt"]]

# Item fit
i.fit <- msq.itemfit(model7)$itemfit
subset(i.fit, i.fit$Outfit > 1.5)
subset(i.fit, i.fit$Infit > 1.5)
subset(i.fit, i.fit$Outfit_t > 2)
subset(i.fit, i.fit$Infit_t > 2)

# Person fit
p.fit <- data.frame(df$person, tam.personfit(model7))
subset(p.fit, p.fit$outfitPerson > 1.5)[,c(1,2,4)]
subset(p.fit, p.fit$infitPerson > 1.5)[,c(1,2,4)]
subset(p.fit, p.fit$outfitPerson_t > 2)[,1]
subset(p.fit, p.fit$infitPerson_t > 2)[,1]

# 1 oufit & infit MNSQ > 1.5 was examined and deemed detrimental to model were removed 

p_elim <- c("a075")

# person outfitPerson infitPerson
# a075         1.55        1.16

i=1
for(i in i:length(p_elim)){
  df <- subset(df, df$person!=p_elim[i])
  i=i+1
}


### Model 8 ###

model8 <- tam.mml(df[,2:ncol(df)])
logits <- model8[["item_irt"]]

# Item fit
i.fit <- msq.itemfit(model8)$itemfit
subset(i.fit, i.fit$Outfit > 1.5)
subset(i.fit, i.fit$Infit > 1.5)
subset(i.fit, i.fit$Outfit_t > 2)
subset(i.fit, i.fit$Infit_t > 2)

# Person fit
p.fit <- data.frame(df$person, tam.personfit(model8))
subset(p.fit, p.fit$outfitPerson > 1.5)[,c(1,2,4)]
subset(p.fit, p.fit$infitPerson > 1.5)[,c(1,2,4)]
subset(p.fit, p.fit$outfitPerson_t > 2)[,1]
subset(p.fit, p.fit$infitPerson_t > 2)[,1]

# 1 oufit & infit MNSQ > 1.5 was examined and deemed detrimental to model were removed 

p_elim <- c("a204")

# person outfitPerson infitPerson
# a204         1.51        1.29

i=1
for(i in i:length(p_elim)){
  df <- subset(df, df$person!=p_elim[i])
  i=i+1
}


### Model 9 ###

model9 <- tam.mml(df[,2:ncol(df)])
logits <- model9[["item_irt"]]
i.se <- model9[["xsi"]]
logits$se <- i.se$se.xsi

# Item fit
i.fit <- msq.itemfit(model9)$itemfit
subset(i.fit, i.fit$Outfit > 1.5)
subset(i.fit, i.fit$Infit > 1.5)
subset(i.fit, i.fit$Outfit_t > 2)
subset(i.fit, i.fit$Infit_t > 2)

# Person fit
p.fit <- data.frame(df$person, tam.personfit(model9))
subset(p.fit, p.fit$outfitPerson > 1.5)[,c(1,2,4)]
subset(p.fit, p.fit$infitPerson > 1.5)[,c(1,2,4)]
subset(p.fit, p.fit$outfitPerson_t > 2)[,1]
subset(p.fit, p.fit$infitPerson_t > 2)[,1]

### Model 9 Plot

ggplot(data = logits, aes(x=beta, y=reorder(item, beta))) + geom_point() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_reverse() +
  theme_classic() + 
  geom_point(size = 2) 
#+ geom_text(aes(label=item),hjust=0, vjust=0)

### Distribution check
# The MML equation requires that the person parameters are normally distributed

df.t <- write_csv(data.table::transpose(df)[-1,], "test.csv") # Transpose data
test <- read_csv("test.csv")
model9.t <- tam.mml(test[,2:ncol(test)])
p.logits <- model9.t[["item_irt"]] # Items = Persons because dataframe was transposed
hist(p.logits$beta) # The distribution is normal

ggplot(p.logits, aes(x=beta)) + 
  geom_histogram(bins=12, colour = "black", fill = "darkslategray4", size = 0.1) + 
  stat_function(fun = function(x) dnorm(x, mean = mean(p.logits$beta), sd = sd(p.logits$beta)) * length(p.logits$beta)/2, color = "black", size = 1) +
  theme_classic() +
  labs(x="Logits", y = "Count")+
  theme(axis.title.x = element_text(size=16)) +                  
  theme(axis.title.y = element_text(size=16)) +                 
  theme(axis.text = element_text(size = 12)) 

### Model 9 Descriptives
# ITEMS
i.raw <- vector()
i=2
for(i in i:ncol(df)){
  i.raw <- append(i.raw, sum(df[,i]))
  i=i+1
}

describe(i.raw)
describe(logits$beta)
describe(logits$se)
describe(i.fit$Infit)
describe(i.fit$Outfit)
describe(i.fit$Infit_t)
describe(i.fit$Outfit_t)

# PERSONS
p.raw <- vector()
i=1
for(i in i:nrow(df)){
  p.raw <- append(p.raw, sum(as.numeric(df[i,])[-1]))
  i=i+1
}

p.se <- model9.t[["xsi"]]$se.xsi

describe(p.raw)
describe(p.logits$beta)
describe(p.se)
describe(p.fit$infitPerson)
describe(p.fit$outfitPerson)
describe(p.fit$infitPerson_t)
describe(p.fit$outfitPerson_t)

### Compare with eRm

erm_m9 <- RM(df[2:ncol(df)])
erm.i <- as.numeric(erm_m9[["betapar"]]) # Item logits

# Correlation
logits.r <- as.numeric(logits$beta*-1) # TAM logits revrse coded for ease of interprability
cor.test(logits.r, erm.i)
plot(logits.r, erm.i)
p_param <- person.parameter(erm_m9) #Person ability [logits]

# Boxplots
se_s <- as.numeric(logits$se)
se_s <- append(se_s, as.numeric(erm_m9[["se.beta"]]))
method <- c(rep("TAM", 92), rep("eRm", 92))
se_long <- data.frame(method, se_s)
colnames(se_long) <- c("Method", "SE")
bp1 <- ggplot(se_long, aes(x = Method, y = SE)) +
  geom_boxplot(outlier.shape = NA, outlier.size = 1, notch=FALSE) +
  geom_point(alpha = .05, size = .1) +  
  geom_jitter(position=position_jitter(0.2), size = 1.5) +
  labs(x="Method", y="Item SEs") +                        
  theme_bw() +
  theme(axis.title.x = element_text(size=16)) +                  
  theme(axis.title.y = element_text(size=16)) +                 
  theme(axis.text = element_text(size = 14)) +
  theme(legend.position = "none") +
  scale_x_discrete(labels= c("TAM", "eRm")) +
  geom_boxplot(fatten=2, fill = c("darkslategray4", "indianred3"), alpha = 0.8)
# Infit MNSQ boxplots
inf.mnsq <- as.numeric(i.fit$Infit) # TAM
inf.mnsq <- append(inf.mnsq, as.numeric(itemfit(p_param)$i.infitMSQ)) #eRm
inf.mnsq_long <- data.frame(method, inf.mnsq)
colnames(inf.mnsq_long) <- c("Method", "Inf.MNSQ")
bp2 <- ggplot(inf.mnsq_long, aes(x = Method, y = Inf.MNSQ)) +
  geom_boxplot(outlier.shape = NA, outlier.size = 1, notch=FALSE) +
  geom_point(alpha = .05, size = .1) +  
  geom_jitter(position=position_jitter(0.2), size = 1.5) +
  labs(x="Method", y="Infit (MNSQ)") +                        
  theme_bw() +
  theme(axis.title.x = element_text(size=16)) +                  
  theme(axis.title.y = element_text(size=16)) +                 
  theme(axis.text = element_text(size = 14)) +
  theme(legend.position = "none") +
  scale_x_discrete(labels= c("TAM", "eRm")) +
  geom_boxplot(fatten=2, fill = c("darkslategray4", "indianred3"), alpha = 0.8) +
  coord_cartesian(ylim = c(0, 1.50))
# Outfit MNSQ boxplots
out.mnsq <- as.numeric(i.fit$Outfit) # TAM
out.mnsq <- append(out.mnsq, as.numeric(itemfit(p_param)$i.outitMSQ)) #eRm
out.mnsq_long <- data.frame(method, out.mnsq)
colnames(out.mnsq_long) <- c("Method", "Out.MNSQ")
bp3 <- ggplot(out.mnsq_long, aes(x = Method, y = Out.MNSQ)) +
  geom_boxplot(outlier.shape = NA, outlier.size = 1, notch=FALSE) +
  geom_point(alpha = .05, size = .1) +  
  geom_jitter(position=position_jitter(0.2), size = 1.5) +
  labs(x="Method", y="Outfit (MNSQ)") +                        
  theme_bw() +
  theme(axis.title.x = element_text(size=16)) +                  
  theme(axis.title.y = element_text(size=16)) +                 
  theme(axis.text = element_text(size = 14)) +
  theme(legend.position = "none") +
  scale_x_discrete(labels= c("TAM", "eRm")) +
  geom_boxplot(fatten=2, fill = c("darkslategray4", "indianred3"), alpha = 0.8) +
  coord_cartesian(ylim = c(0, 1.50))
# Infit t boxplots
inf.t <- as.numeric(i.fit$Infit_t) # TAM
inf.t <- append(inf.t, as.numeric(itemfit(p_param)$i.infitZ)) #eRm
inf.t_long <- data.frame(method, inf.t)
colnames(inf.t_long) <- c("Method", "Inf.t")
bp4 <- ggplot(inf.t_long, aes(x = Method, y = Inf.t)) +
  geom_boxplot(outlier.shape = NA, outlier.size = 1, notch=FALSE) +
  geom_point(alpha = .05, size = .1) +  
  geom_jitter(position=position_jitter(0.2), size = 1.5) +
  labs(x="Method", y="Infit (t)") +                        
  theme_bw() +
  theme(axis.title.x = element_text(size=16)) +                  
  theme(axis.title.y = element_text(size=16)) +                 
  theme(axis.text = element_text(size = 14)) +
  theme(legend.position = "none") +
  scale_x_discrete(labels= c("TAM", "eRm")) +
  geom_boxplot(fatten=2, fill = c("darkslategray4", "indianred3"), alpha = 0.8) +
  coord_cartesian(ylim = c(-2.00, 2.00))
# Outfit t boxplots
out.t <- as.numeric(i.fit$Outfit_t) # TAM
out.t <- append(out.t, as.numeric(itemfit(p_param)$i.outfitZ)) #eRm
out.t_long <- data.frame(method, out.t)
colnames(out.t_long) <- c("Method", "Out.t")
bp5 <- ggplot(out.t_long, aes(x = Method, y = Out.t)) +
  geom_boxplot(outlier.shape = NA, outlier.size = 1, notch=FALSE) +
  geom_point(alpha = .05, size = .1) +  
  geom_jitter(position=position_jitter(0.2), size = 1.5) +
  labs(x="Method", y="Outfit (t)") +                        
  theme_bw() +
  theme(axis.title.x = element_text(size=16)) +                  
  theme(axis.title.y = element_text(size=16)) +                 
  theme(axis.text = element_text(size = 14)) +
  theme(legend.position = "none") +
  scale_x_discrete(labels= c("TAM", "eRm")) +
  geom_boxplot(fatten=2, fill = c("darkslategray4", "indianred3"), alpha = 0.8) +
  coord_cartesian(ylim = c(-2.00, 2.00))
# Join
grid.newpage() 
pushViewport(viewport(layout = grid.layout(3,2))) 
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y) 
print(bp1, vp = vplayout(1,1)) 
print(bp2, vp = vplayout(2,1))
print(bp3, vp = vplayout(2,2))
print(bp4, vp = vplayout(3,1))
print(bp5, vp = vplayout(3,2))
grid.text(c("a","b","c","d","e"), x=c(0.02, 0.02, 0.52, 0.02, 0.52), y=c(.99, .67, .67, .34, .34), gp = gpar(fontsize = 18))

# Fit Statistic check
items <- itemfit(p_param) # Item fit stats
i_out <- data.frame(names(df)[-1], items$i.outfitMSQ)
subset(i_out, i_out$items.i.outfitMSQ > 1.5) # Outfit MNSQ check = 0
i_inf <- data.frame(names(df)[-1], items$i.infitMSQ)
subset(i_inf, i_inf$items.i.infitMSQ > 1.5) # Infit MNSQ check = 0

persons <- personfit(p_param)
p_out <- data.frame(df$person, persons$p.outfitMSQ)
subset(p_out, p_out$persons.p.outfitMSQ > 1.5) # Outfit MNSQ check = 13
p_inf <- data.frame(df$person, persons$p.infitMSQ)
subset(p_inf, p_inf$persons.p.infitMSQ > 1.5) # Infit MNSQ check = 0

# The 13 persons displaying outfit MNSQ > 1.5 were individually checked.
# All of the participants misfit related to cleaning up the top-left and bottom-right corners of the Guttman map
# This indicates that the eRm misfit statistics are perhaps more conservative than the TAM equivalents

### Compare site A and site B

# TAM person parameter

p.logits$person <- df$person[-1]
p.logits$group <- as.factor(substr(p.logits$person, 1, nchar(p.logits$person)-3))

ggplot(p.logits, aes(x = group, y = beta)) +
  geom_boxplot(outlier.shape = NA, outlier.size = 1, notch=FALSE) +
  geom_point(alpha = .05, size = .1) +  
  geom_jitter(position=position_jitter(0.2), size = 1.5) +
  labs(x="Site", y="Person Logits (TAM)") +                        
  theme_bw() +
  theme(axis.title.x = element_text(size=16)) +                  
  theme(axis.title.y = element_text(size=16)) +                 
  theme(axis.text = element_text(size = 14)) +
  theme(legend.position = "none") +
  scale_x_discrete(labels= c("Site A", "Site B")) +
  geom_boxplot(fatten=2, fill = "darkslategray4", alpha = 0.8)

# eRm person parameters

# The p.logits() function provides the logit score and accompanying SE for each participant 
# It requires an object of class 'eRm'
# i.e., the output from the RM() function in the 'eRm' package
# d = data
p.logits <- function(eRm,d){
  d = data.frame(d)
  score <- vector()
  i=1
  for(i in i:length(d[,1])){
    x <- d[i,]
    s <- sum(x[2:ncol(d)])
    score <- append(score,s)
    i=i+1
  }
  rs <- data.frame(d[,1],score) #raw scores
  rs$logit <- data.frame(person.parameter(eRm)$thetapar)$NAgroup1
  rs$se <- data.frame(person.parameter(eRm)$se.theta)$NAgroup1
  rs
}

p.erm <- p.logits(erm_m9, df)
p.erm$group <- as.factor(group <- substr(p.erm$d...1., 1, nchar(p.erm$d...1.)-3))

ggplot(p.erm, aes(x = group, y = logit)) +
  geom_boxplot(outlier.shape = NA, outlier.size = 1, notch=FALSE) +
  geom_point(alpha = .05, size = .1) +   
  geom_jitter(position=position_jitter(0.2), size = 1.5) +
  labs(x="Site", y="Person Logits (eRm)") +                        
  theme_bw() +
  theme(axis.title.x = element_text(size=16)) +                  
  theme(axis.title.y = element_text(size=16)) +                 
  theme(axis.text = element_text(size = 14)) +
  theme(legend.position = "none") +
  scale_x_discrete(labels= c("Site A", "Site B")) +
  geom_boxplot(fatten=2, fill = "indianred3", alpha = 0.8)

### Export TAM Item Logits

names(i.se)[names(i.se) == "xsi"] <- "beta"
names(i.se)[names(i.se) == "se.xsi"] <- "se"
i.se$item <- colnames(df[,-1])

pn_count <- read_csv("pnCount.csv")
names(pn_count)[names(pn_count) == "df"] <- "item"
i.se <- left_join(i.se, pn_count)

write_csv(i.se, "TAM_logits.csv")

### Export TAM Item Fit Statistics
write_csv(i.fit, "i_fit.csv")
