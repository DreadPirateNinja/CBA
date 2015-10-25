#Load Data
df1 <- read.csv("~/R-tables/JL-M120_CBA.csv", header = TRUE)

#Reshape data
library(reshape2)
df2 <- melt(df1, id=c("Sample", "Tx", "Rep"))
colnames(df2) <- c("Sample", "Tx", "Rep", "Cytokine", "Concentration")

#subset by cytokines
df_gmcsf <- subset(df2, Cytokine == "GMCSF")
df_ifng <- subset(df2, Cytokine == "IFNg")
df_il4 <- subset(df2, Cytokine == "IL4")
df_il6 <- subset(df2, Cytokine == "IL6")
df_il10 <- subset(df2, Cytokine == "IL10")
df_il12 <- subset(df2, Cytokine == "IL12")
df_mcp1 <- subset(df2, Cytokine == "MCP1")
df_tnfa <- subset(df2, Cytokine == "TNFa")
df_il1b <- subset(df2, Cytokine == "IL1b")

#convert MFIs to concentrations
df_gmcsf$Concentration <- (df_gmcsf$Concentration * 11.771) - 394
df_ifng$Concentration <- (df_ifng$Concentration * 18.021) - 154
df_il4$Concentration <- (df_il4$Concentration * 29.094) -545
df_il6$Concentration <- (df_il6$Concentration * 17.253) -287
df_il10$Concentration <- (df_il6$Concentration * 2.0357) -117
df_il12$Concentration <- (df_il12$Concentration * 1.384) + 44
df_mcp1$Concentration <- (df_mcp1$Concentration * 2.6769) - 346.87
df_tnfa$Concentration <- (df_tnfa$Concentration * 8.719) - 44
df_il1b$Concentration <- (df_il1b$Concentration *14.068) + 57

#convert negative values to zeros
df_gmcsf$Concentration[df_gmcsf$Concentration < 0] <- 0
df_ifng$Concentration[df_ifng$Concentration < 0] <- 0
df_il4$Concentration[df_il4$Concentration < 0] <- 0
df_il6$Concentration[df_il6$Concentration < 0] <- 0
df_il10$Concentration[df_il10$Concentration < 0] <- 0
df_il12$Concentration[df_il12$Concentration < 0] <- 0
df_mcp1$Concentration[df_mcp1$Concentration < 0] <- 0
df_tnfa$Concentration[df_tnfa$Concentration < 0] <- 0
df_il1b$Concentration[df_il1b$Concentration < 0] <- 0

#concatenate tables
df3 <- rbind(df_gmcsf, df_ifng, df_il4, df_il6, df_il10, df_il12, df_mcp1, df_tnfa, df_il1b)

#average values
test <- aggregate(Concentration~Tx, df_tnfa, mean)

library(dplyr)
gmcsf.mean <- df_gmcsf %>% group_by(Tx) %>% 
  summarize(Concentration.mean = mean(Concentration))
ifng.mean <- df_ifng %>% group_by(Tx) %>% 
  summarize(Concentration.mean = mean(Concentration))
il4.mean <- df_il4 %>% group_by(Tx) %>% 
  summarize(Concentration.mean = mean(Concentration))
il6.mean <- df_il6 %>% group_by(Tx) %>% 
  summarize(Concentration.mean = mean(Concentration))
il10.mean <- df_il10 %>% group_by(Tx) %>% 
  summarize(Concentration.mean = mean(Concentration))
il12.mean <- df_il12 %>% group_by(Tx) %>% 
  summarize(Concentration.mean = mean(Concentration))
mcp.mean <- df_mcp1 %>% group_by(Tx) %>% 
  summarize(Concentration.mean = mean(Concentration))
tnfa.mean <- df_tnfa %>% group_by(Tx) %>% 
  summarize(Concentration.mean = mean(Concentration))
il1b.mean <- df_il1b %>% group_by(Tx) %>% 
  summarize(Concentration.mean = mean(Concentration))

#Calculate standard deviation
gmcsf.sd <- df_gmcsf %>% group_by(Tx) %>% 
  summarize(Concentration.sd = sd(Concentration))
ifng.sd <- df_ifng %>% group_by(Tx) %>% 
  summarize(Concentration.sd = sd(Concentration))
il4.sd <- df_il4 %>% group_by(Tx) %>% 
  summarize(Concentration.sd = sd(Concentration))
il6.sd <- df_il6 %>% group_by(Tx) %>% 
  summarize(Concentration.sd = sd(Concentration))
il10.sd <- df_il10 %>% group_by(Tx) %>% 
  summarize(Concentration.sd = sd(Concentration))
il12.sd <- df_il12 %>% group_by(Tx) %>% 
  summarize(Concentration.sd = sd(Concentration))
mcp1.sd <- df_mcp1 %>% group_by(Tx) %>% 
  summarize(Concentration.sd = sd(Concentration))
tnfa.sd <- df_tnfa %>% group_by(Tx) %>% 
  summarize(Concentration.sd = sd(Concentration))
il1b.sd <- df_il1b %>% group_by(Tx) %>% 
  summarize(Concentration.sd = sd(Concentration))

#boxplot values
library(ggplot2)
library(easyGgplot2)
ggplot2.boxplot(data=df_tnfa)
ggplot2.boxplot(data=df_tnfa, yName="Concentration", xName ="Tx") +  scale_x_discrete(limits = Treatments)

ggplot2.boxplot(data=df_tnfa, yName="Concentration", xName ="Tx", groupName="Tx", groupColors=c("blue", "red","black")) +  scale_x_discrete(limits = Treatments)
ggplot2.boxplot(data=df_il6, yName="Concentration", xName ="Tx", groupName="Tx", groupColors=c("blue", "red","black")) +  scale_x_discrete(limits = Treatments)
ggplot2.boxplot(data=df_tnfa, yName="Concentration", xName ="Tx", groupName="Tx", groupColors=c("blue", "red","black")) +  scale_x_discrete(limits = Treatments)
#calculate significance



#merge tables
#df3 <- rbind(df_gmcsf, df_ifng, df_il4, df_il6, df_il10, df_il12, df_mcp1, df_tnfa, df_il1b)
#convert to zeroes
#df3$Concentration[df3$Concentration < 0] <- 0



