#Load Data
df1 <- read.csv("~/R-tables/JL-M120_CBA.csv", header = TRUE)

#Reshape data
library(reshape2)
df2 <- melt(df1, id=c("Sample", "Tx", "Rep"))
colnames(df2) <- c("Sample", "Tx", "Rep", "Cytokine", "Concentration")

#convert MFIs to concentrations
if(df2$Cytokine = "GMCSF"){
  df2$Concentration <- (df2$Concentration * 11.771) - 394
}
if(df2$Cytokine = "IFNg"){
  df2$Concentration <- (df2$Concentration * 18.021) - 154
}
if(df2$Cytokine = "IL4"){
  df2$Concentration <- (df2$Concentration * 29.094) -545
}
if(df2$Cytokine = "IL6"){
  df2$Concentration <- (df2$Concentration * 17.253) -287
}
if(df2$Cytokine = "IL10"){
  df2$Concentration <- (df2$Concentration * 2.0357) -117
}
if(df2$Cytokine = "IL12"){
  df2$Concentration <- (df2$Concentration * 1.384) + 44
}
if(df2$Cytokine = "MCP1"){
  df2$Concentration <- (df2$Concentration * 2.6769) - 346.87
}
if(df2$Cytokine = "TNFa"){
  df2$Concentration <- (df2$Concentration * 8.719) - 44
}
if(df2$Cytokine = "IL1b"){
  df2$Concentration <- (df2$Concentration *14.068) + 57
}

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



