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
df_il1b$Concentration[df_il1b$Concheatentration < 0] <- 0

#concatenate tables
df3 <- rbind(df_gmcsf, df_ifng, df_il4, df_il6, df_il10, df_il12, df_mcp1, df_tnfa, df_il1b)

#average values
df4 <- aggregate(df3$Concentration, list(df3$Tx, df3$Cytokine), mean)
colnames(df4) <- c("Tx", "Cytokine", "Concentration.mean")

#long to wide formatting
df5 <- dcast(df4, Cytokine ~ Tx, value.var="Concentration.mean")
df6 <- df5[, c("Cytokine", "Mock", "LPS", "CpG")]

#Building heatmap matrix
library(gplots)
library(RColorBrewer)

rnames <- df6[,1]
df7 <- data.matrix(df6[,2:ncol(df6)])
rownames(df7) <- rnames

#log scale
df7 <- df7+1
df8 <- log10(df7)

#Adjust positions
#lmat = rbind(c(0,3),c(2,1),c(0,4))
#lwid = c(0.5,4)
#lhei = c(0.5,3,1)

#plot heatmap
my_palette <- colorRampPalette(c("black", "yellow", "orange", "red"))
heatmap.2(df8, col=my_palette, trace="none", dendrogram="row", Colv="NA", symm=F, density.info="none", symkey=F,symbreaks=F, scale="none", margin=c(8,8), colsep=1:ncol(df8), rowsep=1:nrow(df8), sepcolor="white")

#boxplot values
library(ggplot2)


order <- factor(df_tnfa$Tx, levels=c("Mock", "LPS", "CpG")) #Define order
error <- aes(ymax = resp + se, ymin=resp - se) #define error bars

ggplot(data=df_tnfa, aes(x=order, y=Concentration, fill=order)) + geom_bar(stat="identity") 
  + scale_fill_manual(values=c("black", "red", "blue")) + scale_y_log10() 
  + xlab("Treatment") + ylab("Concentration (pg/ml)") + ggtitle("TNFa")
  + geom_errorbar(limits, width=0.2)

#calculate significance



#merge tables
#df3 <- rbind(df_gmcsf, df_ifng, df_il4, df_il6, df_il10, df_il12, df_mcp1, df_tnfa, df_il1b)
#convert to zeroes
#df3$Concentration[df3$Concentration < 0] <- 0



