df1 <- read.csv("~/R-tables/JL-M154_CBA.csv")

#######################################################################################################################################
## Defines treatment groups

df2 <- df1[1:40, 2:NCOL(df1)]
colnames(df2)<- c("IFN-g", "IL-4", "IL-6", "IL-10", "IL-12", "IL-1b", "MCP-1", "TNF-a")
df2$Number <- 1:40
df2$Line <- rep(c("152 CTRL", "152 CD47 KO"), each=20)
df2$Tx <- rep(c("Vehicle", "CpG", "Vehicle", "CpG"), each=10)
df3 <- df2[c("Number", "Line", "Tx", "MCP-1", "IFN-g", "IL-6", "TNF-a", "IL-12", "IL-10",  "IL-4", "IL-1b")]


#######################################################################################################################################
## Converts MFIs to concentrations

#Conversions based on standard curves
df3$`IFN-g`<- (df3$`IFN-g` + 154)/18.02
df3$`IL-4` <- (df3$`IL-4` + 545)/29.094
df3$`IL-6` <- (df3$`IL-6` + 287)/17.253
df3$`IL-10` <- (df3$`IL-10` * 2.0357) -117
df3$`IL-12`<- (df3$`IL-12` - 44)/ 1.384
df3$`MCP-1` <- (df3$`MCP-1` + 346.87)/2.6769
df3$`TNF-a` <- (df3$`TNF-a` + 44)/8.719
df3$`IL-1b` <- (df3$`IL-1b` - 57)/14.068

#Sets negative valuess to zero

df3[df3 < 0] <- 0

#######################################################################################################################################
## Makes Heatmap of data
library(gplots)
library(RColorBrewer)

#Extracts matrix for heatmap and transposes it
df4 <- df3[,4:NCOL(df3)]
df5 <- t(df4)
df6 <- log10(df5 + 1)

#Defines colors and cutoffs
#my_palette <- colorRampPalette(c("black", "yellow", "orange", "red"))
#colors <- c(0, 1, 2, 3, 4)
my_palette <- colorRampPalette(c("black", "yellow", "red"))(n = 319)
colors <- c(seq(0,0.5,length=10), seq(0.51, 2,length=100),  seq(2.1, 3,length=200), seq(3.1,4,length=10))
                
#Plots heatmap
#png(file = "heatmap3.png")
heatmap.2(as.matrix(df6),
          col=my_palette,  
          breaks=colors,
          trace="none", 
          dendrogram="none", 
          Colv=FALSE,
          Rowv=FALSE,
          symm=FALSE, 
          density.info="none", 
          symkey=FALSE,
          symbreaks=FALSE, 
          scale="none", 
          margin=c(10, 5), 
          colsep=1:ncol(df6), 
          rowsep=1:nrow(df6), 
          lmat=rbind(c(4, 2), c(1, 3)), lhei=c(2, 8), lwid=c(4.5, 0.5),
          sepcolor="white",
          key.xlab="Log10 [Concentration(pg/ml)]",
          key.title="",
          key.par=list(mar=c(4,4,4,10))
)
#dev.off() 
#######################################################################################################################################
## Subsets and calculates standard curves

std1 <- df1[41:60,]
n <- 2500
std1$Concentration <- rep(c("0", n/256, n/128, n/64, n/32, n/16, n/8, n/4, n/2, n), each=2)

library(plyr)
ddply
