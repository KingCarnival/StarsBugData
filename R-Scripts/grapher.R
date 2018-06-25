library(tidyverse)
bugNames<-read.csv("R-Formated Data/Species Names.csv")
pdf("Results/graphs.pdf") 
for (var in unique(bugTables$SPECIES)) {
   graph1<- ggplot(data = bugTables) + geom_point(aes_string(x="`COUNTY`",y="`QUANTITY`"))+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .2))
   
   print(graph1)
}
dev.off()