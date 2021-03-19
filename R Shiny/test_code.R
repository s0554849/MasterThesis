#plotly
library(plotly)
# install d3 package 
#df <- read.csv("/Users/benjaminwuthe/Library/Mobile Documents/com~apple~CloudDocs/Masterarbeit/Joshuas stuff/Daten/SCHRITT_1_CLOSED_NON_DERIVABLE_ITEMSETS.csv")

devtools::install_github("rstudio/r2d3")
################################
#                              #
#         INIT STUFF           #
#                              #
################################



df <- read.csv("/Users/benjaminwuthe/Library/Mobile Documents/com~apple~CloudDocs/Masterarbeit/Joshuas stuff/Daten/SCHRITT_5_1_WEIGHTED_SELECTION.csv")

# data - count, sort, and as DF
df_cnts <- as.data.frame( sort(table(df$FAULT_TYPE),decreasing =  FALSE))

# names(df) # show columnnames

#

interesting_cols <- c("FAULT_TYPE","rules","support","confidence","lift",
                      "FAULT_COUNT","MODEL","AGE","DEALERSHIP","CUSTOMER_TYPE",
                      "USER_CUSTOMISED", "COUNTRY","GEO_TYPE", "OBJECT_COUNT", 
                      "FAULT_RATE",  "AGGREGATION_LEVEL", "SET_SIZE" )

df <- df[,interesting_cols]

################################
#      INIT STUFF END          #
################################

# drop cols from df
names(df_sub)
df_sub <- subset(df, select= -c(rules,support,confidence, 
                                lift,FAULT_COUNT,#OBJECT_COUNT,
                                FAULT_RATE#,AGGREGATION_LEVEL,SET_SIZE
                                ))

# tree test
library(rpart)
library(rpart.plot)
library(collapsibleTree)
treetest<-function(){
  tree<- NULL
  print('treetest')
  
  tree <- rpart(
    FAULT_TYPE ~ .,
    data = df_sub, 
    method = "class", 
    maxdepth = 6)
  
  
  prp(tree,
  box.palette = "BuGn",
  # box.palette = "auto",
  split.box.col = "lightgray",
  split.border.col = "darkgray",
  )
  # collapsibleTree(df_sub,  hierarchy = c("MODEL", "AGE", "FAULT_TYPE"), 
  #                 width = 800,
  #                 zoomable = FALSE
  #                 )
}
treetest()

################################



DT::datatable(df[,interesting_cols])


library(r2d3)
r2d3(data=c(0.3, 0.6, 0.8, 0.95, 0.40, 0.20), script = "barchart.js")


# fig <- plot_ly(
#  y=df_cnts$Var1,
#  x=df_cnts$Freq,
# 
#   name = "SF Zoo",
#   type = "bar", 
#  orientation = 'h'
# )
# 
# fig
# library(data.table)
library(dplyr)
filter_rules <- c("Urban")
df_filter <- filter(df, grepl(filter_rules, rules, fixed = TRUE))
nrow(df)
print (nrow(df_filter))



txt <- "hund;katze;Maus"

txtlist <- strsplit(txt, ';')
print(txt)
library(foreach)
foreach(txtlist) %do%
  print(e1)

for (i in 1:length(txtlist[[1]])){
  print(txtlist[[1]][i])
}
print(txtlist[2])

foreach(i=1:3) %do%
  sqrt(i)

txtlist[[1]][1]




library(ggplot2)

df_cnts <-df
df_cnts <- as.data.frame( sort(table(df_cnts$FAULT_TYPE),decreasing =  TRUE))

ggplot(df_cnts, aes(Var1,Freq)) + geom_col()



df <- data.frame(trt = c("a", "b", "c"), outcome = c(2.3, 1.9, 3.2))
ggplot(df_cnts, aes(trt, outcome)) +geom_col()
