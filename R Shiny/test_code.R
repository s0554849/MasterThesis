#plotly
library(plotly)

df <- read.csv("/Users/benjaminwuthe/Library/Mobile Documents/com~apple~CloudDocs/Masterarbeit/Joshuas stuff/Daten/SCHRITT_4_1_MINSUP_MINCONF.csv")
#df <- read.csv("/Users/benjaminwuthe/Library/Mobile Documents/com~apple~CloudDocs/Masterarbeit/Joshuas stuff/Daten/SCHRITT_1_CLOSED_NON_DERIVABLE_ITEMSETS.csv")

# data - count, sort, and as DF
df_cnts <- as.data.frame( sort(table(df$FAULT_TYPE),decreasing =  FALSE))


# 
# 
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
