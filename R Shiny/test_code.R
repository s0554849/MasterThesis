#plotly
library(plotly)

# install d3 package 
#df <- read.csv("/Users/benjaminwuthe/Library/Mobile Documents/com~apple~CloudDocs/Masterarbeit/Joshuas stuff/Daten/SCHRITT_1_CLOSED_NON_DERIVABLE_ITEMSETS.csv")

#devtools::install_github("rstudio/r2d3")
################################
#                              #
#         INIT STUFF           #
#                              #
################################

df <- read.csv('SCHRITT_4_1_MINSUP_MINCONF.csv')# Initial dataset

df <- read.csv("data/SCHRITT_5_1_WEIGHTED_SELECTION.csv")

# data - count, sort, and as DF
df_cnts <- as.data.frame( sort(table(df$FAULT_TYPE),decreasing =  FALSE))

# names(df) # show columnnames
#

interesting_cols <- c("FAULT_TYPE","rules","support","confidence","lift",
                      "FAULT_COUNT","MODEL","AGE","DEALERSHIP","CUSTOMER_TYPE",
                      "USER_CUSTOMISED", "COUNTRY","GEO_TYPE", "OBJECT_COUNT", 
                      "FAULT_RATE",  "AGGREGATION_LEVEL", "SET_SIZE" )

df <- df[,interesting_cols]

items <- c("AGE","MODEL",	"DEALERSHIP"	,"CUSTOMER_TYPE",	"USER_CUSTOMISED",
           "COUNTRY",	"GEO_TYPE",	"FAULT_TYPE")


################################
#      INIT STUFF END          #
################################

sankey_cols <- c("AGE","MODEL",	"DEALERSHIP"	,"CUSTOMER_TYPE")


dats_all <- df %>%      
  group_by_at(vars(one_of(sankey_cols)))%>%  # group them# data
  # group_by( vars(sankey_cols)) %>%  # group them
  summarise(Freq = FAULT_COUNT) 


l <- length(sankey_cols)


# now plot it
data_allu <-alluvial( dats_all[,1:l], 
          freq=dats_all$Freq, 
          border= NA , 
          col =  "Red",
          alpha = 0.3
)
###############
d2<-df[,c(selectedHier , "FAULT_COUNT")]
x<- for (e in selectedHier){df[,e]}

# AGGREGATING
form = as.formula(paste(". ~", paste(selectedHier, collapse = " + ")))
aggregate(form, data = d2,FUN = sum)

aggregate(FAULT_COUNT ~ AGE, data = df , FUN = sum)
######


dtest <- data.frame(a = c('a','b','c','a'), b=c(2,3,2,4))

dtest$perc<-sapply(dtest$b, FUN = function(x)x/sum(dtest$b))


####
# colorpalete mapping

colorRamp()


library(RColorBrewer)

brewer.pal(n = 3, name = "RdBu")






df_grp1 <- df %>%      
  group_by_at(vars(one_of(sankey_cols[1:2])))%>%  # group them# data
  # group_by( vars(sankey_cols)) %>%  # group them
  summarise(Freq = FAULT_COUNT) 


df_grp2 <- df %>%      
  group_by_at(vars(one_of(sankey_cols[2:3])))%>%  # group them# data
  # group_by( vars(sankey_cols)) %>%  # group them
  summarise(Freq = FAULT_COUNT) 

names(df_grp1) <- c("source", "target", "value")
names(df_grp2) <- c("source", "target", "value")

df_grp_full<-NULL
df_grp_full <- as.data.frame( rbind(df_grp1, df_grp2))
library(networkD3)
nodes = data.frame("name" = 
                     c("*", # Node 0
                       "InWarranty", # Node 1
                       ))# Node 3
sankeyNetwork(Links = df_grp_full, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 12, nodeWidth = 30)

nodes = data.frame("name" = 
                     c("Node A", # Node 0
                       "Node B", # Node 1
                       "Node C", # Node 2
                       "Node D"))# Node 3
links = as.data.frame(matrix(c(
  0, 1, 10, # Each row represents a link. The first number
  0, 2, 20, # represents the node being conntected from. 
  1, 3, 30, # the second number represents the node connected to.
  2, 3, 40),# The third number is the value of the node
  byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 12, nodeWidth = 30)



################################
#      ICICLE TEST START       #
################################


plot(df$FAULT_TYPE)#, type= "histogram")

plot(table(df$FAULT_TYPE) )
hist(tabele(df$FAULT_TYPE))

t <- as.data.frame(table(df$FAULT_TYPE))
t$Freq2 <- as.data.frame(table(df$FAULT_TYPE))$Freq

fig <- plot_ly(t, x = ~Var1, y = ~Freq, type = 'bar', name = '1')
fig <- fig %>% add_trace(y = ~Freq2, name = 'LA Zoo')
fig


df$FAULT_TYPE[1:40]

f <- 'FAULT_COUNT'
f <- 'FAULT_TYPE'
categoricalFeatures <-
  c(
    "FAULT_TYPE",
    "MODEL",
    "AGE",
    "DEALERSHIP",
    "CUSTOMER_TYPE",
    "USER_CUSTOMISED",
    "COUNTRY",
    "GEO_TYPE"
  )

paste("a","v",sep="")



df[f]

df_sub
library(data.tree)
data("acme")
as.data.frame(acme)
df_net <- ToDataFrameNetwork(acme, "cost")#
df_net

ToDataFrameNetwork(df_sub, "FAULT_RATE")

append( c(1,2,3),c(9,8,7))

library(hierarchicalSets)
data('twitter')

twitSet <- create_hierarchy(twitter)
twitSet
## A HierarchicalSet object
## 
##                  Universe size: 28459
##                 Number of sets: 100
## Number of independent clusters: 6

plot(twitSet)

################################
#      ICICLE TEST END         #
################################


print(as.data.frame(twitter))


library(hierarchicalSets)
data('twitter')

twitSet <- create_hierarchy(twitter)
twitSet
plot(twitSet, type = 'intersectStack', showHierarchy = TRUE)


# drop cols from df
df_sub <- subset(df, select= -c(rules,support,confidence, 
                                lift,FAULT_COUNT,OBJECT_COUNT#,
                                #FAULT_RATE#,AGGREGATION_LEVEL,SET_SIZE
                                ))




# tree test
library(rpart)
library(rpart.plot)
library(collapsibleTree)
treetest<-function(){
  tree<- NULL
  print('treetest')
  
  tree <- rpart(
    FAULT_RATE ~ .,
    data = df_sub, 
    method = "anova", 
    maxdepth = 5)
  
  
  prp(tree,
  box.palette = "BuGn",
  # box.palette = "auto",
  split.box.col = "lightgray",
  split.border.col = "darkgray",
  )
  return(tree)
  # collapsibleTree(df_sub,  hierarchy = c("MODEL", "AGE", "FAULT_TYPE"), 
  #                 width = 800,
  #                 zoomable = FALSE
  #                 )
}



tree <- treetest()
tree$variable.importance
# tree$ordered

print(rpart.rules(tree))

rpart.rules(tree_r)

suggestionText <- paste("Next best feature by regression Tree: \n", rpart.rules(tree)[[3]][1],"\nConsider feature values:", rpart.rules(tree)[[5]][1])
print(suggestionText)

prp(tree)
rpart.plot(tree , box.palette = "Reds")
################################
# TEST TREE STRUCTURE

library(treemap)
library(data.tree)

df$pathString <- paste("FAULTS", 
                        df$AGE,
                        df$FAULT_TYPE,
                        df$COUNTRY,
                       # sum(population$Get("COUNTRY", filterFun = isLeaf), na.rm = TRUE),
                        #df$OBJECT_COUNT,
                        sep = "/")
df_node <-  as.Node(df)


Aggregate(df_node, "OBJECT_COUNT", sum)

myApply <- function(node) {
  node$totalObjects <- 
    sum(c(node$FAULT_COUNT, purrr::map_dbl(node$children, myApply)), na.rm = TRUE)
}
myApply(df_node)

df_node2<-print(df_node,'totalObjects')




df_node$AddChild("OBJECT_COUNT")

range01 <- function(x){(x-min(x))/(max(x)-min(x))}
df_node
df$FAULT_SCALE <- range01(df$FAULT_COUNT )
  
collapsibleTreeSummary(
    df,  hierarchy = selectedHier, 
    root = "Cars",
    attribute = "FAULT_SCALE",
    nodeSize = "FAULT_SCALE",
    width = 800, zoomable = TRUE,
    height = 
    inputId = "treeUpdate"
)






print(df_node)
plot(df_node, "totalObjects")

data(acme)
print(acme, "cost")

df_node$aggVals<- Aggregate(df_node, "cost", sum)



library(networkD3)
acmeNetwork <- ToDataFrameNetwork(df_node2, "name")
p<-simpleNetwork(acmeNetwork[-3], fontSize = 10)


useRtreeList <- ToListExplicit(df_node, unname = TRUE)
a<-radialNetwork( useRtreeList)

library(plotly)
library(crosstalk)
library(DT)


sd <- SharedData$new(df)

a <- plot_ly(sd, x = ~support, y = ~confidence) %>% 
  add_markers(alpha = 0.5) %>%
  highlight("plotly_selected", dynamic = TRUE)


options(persistent = TRUE)

p <- datatable(sd)

bscols(widths = c(6, 6), a, p)

df_sub <- df


df_sub <- df_sub %>%
  filter(FAULT_TYPE %in% 'BreakFluid')
names(df)


treemap <- plot_ly(
  source = "treemap",
  type = "treemap",
  labels = prepared_data$labels,
  ids = prepared_data$ids,
  parents = prepared_data$parents,
  customdata = prepared_data$customdata,
  values = prepared_data$values,
  text = prepared_data$colors,
  texttemplate = texttemplate,
  hovertemplate = hovertemplate,
  marker = list(
    colors = prepared_data$colors_adj,
    showscale = TRUE,
    colorscale = "Reds"
  ),
  branchvalues = "total",
  height = 700,
  maxdepth = -1
)
treemap


##################
dfs<-NULL
dfs <- list(Id= "Init Data", subsets= df_cnts)

dfs<-append(dfs, list(Id= "new one", subsets= df))



df_names <- c('Init Dataset')
df_subsets <- list(list(df))



addSubset <- function (name, subset){
  df_names<<-append(df_names, name)
  df_subsets <<- append(df_subsets, list(subset))
}

getSubset <- function(name){
  tf <- df_names ==name #where name is in subset
  print(df_subsets[tf]) # retrun the corresponding subset
}

getSubset('newone')


addSubset('newone', df_cnts)


print(df_names, df_subsets)



d<-NULL
d <- objects(df[])
as.data.frame(d)

print(d)

##################


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





s <-strsplit("Please create a subset to compare"," ")
fr <- c()
i<-1
for (x in 1:length(s)){
  #integer(i)
  fr[i]<- i
  i<-i+1
  
}
library(wordcloud2)
txt <- data.frame(word=s,
freq=fr)

wordcloud2(data = txt )

