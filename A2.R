#ASSIGNMENT 2 sOCIAL NETWORK ANALYSIS

#T1

# Installing packages("igraph")
library(igraph) # Load the igraph package
library(igraphdata)
library(dplyr)

Gnodes <- read.csv("StreetGangNodes.csv") # Importing the data sets into R Studio
Glinks <- read.csv("StreetGangLinks.csv")

head(Gnodes) # Examine the data Vertices nodes 
head(Glinks) # Examine the edges links

# Creating an igraph object based on the data files
StreetGang_net <- graph_from_data_frame(d=Glinks, vertices=Gnodes, directed=F) #Directed = F (for false so undirected indicated by no arrows)

# Inspecting the attributes of the network
E(StreetGang_net) #(315) edges also links
V(StreetGang_net) #(54) vertices also nodes
edge_attr(StreetGang_net) #examine edge (links) attributes 
vertex_attr(StreetGang_net) #examine names vertices (nodes)
V(StreetGang_net)$Age #check attributes

#The following commands extract an edge list and an adjacency matrix from our igraph network.
as_data_frame(StreetGang_net, what="edges") #as_edgelist(StreetGang_net, names=T)
as_data_frame(StreetGang_net, what="vertices") #as_adjacency_matrix(StreetGang_net, attr="weight")

plot(StreetGang_net)#plotting simple network

#Widths of the links (edges) between nodes set to the value of weight attribute
E(StreetGang_net)$width <- E(StreetGang_net)$weight
V(StreetGang_net)$size <- V(StreetGang_net)$Age/2 #Size of each node equal age attribute divide by 2 

plot(StreetGang_net) #adjusted network with above attributes and specifications
#===============================================================================

#T2
#(a) Calculate degree, betweenness and closeness measures of each node

degcent<-degree(StreetGang_net) #number of links each node has
betcent<-betweenness(StreetGang_net, directed=T, weights=NA)
clocent<-closeness(StreetGang_net, mode="all", weights=NA) 

#saving above data to dataframe
centdf <-data.frame(name=names(degcent),degree=(degcent),betweeness=betcent,closeness=clocent)

#(b) repeat analysis performed in (a) to display nodes in descending order of their value for each centrality measure. 
# identify the top 3 nodes with the highest value for each centrality measure. 

order(degree(StreetGang_net), decreasing=TRUE) #attaining order of degree 
order(betweenness(StreetGang_net, directed=T, weights=NA), decreasing=TRUE)
order(closeness(StreetGang_net, mode="all", weights=NA), decreasing=TRUE) 

degsort<-centdf[order(-centdf$degree),] #able to order and create df for centrality values
betsort<-centdf[order(-centdf$betweeness),] #specifying the attribute value
closort<-centdf[order(-centdf$closeness),]

topdeg <-head(degsort,3) #sorting the top scoring values of our centrality measure
topbet <-head(betsort,3) #top three numbers 
topclo <-head(closort,3)

#Highest scoring nodes in the three different areas (degree, betweness, closeness)
topdeg
topbet
topclo

#================================================================================

#T3
#(a) Simplify network
#Remove nodes with a degree less than 15
StreetGang_net.deg <- delete.vertices(StreetGang_net, degree(StreetGang_net)<15)
plot(StreetGang_net.deg)
#Remove edges with a weight less than 3
StreetGang_net.sp <- delete_edges(StreetGang_neg, E(StreetGang_neg)[weight < 3])
plot(StreetGang_net.sp)
#Plot adjusted network 'layout nicely'
plot(StreetGang_net.sp,layout = layout_nicely)

#======================================================================================

#T4 
#Set colour of each node in network based on the ranking (Blue, Red)

V(StreetGang_net)$Ranking 

colrs <-c("white","gray","green","blue","gold") #colours we add to frame
V(StreetGang_net)$color <-colrs[V(StreetGang_net)$Ranking]
V(StreetGang_net)$size <-V(StreetGang_net)$Ranking*3#ranking multiply three for the size of node
V(StreetGang_net)$label<-NA #no label required
E(StreetGang_net)$width<-E(StreetGang_net)$weight/6#diving the weight of edges by three 
E(StreetGang_net)$arrow.size<-.2 #size of arrow
E(StreetGang_net)$edge.color<-"black" #colour of edge
plot(StreetGang_net,layout = layout_nicely) #plot setout format
legend(x="topleft", y=-1.1, c("Rank1","Rank2","Rank3","Rank4","Rank5"), pch=21,col="777777",pt.bg=colrs, pt.cex=2, cex=.8, bty="n",
       ncol=1) #legend and assignment of values

#=====================================================================================

#T4 
V(StreetGang_net)$Birthplace #gives us values 1,2,3,4
V(StreetGang_net)$Prison

BPcolrs <-c("yellow","red","blue","orange")
V(StreetGang_net)$color <-BPcolrs[V(StreetGang_net)$Birthplace]
V(StreetGang_net)$size <-V(StreetGang_net)$Birthplace*3
V(StreetGang_net)$label<-NA
E(StreetGang_net)$width<-E(StreetGang_net)$weight/6
E(StreetGang_net)$arrow.size<-.2
E(StreetGang_net)$edge.color<-"black"

plot(StreetGang_net)
legend(x="topleft", y=-1.1, c("West Africa","Carribean","UK","East African"), pch=21,col="777777",pt.bg=BPcolrs, pt.cex=2, cex=.8, bty="n",
       ncol=1)

prison <- delete.vertices(StreetGang_net, V(StreetGang_net) #deleting vertices of no prison time
[V(StreetGang_net)$Prison == 0])

edge.start <-ends(prison,es=E(prison), names=F)[,1]#creating new edges and assigning prison df
edge.col<-V(prison)$color[edge.start] #adding colour to the edges to signify connections 
plot(prison, edge.color=edge.col, edge.curved=.1,layout = layout_nicely)
legend(x="topleft", y=-1.1, c("West Africa","Carribean","UK","East African"), pch=21,col="777777",pt.bg=BPcolrs, pt.cex=2, cex=.8, bty="n",
       ncol=1)

#===========================================================================

#T4 
#(C)	Using the network created in (b) delete nodes where gang member ranking is less than 3. 

prison2 <- delete.vertices(prison, V(prison)
                          [V(prison)$Ranking < 3])#adjustment of ranking

edge.start <-ends(prison2,es=E(prison2), names=F)[,1]
edge.col<-V(prison2)$color[edge.start]
plot(prison2, edge.color=edge.col, edge.curved=.1)
legend(x="topleft", y=-1.1, c("West Africa","Carribean","UK","East African"), pch=21,col="777777",pt.bg=BPcolrs, pt.cex=2, cex=.8, bty="n",
       ncol=1)

#Now determine the hub score of gang members within this network.
#Using this network create a two-panel plot.
#In the first panel, plot the network where the size of each node is set to 15 times the value of the hub score. 
#In the second panel, display the communities within the network using the cluster_optimal() function.

hs <-hub.score(prison2,weights=NA)$vector #creating hub score using prison2 network
par(mfrow=c(1,2)) #plot two network format
plot(prison2, vertex.size=hs*15,main="Hubs") #creating visual plot with title 
clp <- custer_optimal(StreetGang_net) #cluster creation so we can see community behavior
plot(StreetGang_net,main="Communities") #plotting network

#===============================================================================

#T5. Simplify the network created in T1 based on the Ranking attribute. 
#Create 5 networks in total – one for each ranking. Plot each network. 

StreetGang_net <- graph_from_data_frame(d=Glinks, vertices=Gnodes, directed=F)
E(StreetGang_net)$width <- E(StreetGang_net)$weight
V(StreetGang_net)$size <- V(StreetGang_net)$Age/2 #Size of each node equal age attribute divide by 2 

rankings <- unique(V(StreetGang_net)$Ranking) #ranking attribute being created in rankings
subgraphs<-list() #creating subgraph list to create a total of 5 networks
for (ranking in rankings) {vrank <-V(StreetGang_net)[V(StreetGang_net)$Ranking == ranking]
grank<-induced_subgraph(StreetGang_net, vrank) #new data frame for stored sub graphs to implement networks
subgraphs[[as.character(ranking)]]<- grank} #function to plot rankings ref rstudio

colors1 <- c("white","gray","green","blue","gold")
par(mfrow = c(2, 3))  
for (i in 1:length(subgraphs)) {plot(subgraphs[[i]], main = paste("Ranking", names(subgraphs)[i]), vertex.color = colors1[i])
} #arrays of stored data in rankings distributing a total of 5 as per ranking attributes and counting system 

#================================================================================

#T6 (a)

#UK and West Africa

StreetGang_net <- graph_from_data_frame(d=Glinks, vertices=Gnodes, directed=F)
E(StreetGang_net)$width <- E(StreetGang_net)$weight!=1
V(StreetGang_net)$size <- V(StreetGang_net)$Age/2
StreetGang_net_df <- get.data.frame(StreetGang_net, what = "edges")
V(StreetGang_net)$Birthplace 

BPcol <-c("West Africa" = "yellow","Carribean" = "red","UK" ="blue","East Africa" = "orange")
V(StreetGang_net)$color <-BPcol[V(StreetGang_net)$Birthplace]
V(StreetGang_net)$label<-NA
E(StreetGang_net)$width<-E(StreetGang_net)$weight!=1
E(StreetGang_net)$arrow.size<-.2

except24 <- delete.vertices(StreetGang_net, V(StreetGang_net) #creating new network removing unneeded countries
                          [V(StreetGang_net)$Birthplace == 2]) #removing birthplace 2

except25 <- delete.vertices(except24, V(except24) #reintegrating formula to simplify networking accordingly 
                            [V(except24)$Birthplace == 4]) #removing birthplace 4

plot(except25, main ="UK and West Africa")
legend(x="topleft", y=-1.1, c("West Africa","Carribean","UK","East African"), pch=21,col="777777",pt.bg=BPcol, pt.cex=2, cex=.8, bty="n",
       ncol=1) #plotting only the data we need to measure rankings with UK and other countries
#===========================================================================================

#UK and Carribean

BPcol <-c("West Africa" = "yellow","Carribean" = "red","UK" ="blue","East Africa" = "orange")
V(StreetGang_net)$color <-BPcol[V(StreetGang_net)$Birthplace]
V(StreetGang_net)$label<-NA
E(StreetGang_net)$width<-E(StreetGang_net)$weight!=1
E(StreetGang_net)$arrow.size<-.2


except14 <- delete.vertices(StreetGang_net, V(StreetGang_net)
                            [V(StreetGang_net)$Birthplace == 1])

except15 <- delete.vertices(except14, V(except14)
                            [V(except14)$Birthplace == 4])

plot(except15, main = "UK and Carribean")
legend(x="topleft", y=-1.1, c("West Africa","Carribean","UK","East African"), pch=21,col="777777",pt.bg=BPcol, pt.cex=2, cex=.8, bty="n",
       ncol=1)

#=============================================================================================

#UK and East Africa

BPcol <-c("West Africa" = "yellow","Carribean" = "red","UK" ="blue","East Africa" = "orange")
V(StreetGang_net)$color <-BPcol[V(StreetGang_net)$Birthplace]
V(StreetGang_net)$label<-NA
E(StreetGang_net)$width<-E(StreetGang_net)$weight!=1
E(StreetGang_net)$arrow.size<-.2


except12 <- delete.vertices(StreetGang_net, V(StreetGang_net)
                            [V(StreetGang_net)$Birthplace == 1])

except13 <- delete.vertices(except12, V(except12)
                            [V(except12)$Birthplace == 2])

plot(except13, main = "UK and East Africa")
legend(x="topleft", y=-1.1, c("West Africa","Carribean","UK","East African"), pch=21,col="777777",pt.bg=BPcol, pt.cex=2, cex=.8, bty="n",
       ncol=1)

#=============================================================================================

#(b) using UK and carribean interactions, calculate authority scores of gang members. 
#set size of node to 10 x value of authority score 
#create two panel plotting window and plot network in first window 
#identify communities in this network using cluster_optimal() function and plot these communities in second panel

as <- authority_score(except15,weights=NA)$vector#creating authority score
clp<-cluster_optimal(except15)#creating cluster to see and identify communities in a network
par(mfrow=c(1,2)) #two networks to plot 
plot(except15, vertex.size=as*10, main = "Authorities") #authorities plot 
plot(clp,except15,main="Communities") #communities plot
