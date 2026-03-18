
##BSE -- Networks ## 
## - Pau Milán - Jan. 2024

#########################
## Practice Session 1 ### 
#########################
## Introduction to manipulating network data 


## The first thing to do always is call the required packages (which should have already been installed)
## We will use the following packages for now: 
library(tidyverse)  #a collection of popular R packages for clean data management 
library(igraph)     #the main package related to network data and manipulation 
library(tidygraph)  #a wrapper around igraph objects that looks and feels tidy 
library(igraphdata) #a package containing some off-the-shelf network data sets to practice with 


#always make sure you clear your workspace before starting (it's good form)
rm(list=ls())


#Now we will learn how to import and create network objects in R 
#The easiest thing to do is to load a pre-existing network from the igraphdata package 
data(karate)  #this axiomatically loads the karate club we saw in class 

#notice that we now have an object (list of 10) in our environment. This is our network! 
#to explore it we can type karate in the console... notice it says it is an UNW graph of 34 nodes and 78 edges 
#What does UNW- mean? it means Undirected,Named,Weighted, 
# the last one is - (because it is NOT bipartite, otherwise there would be a B)

#recall that the weight of an edge corresponds to the number of activities the members participated in
#for more info go to: https://rdrr.io/cran/igraphdata/man/karate.html

#You can extract the Edges and Vertices of this graph by typing 
E(karate)
V(karate)

#and you can count them with 
ecount(karate)
vcount(karate)

#if I want to see the weights I can type (using the same column subsetting notation that is typical in base R)
E(karate)$weight
#this is an edge attribute, which will also show up if I call all edge attributes 
edge.attributes(karate)
# weight is the only edge attribute in this graph, but the vertices also have attributes 
vertex.attributes(karate)
#here I have 4 attributes: faction, name, label, and color
#faction is the faction they eventually split up into  (color is the same)
#so if I want to extract one of these attributes I just need to type V(karate)$<attribute name>
V(karate)$name 
V(karate)$Faction 

#so now that we have this object, what can we do with it? 
# igraph is equipped with many built-in functions for extracting network statistics very easily 
#for instance, if I want to compute the diameter of the graph **(maximal shortest path between any two nodes)**
diameter(karate, weights=NA)
# I have written "weights=NA" to tell it not to count the weighted shortest path 
# It spits out 5, which is the *shortest path between the two farthest nodes* (with weights it is 13)
# You might not believe me, but we can visualize this by simply plotting our graph 
plot(karate)
#this plot looks terrible... let's make it pretty 
V(karate)$size<-10
V(karate)$color="darkblue"
E(karate)$width<-1
E(karate)$color="darkred"
l=layout_with_graphopt(karate, charge=0.00001, mass=30)
# ?layout_with_graphopt    #check documentation

#ll=layout.circle(karate)
plot.new()
plot(karate,  vertex.frame.color='yellow', layout=l, vertex.label=NA)

#instead of first defining the size of the vertex to 10 and then plotting, we could have simply done it directly
plot(karate,  
     vertex.frame.color='yellow', 
     vertex.size=9, 
     layout=layout_with_fr, 
     vertex.label=NA, 
     edge.color="darkred")


##What if I want the size of the nodes to depend on how many connections they have? 
V(karate)$size=degree(karate) 

#and then plot again. Or alternatively you can directly say 
plot(karate,  
     vertex.frame.color='yellow', 
     vertex.size=2*degree(karate), 
     layout=l, 
     vertex.label=NA, 
     edge.color="darkred")


#now let's check visually that the diameter of this network is indeed 5 
nodes.diameter=get_diameter(karate)  # I get the people involved in this longest path 

#you can already see here that it involves 6 people (so will have length 5)
#but let's make it pop out in the picture
E(karate)$color="dark gray"
E(karate, 
  path=nodes.diameter)$color="dark blue"

# <- --- - #
# PITFALL No.1 #
# notice that the following natural way of subsetting would actually not work 
E(karate)[nodes.diameter]

# what is going on here is that each individual in nodes.diameter has a factor (i.e. number) associated to it and
# we are subsetting the edge that is in that number on the list (not what we want)

E(karate)$width=1
E(karate, path=nodes.diameter)$width=3
plot.new()
plot(karate,  vertex.color='yellow', vertex.frame.color="magenta",
     layout=l, vertex.size=9)

#we can also extract an NxN matrix telling us the shortest path length between any two nodes
L=distances(karate, weights=NA)

?distances
#Sometimes we want to see the degree distribution of a graph 
hist(degree(karate), 
     xlim=c(1, 18), 
     include.lowest = F, 
     breaks=20, 
     freq=F)
#we add the degree distribution on top
lines(degree.distribution(karate)[-1], col="dark red")
## or we can also write it using the pipe %>% operator which makes things easier to read (tidyverse philosophy)
karate%>%
  degree()%>%
  hist()

# the probability mass function: #type="o"  means put the dots and lines on the graph
plot(degree.distribution(karate), 
     type="o", xlab="degree", 
     ylab="probability", 
     main="The degree distribution of Zachary's Karate Club")


#We can do this with a number of different "centralities" that can be easily extracted 
plot(karate,  
     vertex.frame.color='gray', 
     vertex.size=betweenness(karate)*0.1)

## notice that those with the highest betweenness are the ones involved in our longest path 


#we could choose to incorporate the degree as an additional vertex attribute, so that it is associated to each node
vertex_attr(karate, "degree")=degree(karate)
#or equivalently
set_vertex_attr(karate, "degree", value=degree(karate))
#or even simpler 
V(karate)$degree=degree(karate)
#now there will be an additional vertex attribute called degree that has the degree of each node attached

#We could have just as well decided to incorporate the diameter of the graph as a graph attribute 
graph_attr(karate, "diameter")=diameter(karate, weight=NA)
#or even simpler 
karate$diameter=diameter(karate)
#this is a graph attribute since it is a single number that describes the entire network (not a statistic per node)


#returning to the plot, notice the way the nodes are placed. This can be changed using a different layout 
V(karate)$size=9
V(karate)$color="yellow"
plot(karate, layout=layout.circle)
#there are many algorithms: 
plot(karate, layout=layout.star)
plot(karate, layout=layout.fruchterman.reingold)
plot(karate, layout=layout.drl)
plot(karate, layout=layout.graphopt)
#you need to read up on them a bit to see how they differ and experiment to find out which one works best 
#for each setting (as we will see when we import Duflo's India Dataset)
## as we saw before with graph_opt, each one of these layouts has its own settings that can be

#Now that we have seen the basics, let us try and import a data set and create the igraph object ourselves 
#network data can come in many varieties and we should know how to transform it into an igraph object 
#to do this we will play around with the data that I collected in Malawi

malawi=read.csv("/Users/paumilan/Dropbox/Data/Malawi/Chied_Field_June_19/Data/Network/network module/networkmodule_expost.csv")

#this is a csv file containing nominations in a series of questions on assitance, and friendship. 
#the structure of this file is the most common way that we find network data
#this is known as an "edge list" and each row corresponds to a pair of households and a particular question
#moreover, we have enough info here to construct a weighted and directed graph (we know who points to who and for how many different questions)

#Union of all questions: build an igraph object using all unique pairs that are identified (id is not 0) 
graphi=graph_from_data_frame(filter(unique(malawi[,c("hhid","id")]), id!=0), directed = T)
#notice that I can tell it if the graph I am importing is directed or not (can also do it with weighted)


#One can also extract graphs from adjacency lists, from edgelists, or from adjacency matrices
#what are these objects? they are different ways of representing networks 

#you can also create these objects from an igraph object 
#for instance if I want the adjacency matrix of my graph I type
Amat=graphi[,]
#or equivalently (declaring if Sparse or not) 
Amat=get.adjacency(graphi, sparse = T)

#similarly to obtain an edgelist form an igraph object
edgelist=get.edgelist(graphi)

##and similarly you can obtain graph objects from them
#an edgelist is a matrix of 2 columns representing linked pairs
#here I need to make sure my edgelist is a character matrix
#otherwise it will create incorrect number of nodes 
lala=apply(filter(unique(malawi[,c("hhid","id")]), id!=0), 2, as.character) 

graph2=graph_from_edgelist(lala, directed =T)
graph3=graph_from_edgelist(edgelist, directed =T)
#check that they are identical
is_isomorphic_to(graph2, graph3)

#similarly we get the same graph if we build it from adjacency matrix or edgelist
graph4=graph_from_adjacency_matrix(Amat)
is_isomorphic_to(graph3, graph4)



#Now that we have seen how to deal with igraph, let us learn about a different (but I think better) way to do the same
# this approach is known as TIDYGRAPH and it essentially creates a different type of object than an igraph
# it stores all the same information as an igraph object, but in "tidy" format
#this allows for much easier manipulation, and it is in line with the tidyverse philosophy 

as_tbl_graph(filter(unique(malawi[,c("hhid","id")]), id!=0), directed=T)
#if we inspect tidygraph we see that it displays very differently from the igraph objects we created above
#in particular, we see that we can now "look inside" the igraph object and see the entire list of vertices and edges 
#they are stored as two simple data frames (tibbles to be more exact) and we can manipulate them as any other data frame

#although this graph looks very different, we can confirm that it is identical to the previous ones:
is_isomorphic_to(graph2, tidygraph)
#so tidygraph is the same "List of 10" that we had before but now we can manipulate it as two data frames


#Let's construct the malawi graph more seriously now
exp=c(999)  #I pick levels of confidence (for manual matches)
## choose if directed = FALSE or TRUE depending on the graph you want 
tidygraph=malawi%>%
      filter(expost_yes %in% exp)%>%
      select(hhid, id)%>%
      distinct%>%
      as_tbl_graph(directed=T)

#now you have a tbl_graph object with 270 nodes and 1659 edges 
#it tells you there is 1 component and it is a directed and simple graph
#then you can see two tables, one for the Nodes and one for the Edges (think of this as V(graph) and E(graph) from before)
#you can "activate" either of this tables and then manipulate it as you would any normal dataframe in R 



##Imagine I want to create a weighted graph counting the number of times that a household names each other 
mala_weight <- malawi %>%  
   group_by(hhid, id) %>%
   summarise(weight = n()) %>% 
   filter(id!=0)%>%
   ungroup()%>%
      as_tbl_graph()

#add Area as a node attribute, so we can see on the graph what area each household belongs to
#step1) first load the roster file with the area information and create a joint village-area name
load("/Users/paumilan/Dropbox/Data/Malawi/Chied_Field_June_19/Data/Roster/roster.RData")

data2 = data2 %>%
  as.tibble %>%
      select(hhid, villname, area)%>%
      unite(villarea, c("villname", "area"))%>%
      mutate(villarea=str_replace(villarea, '_Other', "" ))%>%
      mutate(hhid = as.character(hhid))%>%
      mutate(villarea=replace(villarea, villarea=="Kalonga_Geradi", "Kalonga"))%>%
      mutate(villarea=replace(villarea, villarea=="Geradi_Mkanda", "Geradi_Mkwanda"))%>%
      distinct()

#Now I add this village-area information as a node attribute to my graph
tidygraph%<>%
   activate(nodes)%>%
   left_join(data2, by=c("name"="hhid"))%>%
   mutate(degree=centrality_degree(mode="total"), 
          between=centrality_betweenness(directed=F))
#here I have added node attributes called degree and between. 

library(ggraph)
#A) plot the graph  (undirected)
ggraph(tidygraph, layout="graphopt") + 
      geom_edge_link(alpha=0.45, colour="gray37") + 
      geom_node_point(aes(colour = villarea), size = 2.5) + 
      #geom_node_text(aes(label=name))+
      theme_graph(base_family="sans")


#B) plot the graph (directed)
ggraph(tidygraph, layout="graphopt") + 
      geom_edge_link(alpha=0.65, colour="grey", arrow=arrow(type="closed", length=unit(1,'mm')), 
                     start_cap = circle(1.5, 'mm'), end_cap = circle(1.5, 'mm')) + 
      geom_node_point(aes(colour = villarea), size = 2.5) + 
      theme_graph()



##Finally, let us look at how to create Random Graphs and simulate stuff, as we saw in class 
#Recall the G(n,p) Erdos-Renyi random networks we saw in class. 
#the first argument is n, the second one can either be p (prob. between two nodes) or
# m the number of edges 
n=2000
p=0.01
q1=erdos.renyi.game(n,p)

#We could also have created G(n,p) graphs by constructing our own program as explained in the lecture notes

ER_graph = function(n, p, seed=1) {
   set.seed(seed)
   probs = runif(n*(n-1)/2)
   k = 1
   edges = NULL
   for (i in 1:(n-1)) {
      for (j in (i+1):n) {
         if (probs[k] < p) {
            edges = c(edges, c(i, j))
         }
         k = k + 1
      }
   }
   return(graph(edges=edges, n=n, directed=F))
}
#notice that this function is only designed to generate undirected random graphs. 
#We would need to adapt it if we wanted a directed version.

q1=ER_graph(n, p, seed=3)  #by not specifying the seed it will default to the one I specified in the defintion of the function (i.e. seed=1)

#Also, this program is MUCH SLOWER than the built-in function we used first. 
#you can compare their running times as follows: 
tic("ER")
q1=erdos.renyi.game(n,p)
toc()
tic("slowER")
q1=ER_graph(n, p, seed=3)
toc()
#that's because we are using a nested loop (so it's 200 times slower for n=2000)
#how could we speed this up? Any ideas? (look into vectorizing loops using the apply family)


### III - Properties of E-R graphs that we can test::

#1) Recall that the average degree should equal c=np 
c=n*p
mean(degree(q1))

#2) we also saw in class that clustering was equal to p
l=transitivity(q1)

#3) does the degree distribution look like a poisson?
plot(degree.distribution(q1), type="o")
lines(dpois(x=1:length(degree.distribution(q1)), lambda=c), col="red", lwd=3, lty=4)

#4) emergence of a giant component 
#create a list, which is an R object, that stores a network in each element of the list
probs=c(0, 0.001, 0.0011, 0.0012, 0.00125, 0.0013, 0.0014, 0.0015, 0.002)
v=lapply(probs, function(x)  erdos.renyi.game(n,x))

#you can summon one network by indexing the list appropriately 
  i=2   
      V(v[[i]])$size<-7
      V(v[[i]])$color="white"
      E(v[[i]])$width=1
      l=layout_with_graphopt(v[[i]])
      plot.new()
      frame()
      plot(v[[i]],  vertex.label=NA, vertex.frame.color='pink', layout=l, vertex.col="blue")

#Now we will create a plot with all the networks side by side so we can see the giant component emerge 
      par(mfrow=c(3,3))
      for (i in 1:length(probs)){
         V(v[[i]])$size<-3.5
         V(v[[i]])$color="white"
         E(v[[i]])$width=1
         plot(v[[i]],  vertex.label=NA, vertex.frame.color='orange', vertex.size=3.5, vertex.col="white")
   }
     #dev.off() 
      
      
      
#a better way to visualize the emergence of a giant component is as follows 
     #I am not plotting them side by side, but in different slides, with a max_size curve next to it
     
#first I create a plot function that will color the giant component nodes differently 
     plot_graph = function(g, main="", layout=layout.fruchterman.reingold, vsize=5) {
        comp = components(g)
        max_comp = (comp$membership == which.max(comp$csize))
        special = ifelse(max_comp, "orange", "blue")
        plot(g, layout=layout, vertex.size=vsize, vertex.label=NA,
             vertex.color=special, main=main)
     }
     
#Now I fix n and run over different values of p 
     n = 1000
     mean_degree = c(seq(0, 10, 0.25))
     max_size = NULL
     for (i in 1:length(mean_degree)) {
        d = mean_degree[i]
        p = d / (n-1)
        g = ER_graph(n, p)
        max_size[i] = max(components(g)$csize)
        
        layout(matrix(c(1, 2), 1), c(4, 3))
        plot_graph(
           g,
           main=paste0("p*(n-1)=", sprintf("%.2f", d), ", max_size=", max_size[i]))
        plot(c(0, max(mean_degree)), c(0, n), type="n",
             main="Summary", xlab="mean_degree", ylab="max_size")
        lines(mean_degree[1:i], max_size, type="o", pch=19)
     }

     
     