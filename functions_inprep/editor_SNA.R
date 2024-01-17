editors1990<-ALLDATA%>%filter(YEAR=="1990")%>%select(editor_id,JOURNAL)
editors1988<-ALLDATA%>%filter(YEAR=="1988")%>%select(editor_id,JOURNAL)
editors2013<-ALLDATA%>%filter(YEAR=="2013")%>%select(editor_id,JOURNAL)
levels(editors1990$JOURNAL)
levels(editors2013$JOURNAL)
levels(editors1988$JOURNAL)
editors1988<-droplevels(editors1988)
editors1990<-droplevels(editors1990)
editors2013<-droplevels(editors2013)

summary(editors2013)

table1990<-table(editors1990$editor_id,editors1990$JOURNAL)
head(table1990)
table2013<-table(editors2013$editor_id,editors2013$JOURNAL)
head(table2013)
table1988<-table(editors1988$editor_id,editors1988$JOURNAL)
head(table1988)
library(igraph)
graph1990<-graph_from_incidence_matrix(table1990)
graph2013<-graph_from_incidence_matrix(table2013)
graph1988<-graph_from_incidence_matrix(table1988)


#Generatecolorsbasedonmediatype:
colrs<-sample(colors(),23)
net1988.bp$color<-colrs

net1988.bp<-bipartite.projection(graph1988)
#editor_IDlinks
plot(net1988.bp$proj1,vertex.label.color="black",vertex.label.dist=1,
vertex.size=7)
#journallinks
plot(net1988.bp$proj2,vertex.label.color="black",vertex.label.dist=1,vertex.size=7,vertex.color=net1988.bp$color)
#editor_IDlinksNOLABELS
plot(net1988.bp$proj1,vertex.label=NA,vertex.size=7)



net1990.bp<-bipartite.projection(graph1990)
#editor_IDlinks
plot(net1990.bp$proj1,vertex.label.color="black",vertex.label.dist=1,
vertex.size=7)
#journallinks
plot(net1990.bp$proj2,vertex.label.color="black",vertex.label.dist=1,
vertex.size=7)
#editor_IDlinksNOLABELS
plot(net1990.bp$proj1,vertex.label=NA,vertex.size=7)



net2013.bp<-bipartite.projection(graph2013)
#editor_IDlinks
plot(net2013.bp$proj1,vertex.label.color="black",vertex.label.dist=1,
vertex.size=7)
#journallinks
plot(net2013.bp$proj2,vertex.label.color="black",vertex.label.dist=1,
vertex.size=7)
#editor_IDlinksNOLABELS
plot(net2013.bp$proj1,vertex.label=NA,vertex.size=7)

bipartite.projection()
