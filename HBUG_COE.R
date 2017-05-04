require(phytools)
require(png)
HBUG_COE<-read.newick("Stopgap___MSA_COE.NR_phyml_tree.bootstrapped.txt")
fontsize=c(0.27)
linewidth=c(1)

#Assign colors and state numbers...
HBUG_COEcols<-c("black","lightslateblue","lightpink1","green4","yellow","plum"); names(HBUG_COEcols)<-c(1,2,3,4,5,6)

#Rotate nodes to make tree pretty...
rtnodes <-c(157)
HBUG_COE <-rotate(HBUG_COE,rtnodes)
#rtnodes <-c(177)
#HBUG_COE <-rotate(HBUG_COE,rtnodes)
#rtnodes <-c(178)
#HBUG_COE <-rotate(HBUG_COE,rtnodes)
#rtnodes <-c(179)
#HBUG_COE <-rotate(HBUG_COE,rtnodes)
#rtnodes <-c(180)
#HBUG_COE <-rotate(HBUG_COE,rtnodes)
#rtnodes <-c(181)
#HBUG_COE <-rotate(HBUG_COE,rtnodes)
rtnodes <-c(188)
HBUG_COE <-rotate(HBUG_COE,rtnodes)
#rtnodes <-c(213)
#HBUG_COE <-rotate(HBUG_COE,rtnodes)
#rtnodes <-c(297)
#HBUG_COE <-rotate(HBUG_COE,rtnodes)

#plotTree(HBUG_COE, fsize=0.1); nodelabels(bg="white"); 

#Open png file
png(filename = "HBUG_COE.png", width = 9000, height = 3000,
    units = "px",pointsize=12, bg="white", res=300)

#png(filename = "HBUG_COE.png", width = 1000, height = 800,
#units = "px",pointsize=12, bg="white", res=300)

###Color the clades using node numebr and state number...
#Beta esterases
bnode<-c(186)
HBUG_COE<-paintSubTree(HBUG_COE,node=bnode,stem=1,state=2)

#Acetylcholinesterases
anode<-c(174)
HBUG_COE<-paintSubTree(HBUG_COE,node=anode,stem=1,state=3)

#Neurotactin
nnode<-c(182)
HBUG_COE<-paintSubTree(HBUG_COE,node=nnode,stem=1,state=4)

#Neuroligin1
no1node<-c(157)
HBUG_COE<-paintSubTree(HBUG_COE,node=no1node,stem=1,state=6)

#Neuroligin2
no2node<-c(266)
HBUG_COE<-paintSubTree(HBUG_COE,node=no2node,stem=1,state=6)

#Finally plot the tree!!!
plotSimmap(HBUG_COE,xlim=c(0,2*max(nodeHeights(HBUG_COE))), HBUG_COEcols, node.numbers=F,pts=F, lwd=linewidth,fsize=fontsize)
nodelabels(HBUG_COE$node.label, adj=c(1.2,-0.2),frame="none",cex=0.21)
#Label the clades, based on node number...
#cladelabels(tree=HBUG_COE, expression(paste(beta,"-esterases")),node=bnode,offset=0,wing.length=NULL,cex=.7,orientation="vertical")
#cladelabels(tree=HBUG_COE, "Acetylcholinesterases",node=anode,offset=6,wing.length=0,cex=.3,orientation="horizontal")
#cladelabels(tree=HBUG_COE, "Neurotactins",node=nnode,offset=2,wing.length=0,cex=.3,orientation="horizontal")
#cladelabels(tree=HBUG_COE, "Neuroligins",node=no1node,offset=1.5,wing.length=0,cex=.3,orientation="horizontal")
#cladelabels(tree=HBUG_COE, "Neuroligins",node=no2node,offset=1.5,wing.length=0,cex=.3,orientation="horizontal")


nodelabels(expression(paste(beta,"-esterases")), 256, adj=-3, frame="none",col="lightslateblue", bg="white", cex=1.2)
nodelabels("Acetylcholinesterases", anode, adj=-1.8, frame="none", col="lightpink1",bg="white", cex=1.2)
nodelabels("Neurotactins", 182, adj=-2.5, frame="none",col="green4", bg="white", cex=1.2)
nodelabels("Neuroligins", 165, adj=-2.5, frame="none", col="plum", bg="white", cex=1.2)
nodelabels("Neuroligins", 193, adj=-5, frame="none", col="plum", bg="white", cex=1.2)



#Close png file
dev.off()


