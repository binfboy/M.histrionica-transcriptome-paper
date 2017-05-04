require(phytools)
require(png)
HBUG_GST<-read.newick("MSA_GST.NR_phyml_tree.bootstrapped.txt")
fontsize=c(0.27)
linewidth=c(1)

#Assign colors and state numbers...
HBUG_GSTcols<-c("black","blue","red","green","violet","orange"); names(HBUG_GSTcols)<-c(1,2,3,4,5,6)

#Rotate tree to make it pretty...
rtnodes <-c(72)
HBUG_GST <-rotate(HBUG_GST,rtnodes)
rtnodes <-c(109)
HBUG_GST <-rotate(HBUG_GST,rtnodes)

#Plot tree, use this for degubbing...
#plotTree(HBUG_GST, fsize=0.4); nodelabels(bg="white"); 

#Open png file
png(filename = "HBUG_GST_BOOTtest.png", width = 3000, height = 1000,
    units = "px",pointsize=12, bg="white", res=300)

###Color the clades using node numebr and state number...
#Sigma
snode<-c(81)
HBUG_GST<-paintSubTree(HBUG_GST,node=snode,stem=1,state=6)

#Theta
tnode<-c(77)
HBUG_GST<-paintSubTree(HBUG_GST,node=tnode,stem=1,state=2)

#Epsilon
enode<-c(74)
HBUG_GST<-paintSubTree(HBUG_GST,node=enode,stem=1,state=3)

#Microsomal
mnode<-c(113)
HBUG_GST<-paintSubTree(HBUG_GST,node=mnode,stem=1,state=4)

#Prostaglandin
pnode<-c(118)
HBUG_GST<-paintSubTree(HBUG_GST,node=pnode,stem=1,state=5)

#Finally plot the tree!!!
plotSimmap(HBUG_GST,xlim=c(0,2*max(nodeHeights(HBUG_GST))),HBUG_GSTcols, node.numbers=F,pts=F, lwd=linewidth,fsize=fontsize)
nodelabels(HBUG_GST$node.label, adj=c(1.2,-0.2),frame="none",cex=0.21)

#Label the clades, based on node number...
#cladelabels(tree=HBUG_GST,expression(paste("Theta (",theta,")")),node=tnode,offset=1,wing.length=0,cex=0.6,orientation="horizontal")
#cladelabels(tree=HBUG_GST,expression(paste("Epsilon (",epsilon,")")) ,node=enode,offset=1,wing.length=0,cex=0.6,orientation="horizontal")
#cladelabels(tree=HBUG_GST, "Microsomal",node=mnode,offset=0,wing.length=0,cex=0.6,orientation="horizontal")
#cladelabels(tree=HBUG_GST, expression(paste("Sigma (",sigma,")")),node=snode,offset=0.5,wing.length=0,cex=0.6,orientation="horizontal")

nodelabels(expression(paste("Theta (",theta,")")), 78, adj=-2.7, frame="none", col="blue",bg="white", cex=.6)
nodelabels(expression(paste("Epsilon (",epsilon,")")), enode, adj=-3, frame="none",col="red", bg="white", cex=.6)
nodelabels("Microsomal", 115, adj=1.5, frame="none", col="green",bg="white", cex=.6)
nodelabels(expression(paste("Sigma (",sigma,")")), 107, adj=-2.5, frame="none",col="orange", bg="white", cex=.6)
nodelabels("Prostaglandin E synthase", 112, adj=-2, frame="none",col="violet", bg="white", cex=.5)


#Close png file
dev.off()



