data<-data.frame(
  Gene=c("TNRC6B","TNRC6B","DAB2IP","DAB2IP","CDH1","CDH1","CDH1","MMP2","HIF1A","MMP2","RSR2","TLR10","TLR10","TLR1","TLR1","TLR1","TLR1","TLR1"),
  SNP=c("rs1447295","rs9623117","rs1571801","rs6983267","rs16260","rs6983561","rs1551512","rs1477017","rs11549465","rs11639960","rs2987983","rs4129009","rs4274855","rs5743604","rs7837688","rs4242382","rs10086908","rs7000448"),
  Genotype=c("CC","TT","GT","GT","CC","AA","TT","AG","CC","AG","AG","TT","CC","AA","GT","GG","TT","TT"),
  LR=c(0.9,0.9,1.2,1.0,0.8,1.0,0.9,1.2,1.0,1.2,1.1,0.9,0.9,0.9,1.7,0.9,1.0,1.1),
  Studies=c(19,8,6,3,3,2,2,1,1,1,1,1,1,1,1,1,1,1),
  Samples=c(56485,35869,13997,3985,2238,1846,1846,2878,2878,2878,2216,2163,2163,2163,2139,2139,2139,1012),
  PTB=c(15,14,16,16,13,12,12,13,14,16,18,17,16,15,23,21,22,23))

data$SNP<-as.character(data$SNP)
data$Gene<-as.character(data$Gene)
data$Genotype<-as.character(data$Genotype)

data$rownumber<-c(1:nrow(data))
data$colnumber<-1

p<-(ggplot(data, aes(x=colnumber,y=rownumber, label=Gene)) +scale_y_discrete() +geom_text()+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),plot.margin = unit(c(0,0,0,0), "cm"),plot.background = element_rect(colour = "white",fill = "white"),panel.background = element_rect(colour = "white",fill = "white"))+ylab(NULL)+labs(title="Gene"))


SNP<-(ggplot(data, aes(x=colnumber,y=rownumber, label=SNP)) +scale_y_discrete() +geom_text()+
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),plot.margin = unit(c(0,0,0,0), "cm"),plot.background = element_rect(colour = "white",fill = "white"),panel.background = element_rect(colour = "white",fill = "white"))+ylab(NULL)+labs(title="SNP"))


Genotype<-(ggplot(data, aes(x=colnumber,y=rownumber, label=Genotype))+scale_y_discrete() +geom_text()
           +
             theme(axis.title.x=element_blank(),
                   axis.text.x=element_blank(),
                   axis.ticks.x=element_blank(),plot.margin = unit(c(0,0,0,0), "cm"),plot.background = element_rect(colour = "white",fill = "white"),panel.background = element_rect(colour = "white",fill = "white"))+ylab(NULL)+labs(title="Genotype"))
LR<-(ggplot(data, aes(x=colnumber,y=rownumber, label=LR)) +scale_y_discrete() +geom_text()+
       theme(axis.title.x=element_blank(),
             axis.text.x=element_blank(),
             axis.ticks.x=element_blank(),plot.margin = unit(c(0,0,0,0), "cm"),plot.background = element_rect(colour = "white",fill = "white"),panel.background = element_rect(colour = "white",fill = "white"))+ylab(NULL)+labs(title="LR"))

Studies<-(ggplot(data, aes(x=colnumber,y=rownumber, label=Studies))+scale_y_discrete() +geom_text()+
            theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank(),plot.margin = unit(c(0,0,0,0), "cm"),plot.background = element_rect(colour = "white",fill = "white"),panel.background = element_rect(colour = "white",fill = "white"))+ylab(NULL)+labs(title="Studies"))

Samples<-(ggplot(data, aes(x=colnumber,y=rownumber, label=Samples))+scale_y_discrete() +geom_text()+
            theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank(),plot.margin = unit(c(0,0,0,0), "cm"),plot.background = element_rect(colour = "white",fill = "white"),panel.background = element_rect(colour = "white",fill = "white"))+ylab(NULL)+labs(title="Samples"))

PTB<-(ggplot(data, aes(x=colnumber,y=rownumber, label=PTB)) +scale_y_discrete() +geom_text()+
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),plot.margin = unit(c(0,0,0,0), "cm"),plot.background = element_rect(colour = "white", fill="white"),panel.background = element_rect(colour = "white",fill = "white"))+ylab(NULL)+labs(title="PTB"))

grid.newpage()
pushViewport(viewport(layout=grid.layout(1,7)))
vplayout<-function(x,y) viewport(layout.pos.row=x,layout.pos.col=y)
print(p,vp=vplayout(1,1))
print(SNP,vp=vplayout(1,2))
print(Genotype,vp=vplayout(1,3))
print(LR,vp=vplayout(1,4))
print(Studies,vp=vplayout(1,5))
print(Samples,vp=vplayout(1,6))
print(PTB,vp=vplayout(1,7))