#####1. 데이터######################
multidat<-read.csv("제주.csv",header=T)
multidat
multidat1<-multidat[,-(19:22)] #위도, 경도 제거
multidat1
str(multidat)
str(multidat1)
multidat11<-multidat[seq(3,92,3),] #합계만 확인
multidat11

summary(multidat1)


-----------------------------------------------------------------------------
#####2. joyplot####################

library(ggplot2)
library(ggjoy)
library(reshape2)

multidat<-read.csv("제주.csv",header=T)
multidat
multidat1<-multidat[,-(19:22)] #위도, 경도 제거
multidat1

multidat2<-melt(id=1:6, multidat1)
multidat2
str(multidat2)


ggplot(multidat2, aes(x = multidat2$value, y = multidat2$variable, fill=multidat2$variable, alpha=0.8)) + 
  geom_joy()+
  labs(title='2016년 제주도 방문객 추이',x= "방문객 수",y = "월별 제주도 관광지 방문객") +
  guides(fill=F)+
  theme(legend.position='none')


# x축 방문객 수 범위 지정 
ggplot(multidat2, aes(x = multidat2$value, y = multidat2$variable, fill=multidat2$variable, alpha=0.8)) + 
  geom_joy()+
  labs(title='2016년 제주도 방문객 추이',x= "방문객 수",y = "월별 제주도 관광지 방문객") +
  guides(fill=F)+
  theme(legend.position='none')+xlim(0,40000)
  
ggplot(multidat2, aes(x = multidat2$value, y = multidat2$variable, fill=multidat2$variable, alpha=0.8)) + 
  geom_joy()+
  labs(title='2016년 제주도 방문객 추이',x= "방문객 수",y = "월별 제주도 관광지 방문객") +
  guides(fill=F)+
  theme(legend.position='none')+xlim(40000,320000)
  

#상위 관광지 4개와 하위 관광지 4개 뽑기

multidat<-read.csv("제주.csv",header=T)
multidat
multidat11<-multidat[seq(3,92,3),] #합계만 확인
multidat11
str(multidat11)
multidat112<-multidat11[,-(19:22)]

library(plyr)
multidat111<-arrange(multidat112,desc(총계))
multidat111

high<-multidat111[1:4,]
high
low<-multidat111[27:30,]
low

c<-rbind(high,low)
c

multidat22<-melt(id=1:6,c)
multidat22

ggplot(multidat22, aes(x = multidat22$value, y = multidat22$관광지, fill= multidat22$관광지, alpha=0.8)) + 
  geom_joy()+
  labs(title='2016년 상위/하위 제주도 관광지 방문객 추이',x= "전체 방문객 수",y = "상위/하위 관광지") +
  guides(fill=F)+
  theme(legend.position='none')


-----------------------------------------------------------------
#####3. treemap###################


library(treemap)
library(RColorBrewer)
display.brewer.all()

library(plyr)
multidat2<-melt(id=1:6, multidat11)
multidat2

#내국인 대비 외국인 비율
m1<-multidat[seq(1,89,3),]
m1
m2<-m1[,6]
m2

m3<-multidat[seq(2,90,3),]
m3
m4<-m3[,6]
m4

mm<-(m4/m2)
mm

multidat3<-cbind(multidat11,mm)
multidat3

multidat4<-multidat3[,-(7:22)]
multidat4 


treemap(multidat4,
        index=c("군구", "관광지"),
        vSize="총계",
        vColor="mm",
        title.legend="16년 내국인 대비 외국인 제주도 관광객 수",
        palette='Spectral',
        type="value")


------------------------------------------------------------------
#####4.buble plot####################


library(ggmap)

summary(multidat)
str(multidat)


qmap(location='Jeju island',zoom=10,maptype='roadmap')+
  geom_point(data=multidat,aes(x=경도,y=위도,colour=총계,shape=구분),
             size=4,alpha=0.9)+
  xlab("Latitude") +
  ylab("Longitude")+
  ggtitle("유/무료에 따른 전체 방문객 수")+
  scale_colour_gradient(name ="방문객 수",low = "deepskyblue", high = "red")



##유/무료에 따른 내국인 방문객 수

multi1<-subset(multidat,내.외국인!='합계')
multi1
str(multi1)

multi2<-subset(multi1,내.외국인!='외국인')
multi2


qmap(location='Jeju island',zoom=10,maptype='roadmap')+
  geom_point(data=multi2,aes(x=경도,y=위도,colour=총계,shape=구분),
             size=4,alpha=0.9)+
  xlab("Latitude") +
  ylab("Longitude")+
  scale_colour_gradient(name ="방문객 수",low = "deepskyblue", high = "red")
#=>천지연폭포, 성산일출봉, 중문대포해안주상절리 순으로 높은 것으로 나타남.


##유/무료에 따른 외국인 방문객 수 

multi3<-subset(multi1,내.외국인!='내국인')
multi3

qmap(location='Jeju island',zoom=10,maptype='roadmap')+
  geom_point(data=multi3,aes(x=경도,y=위도,colour=총계,shape=구분),  
             size=5,alpha=0.9)+
  xlab("Latitude") +
  ylab("Longitude")+
  ggtitle("관광지 입장 유/무료에 따른 외국인 방문객 수")+
  scale_colour_gradient(name ="방문객 수",low = "deepskyblue", high = "red")
#=> 민속자연사박물관,


----------------------------------------------------------
#####5.igraph

nodes <- read.csv("links.csv", header=T, as.is=T)

nodes

links <- read.csv("실험2.csv", header=T, as.is=T)

links

nrow(nodes); 
length(unique(nodes$id))
#noes$id에 중복이 있는지 확인

nrow(links);
nrow(unique(links[,c("from", "to")]))

(links <- links[order(links$from, links$to),])

net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
net

class(net)
E(net) 

colrs <- c("#FBB4AE","#B3CDE3")
vertex.
par(mar=c(0,0,0,0))
plot(net, edge.arrow.size=.1, edge.curved=0,
     vertex.color=c(ifelse(nodes$군구=='서귀포시',"#FBB4AE","#B3CDE3")), vertex.frame.color="gray50",
    vertex.label=V(net)$관광지, vertex.label.color="black",
     vertex.label.cex=.7,
     vertex.connectivity(net),
     layout=layout.star)+
  legend('bottomleft', c("서귀포시","제주시"),
       pch=21, pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)


------------------------------------------------------------------
#####6.network plot##################

ne<-read.csv("제주네트워크1.csv",header=T)
ne

net<-ne[,2:5]
net

library(ggplot2)
library(corrr)
install.packages("corrr")

net %>% correlate() %>% fashion()
net %>% correlate() %>% rplot()

x <- correlate(net)
fashion(net)
network_plot(x,min_cor = .1, colors = c("red", "green"), legend = TRUE)

net %>% correlate() %>% network_plot(min_cor = .1)


------------------------------------------------------------------
#####7.단계구분도###################

library(ggplot2)

library(maps)

library(maptools)


j<-read.csv("제주.csv",header=T)
head(j)

map<-read.csv('mapv2_final.csv',header=T)

mapv1<-subset(map,시군구명=='서귀포시')
mapv1$시군구명<-as.character(mapv1$시군구명)
mapv1

j1<-subset(j,내.외국인!='내국인'& 내.외국인!='외국인')
j1

ggplot(j1,aes(map_id=region,fill=총계))+
geom_map(map=mapv1,alpha=0.3,colour='snow4',size=1)+
theme(legend.position='none')+
scale_fill_gradientn(colours=c('white','blue'))+
expand_limits(x=mapv1$long,y=mapv1$lat)+
coord_fixed()+
xlab("Lattitude")+
ylab("Longitude") 






------------------------------------------------------------------
#####igraph

setwd('C:/')


nodes <- read.csv("links.csv", header=T, as.is=T)

nodes

#꼭지점에 대한 정보.

summary(nodes)


head(nodes) #변수명


links <- read.csv("실험2.csv", header=T, as.is=T)

links


#어떤 관계가 있는지.



nrow(nodes); 
length(unique(nodes$id))
#noes$id에 중복이 있는지 확인=> 중복 없어보임.

nrow(links);
#52
nrow(unique(links[,c("from", "to")])) #중복되어 있는거 빼고 중복 안되어있는 것만 확인해봤더니
#49
#중복되어 있는게 3개 link임.


#there are more links than unique from-to combinations.
#That means we have cases in the data where there are
#multiple links between the same two nodes. 


#counts weights for the duplicate links 
(links <- aggregate(links[,3], links[,-3], sum))
#aggregate: 행별로 연산 (colunm별로 연산은 쉬움) #media별로만 selection해서 sum할 수 있음.
#links[,3]: 3번째 column(weight)을 계산할 건데=x역할,
#links[,-3]:3번째 column(weight)만 빼고 (from, to,type의 별로 조합을 만들어서)=by역할



#ordering from to links
(links <- links[order(links$from, links$to),])
#from 가지고 정렬하고 그 안에서 to 기준으로 정렬



#change column names
colnames(links)[4] <- "weight"
links
#weight와 type변수의 위치가 바뀜.


rownames(links) <- NULL 


#Turning networks into igraph objects

library(igraph)


net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
net
#directed=T:방향이 있음. 

#d : describes the edges of the network. 
#link data 지정 
#Its first two columns are the IDs of the source 
#and the target node for each edge. 

#vertices: starts with a column of node IDs.
#Any following columns are interpreted as node attributes.
#꼭지점 data에 대한 정보

class(net)

#edge 가장자리 확인
E(net)       # The edges of the "net" object

#vertices 꼭지점 확인
V(net)       # The vertices of the "net" object



E(net)$type  # Edge attribute "type"


V(net)$media # Vertex attribute "media"




#split the window into 2 row, 1 columns 비교하기 위해.
split.screen( figs = c( 2, 1 ) )
screen(1) #prepare for screen 1 for output #screen 1에 그림을 그려라.
plot(net, edge.arrow.size=.4,vertex.label=NA)
#vertex.label=NA : no labeling for the vertex 


#edge.arrow.size
#arrow size control
screen(2) #prepare for screen 1 for output #아래에 그림을 그려라.
plot(net, edge.arrow.size=1)





#split the window into 1 row, 2 columns
split.screen( figs = c( 1,2 ) )  #행 하나, 열 2개 
net2 <- simplify(net, remove.multiple = F, remove.loops = T) #multiple link를 없애지 마라, 자기가 자기에게 오는건 제거해라.
#control: remove multiple remove loops 단순화 된 그림.
screen(1)
plot(net2, edge.arrow.size=.4)
screen(2)
# Plot with curved edges (edge.curved=.1)
plot(net, edge.arrow.size=.4, edge.curved=.1) #단순화 하기 전에 그림을 그려줌. (단순화 되지 않은 그림)


V(net)$media


# Set edge color to gray, and the node color to orange. 

# Replace the vertex label with the node names stored in "media"

split.screen( figs = c( 1,2 ) )
screen(1) 
plot(net, edge.arrow.size=.2, edge.curved=0,
     vertex.color="orange", vertex.frame.color="red",
     vertex.label=V(net)$관광지, vertex.label.color="black",
     vertex.label.cex=.7)
#net:edge와 꼭지점이 같이 들어가 있는 데이터, edge.curved:curve 줄지 말지.
#vertex.frame.color:주변 둘러싸고 있는걸 빨간색으로
  
screen(2) 
plot(net, edge.arrow.size=.2, edge.curved=0,
     vertex.color="orange", 
     vertex.label=V(net)$관광지, vertex.label.color="black",
     vertex.label.cex=.7)
#frame을 디폴트로 잡으면 검은색으로 나옴.



# Generate colors based on media type:
colrs <- c("gray50", "tomato")
V(net)$군구<-a #꼭지점에 대한 정보.
a<-ifelse(V(net)$군구=='제주시',1,2)
colrs[a] #1,2,3에 따라서 색깔 다르게 주고 싶음.
str(V(net)$군구)
V(net)$군구<-as.factor(V(net)$군구)
V(net)$color <- colrs[V(net)$군구]

nodes
#media type (1;Newspaper) : gray50
#media type (2:TV) : tomato
#media type (3:Online) : gold


#media type에 따라서 색깔을 다르게 지정하고 그림 그려줌. 
plot(net, edge.arrow.size=.2, edge.curved=0,
     vertex.color=V(net)$color,
     vertex.label=V(net)$관광지, vertex.label.color="black",
     vertex.label.cex=.7)





#control the vertex size based on audience size(회사의 규모에 따라서 동그라미 크기를 다르게)
#(larger audience -> larger node).

V(net)$audience.size
 
(V(net)$size <- V(net)$audience.size*0.6)

#vertex.size     
#Size of the node (default is 15)

plot(net, edge.arrow.size=.2, edge.curved=0,
     vertex.color=V(net)$color,
     vertex.size=V(net)$size,   
     vertex.label=V(net)$media, vertex.label.color="black",
     vertex.label.cex=.7)

  




#change the width of the edges(연결선의 폭) 
#based on their weight.
#weight(자기들끼리 얼마나 가중치 큰지)에 따라서 link의 크기를 다르게 해줄 수 있음.

head(links)

(E(net)$width <- 1+E(net)$weight/10)#0의 값을 피하기 위해 10나누고 1을 더한 것.(내가 적절하게 조절해야함)
#edge width default=1 (인데 20은 너무 크니까!)
#edge with is proportional to the computed weights

plot(net, edge.arrow.size=.2, edge.curved=0,
     vertex.color=V(net)$color,
     vertex.size=V(net)$size,   
     vertex.label=V(net)$media, vertex.label.color="black",
     vertex.label.cex=.7)


#주석 넣기
legend(x=-1.5, y=-1.1, c("Newspaper","Television", "Online News"),
 pch=21, pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)
#x=-1.5, y=-1.1: 주석을 넣을 위치.
#pch=21: point character =21(동그라미니까)
#pt.cex: 모양 크기, cex: 글자 크기
#bty="n":박스 안그려줌.



legend('topright', c("Newspaper","Television", "Online News"),
 pch=21, pt.bg=colrs, pt.cex=2, cex=0.8, bty="n", ncol=1)


#location: bottomright, bottomleft,topleft
#


plot(net, edge.arrow.size=.5, edge.curved=0,
     vertex.color=V(net)$color,
     vertex.size=V(net)$size,   
     vertex.label=V(net)$media, vertex.label.color="black",
     vertex.label.cex=.7)


#pt.cex : vertex size for legend
#cex : font size for legend
    

legend(x=-1, y=-1.1, c("Newspaper","Television", "Online News"),
 pch=21, pt.bg=colrs, pt.cex=2.5, cex=1.3, bty="n", ncol=1)
 
 

#vertex.shape="none": no vertex shape 
colrs <- c("black", "tomato", "blue")
V(net)$color <- colrs[V(net)$media.type]
plot(net, edge.arrow.size=.5,vertex.shape="none", edge.curved=0,
     vertex.label.font=2.5,
     vertex.label=V(net)$media, 
     vertex.label.color=V(net)$color,
     vertex.label.cex=.7)
#vertex.shape="none": 동그라미 없애줌.

