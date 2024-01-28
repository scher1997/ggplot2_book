library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

ggplot(data = world) +
  geom_sf()

ggplot(data = world) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(world$NAME)), " countries)"))

ggplot(data = world) +
  geom_sf(aes(fill = pop_est)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

colMeans(world$geometry[[1]][[1]][[1]])[1] # X Coord (Set to 2 for Y Coord)

# To get the means of the geom_polygon of the first country, would need to do for each
x <- c()
y <- c()
for (row in 1:nrow(world)){
  x = append(x,colMeans(world$geometry[[row]][[1]][[1]])[1])
  y = append(y,colMeans(world$geometry[[row]][[1]][[1]])[2])
  }



world <- world |> 
  mutate(average_x = x,average_y = y)

# To see colors use colors()

#### FIRST SET OF PLOTS FOR NICO
set.seed(123)
ggplot(data=world) +
  geom_sf(fill = 'tan',color='black') +
  geom_point(data = filter(world,pop_est<100000000 & pop_est>10000000),aes(x=average_x,y=average_y,size=pop_est/100),color='tomato2',alpha=0.6) + 
  xlab('')+
  ylab('')+
  labs(title='FY2017',size='Number of Deployments')+
  theme(plot.title.position = 'plot',plot.title=element_text(hjust=0.425))+
  scale_size_binned(labels=c('12 oz. - Tall','16 oz. - Grande','20 oz. - Venti'))

# ggsave("notional_deployments_2017.pdf")

ggplot(data=world) +
  geom_sf(fill = 'tan',color='black') +
  geom_point(data = filter(world,pop_est<10000000 & pop_est>1000000) |> mutate(pop_est = pop_est-rnorm(1,100000,10000)),
             aes(x=average_x,y=average_y,size=pop_est/10),color='tomato2',alpha=0.6) + 
  xlab('')+
  ylab('')+
  labs(title='FY2018',size='Number of Deployments')+
  theme(plot.title.position = 'plot',plot.title=element_text(hjust=0.425))+
  scale_size_binned(labels=c('12 oz. - Tall','16 oz. - Grande','20 oz. - Venti'))

# ggsave("notional_deployments_2018.pdf")

#### SECOND SET OF PLOTS FOR ALEX
# geom_point(x=0.6,y=52.5,aes(size=pop_est),alpha=0.3,na.rm = TRUE,show.legend = FALSE)+

## All three
ggplot(data=world)+
  geom_sf()+
  coord_sf(xlim = c(-5,20),ylim=c(42,55),expand=TRUE)+
  geom_text(x=0.4,y=52.35,label='Newmarket')+
  geom_point(x=0.6,y=52.5,size=3,alpha=0.01,color='blue4')+ # Newmarket
  geom_point(x=13.4,y=52.5,size=100,color='black',pch=21)+ # berlin
  geom_point(x=13.4,y=52.5,size=100,alpha=0.01,color='tomato2')+ # berlin
  geom_text(x=13.4,y=52.5-0.15,label='Berlin')+
  
  geom_point(x=10,y=53.5,size=60,color='black',pch=21)+ # hamburg
  geom_point(x=10,y=53.5,size=60,alpha=0.01,color='red')+ # hamburg
  geom_text(x=10,y=53.5-0.15,label='Hamburg')+
  
  geom_point(x=6.95,y=50.9,color='black',size=50,pch=21)+ # cologne
  geom_point(x=6.95,y=50.9,alpha=0.01,color='red4',size=50)+ # cologne
  geom_text(x=6.95,y=50.9-0.15,label='Cologne')+
  xlab('')+
  ylab('')

# ggsave("ALEX_ALL_3.pdf")


# Berlin Only
ggplot(data=world)+
  geom_sf()+
  coord_sf(xlim = c(-5,20),ylim=c(42,55),expand=TRUE)+
  geom_text(x=0.4,y=52.35,label='Newmarket')+
  geom_point(x=0.6,y=52.5,size=3,alpha=0.01,color='blue4')+ # Newmarket
  geom_point(x=13.4,y=52.5,size=100,color='black',pch=21)+ # berlin
  geom_point(x=13.4,y=52.5,size=100,alpha=0.01,color='tomato2')+ # berlin
  geom_text(x=13.4,y=52.5-0.15,label='Berlin')+
  xlab('')+
  ylab('')

# ggsave("ALEX_BERLIN.pdf")


# Hamburg Only
ggplot(data=world)+
  geom_sf()+
  coord_sf(xlim = c(-5,20),ylim=c(42,55),expand=TRUE)+
  geom_text(x=0.4,y=52.35,label='Newmarket')+
  geom_point(x=0.6,y=52.5,size=3,alpha=0.01,color='blue4')+ # Newmarket
  geom_point(x=10,y=53.5,size=60,color='black',pch=21)+ # hamburg
  geom_point(x=10,y=53.5,size=60,alpha=0.01,color='red')+ # hamburg
  geom_text(x=10,y=53.5-0.15,label='Hamburg')+
  xlab('')+
  ylab('')

# ggsave("ALEX_HAMBURG.pdf")

# Cologne Only
ggplot(data=world)+
  geom_sf()+
  coord_sf(xlim = c(-5,20),ylim=c(42,55),expand=TRUE)+
  geom_text(x=0.4,y=52.35,label='Newmarket')+
  geom_point(x=0.6,y=52.5,size=3,alpha=0.01,color='blue4')+ # Newmarket
  geom_point(x=6.95,y=50.9,color='black',size=50,pch=21)+ # cologne
  geom_point(x=6.95,y=50.9,alpha=0.01,color='red4',size=50)+ # cologne
  geom_text(x=6.95,y=50.9-0.15,label='Cologne')+
  xlab('')+
  ylab('')

# ggsave("ALEX_COLOGNE.pdf")

