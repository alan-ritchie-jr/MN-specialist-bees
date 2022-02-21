#install.packages("tidyverse")
library(tidyverse)
library(waffle)
### PULL IN DATA

oligo.bees<- read_csv("Downloads/Known MN Species-Table 1.csv")
bee.database<-read_csv("Downloads/MNBeeDataBase.csv")

bee.database$genus<- recode(bee.database$genus, "calliopsis" = "Calliopsis")
levels(as.factor(bee.database$genus))

#filter out bee genera to add in bee families for bee family x specialist figure

genera<-bee.database%>%select(genus)%>%group_by(genus)%>%summarize()
#export to manually add in family
#write.csv(genera,"Downloads/bee-genera-family.csv")

#now pull bee-genera-family.csv with manually populated families back in.
bee.fams<-read.csv("Downloads/MN-bee-families.csv")
#bee.fams<-bee.fams[,-1] # drop row names; not sure how to get rid of rn
# drop NA
bee.fams<-drop_na(bee.fams)
levels(as.factor(bee.database$habitat))
### MERGE DATA

#first lets do a summary of the database
# 1. how many studies (n_distinct datasets
n_distinct(bee.database$dataset) # 20 total! 1 DNR. 6 musuems. 1 private collection
# 2. how many bees
n_distinct(bee.database$specimenID) # 117216 specimens
# 3. how many specimens per species? how many datasets are each found in?

bee.species<-bee.database%>%select(specimenID,dataset,genus, species,bee)%>%group_by(genus, species,bee)%>%
  summarize(n_specimens=n_distinct(specimenID),n_datasets=n_distinct(dataset))%>%filter(bee!="NA_NA")
#check for duplicate species
duplicated(bee.species$bee)# no duplicates!

#drop bee column
bee.species<-bee.species[,-3]
#

##Summarize the bee database into one of each species (tally n specIDs for each species)
# join bee fams with database
bee.species.fams<-full_join(bee.species,bee.fams, by="genus")
# join specialists with bee fam
bee.species.full<-left_join(bee.species.fams,oligo.bees,by=c("genus","species","family"))%>%
  select(genus,species,family,n_specimens,n_datasets,"oligolectic on",Plant_family)

colnames(bee.species.full)

specialists<-bee.species.full%>%filter(specialization!="polylectic")%>%summarize(n_specimens,n_datasets,"oligolectic on", Plant_family)
#code NA in plant family to be "polylectic"
#first make a new duplicate column
bee.species.full<-mutate(bee.species.full,specialization=Plant_family)
#for stacked bar of species by plant family over polylectic species
bee.species.full$Plant_family[is.na(bee.species.full$Plant_family)] <- "polylectic"

#for a simple poly vs specialist pie chart use "specialization"
#convert plant family name to "oligolectic"
bee.species.full$specialization[!is.na(bee.species.full$specialization)] <- "oligolectic"
bee.species.full$specialization[is.na(bee.species.full$specialization)] <- "polylectic"


#first figure->pie chart of generalist to specialists

ggplot(bee.species.full,aes(x="",y=specialization, fill=specialization)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
theme_void() + 
  theme(legend.position="none") 
  
#install.packages("waffle")
library(waffle)
bee.oligo.summary<-bee.species.full%>%group_by(specialization)%>%summarize(n=n())
bee.oligo<-c(Generalist=379,Specialist = 98)
98+379
waffle(bee.oligo, rows = 20 ,colors = c( "#8dd3c7","#fb8072"))+
  guides(fill = guide_legend(title = "Dietary Specialization")) +
  theme(legend.text=element_text(size=30))+
  theme(legend.title=element_text(size=40))
#nice! 
#histogram; specialists vs generalists found in each study
bee.species.full$specialization = factor(bee.species.full$specialization,levels=c("specialist","generalist"))

ggplot(bee.species.full,aes(x=family, fill=specialization, color=specialization)) +
  geom_bar()+
  theme_classic()+
  labs(x="Bee Family", y = "# of Species")+
  theme(axis.title = element_text(size = 25))+
  theme(axis.text = element_text(size = 20))+
  theme(legend.title = element_text(size = 20))

bee.fam.summ<-bee.species.full%>%group_by(family,specialization)%>%tally()
sum(bee.fam.summ$n)
sum(bee.species.full$n_specimens)
# alright on to the plant families 
# so among the oligoleges, what plant families are represented? 
# waffle with each plant family being a different color.
bee.oligo.plants<-bee.species.full%>%filter(Plant_family!="polylectic")%>%
  group_by(Plant_family)%>%tally()

bee.oligo.plant.vector<-setNames(bee.oligo.plants$n, bee.oligo.plants$Plant_family)
#library(colorspace)
library(waffle)
hcl_palettes(plot = TRUE)

q22 <- qualitative_hcl(22, palette = "Set3")
waffle(bee.oligo.plant.vector, rows = 10 ,colors = q22)


# do bar instead; too many colors to make work
# plot of n species per plant family
ggplot(bee.oligo.plants,aes(x=reorder(Plant_family,-n),y=n, fill=Plant_family)) +
  geom_bar(stat="identity", width=1) 

###
library(tidyverse)
bop2<- bee.oligo.plants %>% 
  mutate(csum = rev(cumsum(rev(n))), 
         pos = n/2 + lead(csum, 1),
         pos = if_else(is.na(pos), n/2, pos))
#
#install.packages("ggrepel")
 library(ggrepel)
#pie with n as label
ggplot(bop2, aes(x = "" , y = n, fill = reorder(Plant_family,-n))) +
  geom_bar(aes(x="", y=n, fill=reorder(Plant_family,-n)),width = 3, colour="white", stat="identity") +
  coord_polar(theta = "y") +
  geom_label_repel(data = bop2,
                   aes(y = pos, label = paste0(n)),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Plant Family")) +
  theme_void()
  # too messy; keep for reference

#or pie
ggplot(bee.oligo.plants,aes(x="",y=n, fill=Plant_family)) +
  geom_bar(aes(x="", y=n, fill=reorder(Plant_family,-n)),width = 3, colour="white", stat="identity") +
  coord_polar("y", start=0)+ 
  theme_void()+
  guides(fill = guide_legend(title = "Plant Family")) +
   theme(legend.text=element_text(size=30))+
  theme(legend.title=element_text(size=40))

sum(bee.oligo.plants$n)
14/98
