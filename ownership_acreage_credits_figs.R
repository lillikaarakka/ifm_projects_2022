#Ownership/Credits/Acreage map and ownership graph

#Packages------------------------------------------------------------------------------------
library(usmap)
library(dplyr)
library(sf)
library(sp)
library(ggplot2)
library(scales)

#Files-----------------------------------------------------------------------------------
IMF_projects <- read.csv("IMF_Analysis-CORRECTED.csv", na.strings=c(""," ", NA, "NA"))
acreage_state <- read.csv("Acreage_State.csv")
locations <- read.csv("project_site_locations.csv")

##Data prep--------------------------------------------------------------------------------
colnames(IMF_projects) <- c("ID", "Developer", "Name", "Owner", "Ownership.Type", "Regestered", "Status", "ARB.Status", "County", "State", "Forest.management", "Forest.management.lc","No.Harvest", "Uneven", "Even", "Precommercial.thinning", "Other.thinning", "Selection", "Regeneration", "Retention", "Additional.Management", "TF.PC.Thinning", "Acreage", "Offsets", "Credits per Acre", "Notes")

notepos <- which(!is.na(IMF_projects[,27])) # ID which cell that has the note
IMF_projects[notepos, 27] # confirm note value
# assign orphan note to corresponding cell in Notes col
IMF_projects[notepos, c("Notes")] <- IMF_projects[notepos, 27] 
# drop empty column
IMF_projects <- IMF_projects[,-27]
# all good now?
str(IMF_projects) # yes

#Create ordered factors for mangmnt - No man > Even/uneven > retention > thinning > regeneration
IMF_projects$Mgmt_exclusive <- NA

IMF_projects$Mgmt_exclusive <- ifelse(IMF_projects$No.Harvest == 1, "No commercial harvest", ifelse(IMF_projects$Uneven == 1, "Uneven-aged management", ifelse(IMF_projects$Even == 1, "Even-aged management", ifelse(IMF_projects$Retention == 1, "Retention",  ifelse(IMF_projects$Precommercial.thinning == 1, "Precommercial thinning", ifelse(IMF_projects$Other.thinning == 1, "Thinning", ifelse(IMF_projects$Regeneration == 1, "Regeneration", "No Management")))))))
#convert text to factors
IMF_projects$Mgmt_exclusive <- as.factor(IMF_projects$Mgmt_exclusive)
IMF_projects$Mgmt_exclusive <-factor(IMF_projects$Mgmt_exclusive, levels=c("No commercial harvest", "No Management", "Even-aged Management", "Uneven-aged management", "Thinning", "Precommercial thinning", "Retention", "Regeneration"))

#Combine all private company categories
IMF_projects$Ownership.Combined <- NA

IMF_projects$Ownership.Combined <- ifelse(IMF_projects$Ownership.Type == "Private Company", "Private Company", ifelse(IMF_projects$Ownership.Type == "Private company", "Private Company", ifelse(IMF_projects$Ownership.Type == "Private Company*", "Private Company", ifelse(IMF_projects$Ownership.Type == "Private company**", "Private Company", ifelse(IMF_projects$Ownership.Type == "Private Company**", "Private Company", ifelse(IMF_projects$Ownership.Type == "Government", "Government", ifelse(IMF_projects$Ownership.Type == "University", "University", ifelse(IMF_projects$Ownership.Type == "Individuals", "Individuals", ifelse(IMF_projects$Ownership.Type == "NGO", "NGO", ifelse(IMF_projects$Ownership.Type == "Tribal", "Tribal", NA))))))))))
IMF_projects$Ownership.Combined <- as.factor(IMF_projects$Ownership.Combined)

#Double check it worked
Ownership_check <- data.frame(IMF_projects$ID, IMF_projects$Ownership.Type, IMF_projects$Ownership.Combined)
Ownership_check$Match <- ifelse(IMF_projects$Ownership.Type == IMF_projects$Ownership.Combined, "true", "false")
subset(Ownership_check, Match == "false")


##Stacked bar - ownership type and management ------------------------------------------
#subset IMF_projects
Ownership_graph <- na.omit(IMF_projects[c("ID","Offsets","Mgmt_exclusive", "Ownership.Combined")])
Ownership_graph <-  subset(Ownership_graph, Ownership.Combined != "University")
#group management types together
Ownership_graph <- Ownership_graph[order(Ownership_graph$Mgmt_exclusive),]
Ownership_graph <- na.omit(Ownership_graph)
summary(Ownership_graph)

#graph
OG <-ggplot(data=Ownership_graph, aes(x=Ownership.Combined, y=Offsets, fill=Mgmt_exclusive, color=Mgmt_exclusive))+
  geom_bar(position="stack", stat="identity") + 
  scale_fill_manual(values = c("#8c6bb1", "#1d91c0", "#41b6c4", "#00441b", "#addd8e", "#f5ff71", "#7fcdbb")) +
  scale_color_manual(values = c("#8c6bb1", "#1d91c0", "#41b6c4", "#00441b", "#addd8e", "#f5ff71", "#7fcdbb")) + 
  scale_y_continuous(breaks=c(0,25000000, 50000000,75000000,100000000,125000000), labels=c("0", "25", "50", "75", "100", "125")) + 
  labs(title = "Offsets issued by Ownership and Management Type", fill = "Primary Management Type") + xlab("Ownership Type") + ylab("Offsets Issued (million)") +
  guides(color=FALSE)

OG


##Map summarizingn acraage/state and offsets/project-----------------------------------
#subset needed columns
coordinates <- data.frame(locations$Project.ID, locations$long, locations$lat, locations$Nro.of.offsets.issued)
#set column names
colnames(coordinates) <- c("ID", "locations.long", "locations.lat", "locations.Nro.of.offsets.issued")


#Join to add ownership combined
coords_ownership <- Ownership_graph %>%
  left_join(coordinates, by = 'ID')
str(coords_ownership)

#resubset needed columns
coordinates1 <- data.frame(coords_ownership$locations.long, coords_ownership$locations.lat, coords_ownership$locations.Nro.of.offsets.issued, coords_ownership$Ownership.Combined)



#transform to match US map projection
locations_transformed <- usmap_transform(coordinates1)
#rename columns
colnames(locations_transformed) <- c("long", "lat", "locations.Nro.of.offsets.issued","Ownership.Combined", "locations.long.1", "locations.lat.1")

#import acreage by state and reorganize
colnames(acreage_state) <- c("row label", "acreage", "row number", "fips", "abreviation", "state")
#convert acres to hectares 
acreage_state$hectares <- acreage_state$acreage*0.404686
#fix Maine and Maryland FIPS
acreage_state[20,4] <- 23
acreage_state[21,4] <- 24

#create data frame with fips and hectares
hectares <- data.frame(acreage_state$fips, acreage_state$hectares)
str(hectares)
colnames(hectares) <- c("fips","hectares")
str(hectares)

#create bins for legend
hectares_custombins <- c(-1, 1, 10000, 50000, 100000, 300000, 508000)
hectares <- hectares %>%
  mutate(hectares = cut(hectares, breaks = hectares_custombins))
str(hectares)


#map
options(scipen = 10000)
usp <-plot_usmap(regions = "state", data=hectares, values="hectares") +
  geom_point(data = locations_transformed, aes(x = locations.long.1, y = locations.lat.1, size=locations.Nro.of.offsets.issued), color = "black", alpha = 0.5)

#with only location and number of offsets issued
usp+
  scale_fill_brewer(palette="Greens",name = "Hectares", labels=c("0", "1-9,999", "10,000-49,999", "50,000-99,999", "100,000-299,999", "300,000-508,000")) + 
  labs(title = "Number of Project Acres and Projects per State",
       size = "Number of offsets issued") +
  theme(legend.position = "right") +
  scale_size_continuous(labels=comma)

#with ownership type
us_op <-plot_usmap(regions = "state", data=hectares, values="hectares") +
  geom_point(data = locations_transformed, aes(x = locations.long.1, y = locations.lat.1, size=locations.Nro.of.offsets.issued, color=Ownership.Combined), alpha = 0.7)

png(file = "projects_state.png", width = 8, height = 5, units = "in", res = 350)
us_op +
  scale_fill_manual(values=c("#FFFFFF", "#c7e9c0", "#a1d99b", "#74c476", "#31a354", "#006d2c"),name = "Hectares", labels=c("0", "1-9,999", "10,000-49,999", "50,000-999,999", "100,000-299,999", "300,000-508,000"))+
  scale_color_manual(values = c("#8c6bb1", "#f0027f", "#d95f02", "#1d91c0", "#91003f", "#a65628"))+
  labs(title = "Project Hectares and Projects per State",
       size = "Number of offsets issued",
       color = "Ownership Type") +
  theme(legend.position = "right", legend.spacing.y = unit(.05,"cm"))+
  guides(color = guide_legend(override.aes = list(size=3)))+
  scale_size_continuous(labels=comma)
dev.off()
