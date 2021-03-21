library(dplyr)
library(ggplot2)

df <- read.csv("Bikes_in_Buildings_Requests.csv")



bikereq<- df%>%group_by(OwnerName)%>%summarize(n=n())%>%arrange(desc(n))%>%head(10) # save this dataset


ggplot(data = bikereq)+geom_bar(aes(x = OwnerName,y=n),stat = "identity")+coord_flip()


# testing out the pending vs hold
# Let the Borough be the Filter

# Let the filter be the number of bycycles requested

ggplot(data = df)+geom_bar(aes(x = RequestStatus,fill=RequestStatus))
  
