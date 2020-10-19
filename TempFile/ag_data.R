library(knitr)
library(tidyverse)
library(magrittr)
#library(kableExtra)

opts_chunk$set(echo = FALSE, 
               warning = FALSE,
               message = FALSE)

## read the data

ag_data <- read_csv("berries.csv", col_names = TRUE)

## look at number of unique values in each column
ag_data %>% summarize_all(n_distinct) -> aa


## make a list of the columns with only one unique value
bb <- which(aa[1,]==1)

## list the 1-unique valu column names 
cn <- colnames(ag_data)[bb]

## remove the 1-unique columns from the dataset
ag_data %<>% select(-all_of(bb))

aa %<>% select(-all_of(bb)) 


## State name and the State ANSI code are (sort of) redundant
## Just keep the name
ag_data %<>% select(-4)
aa %<>% select(-4) 


#kable(head(ag_data)) %>% kable_styling(font_size=12)

berry <- unique(ag_data$Commodity)
nberry <- length(berry)

bberry <- ag_data %>% filter((Commodity=="BLUEBERRIES") & (Period=="YEAR"))
bberry %<>% select(-c(Period, Commodity))   

#### Does every Data Item begin with "
sum(str_detect(bberry$`Data Item`, "^BLUEBERRIES, ")) == length(bberry$`Data Item`)


# di <- bberry$`Data Item`
# di_m <- str_split(di, ",", simplify=TRUE)
# dim(di_m)
# 
# unique(di_m[,1])
# di_m <- di_m[,2:4]

bberry %<>% separate(`Data Item`, c("B","type", "meas", "what"), sep = ",") 
bberry %<>% select(-B)

# head(bberry$type, n=20)
# ty <- str_split(bberry$type, " ", simplify=TRUE)
# head(ty, n=20)

bberry %<>% separate(type,c("b1", "type", "b2", "lab1", "lab2"), " ")

bberry %<>% select(-c(b1,b2)) 

bberry[is.na(bberry)] <- " "  ## OK now Data Item has been split into parts

## onto Domain

# bberry$Domain %>% unique()

bberry %<>% separate(Domain, c("D_left", "D_right"), sep = ", ")

# bberry$D_left %>% unique()
# bberry$D_right %>% unique()

bberry[is.na(bberry)] <- " "

## And now Domain Category


## bberry$`Domain Category` %>% unique()

bberry %<>% separate(`Domain Category`, c("DC_left", "DC_right"), sep = ", ")

## looks like DC_left combines labels

#head(bberry$DC_left %>% unique(),n=20)
#head(bberry$DC_right %>% unique(), n=20)


## work on DC_left first

bberry %<>% separate(DC_left, c("DC_left_l", "DC_left_r"), sep = ": ")

## bberry$DC_left_l %>% unique()
## bberry$DC_left_r %>% unique()

## now work on DC_right

head(bberry$DC_right %>% unique(), n=20)

bberry %<>% separate(DC_right, c("DC_right_l", "DC_right_r"), sep = ": ") 


bberry[is.na(bberry)] <- " "

##  OK now we need to eliminate the redundancy

#bberry$lab2%>%unique()
bberry%<>%select(-c(DC_right_l,DC_left_l))
bberry['Stats']=" "
for(i in 1:length(bberry$Year)){
  if(str_detect(bberry$what[i],"AVG")){
    bberry$Stats[i]=bberry$what[i]
  }
  if(str_detect(bberry$meas[i], "MEASURE")){
    bberry$what[i]=bberry$meas[i]
    bberry$meas[i]=" "
  }
  
}
#%<>%separate(meas,c("Status","Process"),sep="-")

for(i in 1:length(bberry$Year)){
  if(bberry$meas[i]==" "){
    bberry$meas[i]=bberry$lab1[i]
    bberry$lab1[i]=" "
  }
  if(bberry$DC_left_r[i]!=" "){
    bberry$DC_right_r[i]=bberry$DC_left_r[i]
    bberry$DC_left_r[i]=" "
  }
}

#sum(bberry$lab1==" ") == length(bberry$lab1)
bberry%<>%select(-lab1)
#sum(bberry$DC_left_r==" ") == length(bberry$DC_left_r)
bberry%<>%select(-DC_left_r)

#######
for(i in 1:length(bberry$Year)){
  if(str_detect(bberry$meas[i],"ACRES")){
    bberry$what[i]=bberry$meas[i]
    bberry$meas[i]=bberry$lab2[i]
    bberry$lab2[i]=" "
  }
  else if(bberry$meas[i]=="APPLICATIONS"||bberry$meas[i]=="TREATED"){
    bberry$lab2[i]=bberry$meas[i]
    bberry$meas[i]=" "
  }
}



bberry$DC_right_r%<>%str_replace_all("[\\(\\)]","")


bberry%<>%separate(DC_right_r,c("Compound","Coumpound Value"),sep=" = ")
bberry$Value%<>%str_replace_all("\\((.*?)\\)"," ")


#Elimate NA's
bberry[is.na(bberry)] <- " "
#Now the dataset is cleaned.
bberry%<>%rename(Measure=what,Chem=D_left,killer=D_right,Process=lab2,Status=meas)
bberry$Measure%<>%str_trim()
bberry$Status%<>%str_trim()

#kable(head(bberry, n=10)) %>% kable_styling(font_size=12)

write.table(bberry,"bberry.csv",col.names=TRUE,sep=",")