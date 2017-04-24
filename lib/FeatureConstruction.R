#This is a function that read in raw data sets and output feature data file
#input: my.dat is a n by m matrix, where n is the number of observations, and m is the possible features we can work on
my.dat = read.csv("../output/auctionItems.csv",header = TRUE, stringsAsFactors = FALSE)

#my.dat = read.csv("../output/testing.csv",header = TRUE, stringsAsFactors = FALSE)
History = c("Christ","Muhammad","Cross","Crusade","Jerusalem","Maria","Virgin","holy","Trinity","Judgement","God", "Jesus","Zeus",
                       "Hera","Poseidon","Demeter","Ares","Athena","Apollo","Artemis","Hephaestus","Aphrodite","Hermes","Dionysus","Hades","Hypnos",
                       "Nike","Janus","Nemesis","Iris","Hecate","Tyche", "Jupiter", "Neptune", "Juno", "Mars", "Venus", "Bellona", "Minerva", "Janus", 
                       "Vesta","Pope", "Chapel", "Church", "Altarpiece","Creation","Birth","Massacre","Death","Demise","Suicide","Conspiracy","Assumption",
                       "Supper","Oath","Battle","War","Creation", "Birth","Massacre","Death","Demise","Suicide","Conspiracy",
            "Assumption","Supper","Oath","Battle","War","Allegory","Religion","Religious","Mythological","Mythology")
Portrait = c("In","with","of","syndics","Librarian","Emperor","Gardner","Sir","Ambassadors","Nobles","IronSmith","General"
             ,"Slave","Hauler","Solider","Bride","Portrait","Spring","Fall","Summer","Winter")
Landscape = c("Landscape","Pastoral","WinterLanscape","Seascape","seascape","Cityscape","cityscape","landscape","Townscape","Figurative")
Genre = c("Genre","Genre scene","Genre", "Maritime","Lighting")
StillLife = c("Still Life","Stilllife","Still life","Still life","Still","still","Mirrors","Chair","Chairs/armchairs",
              "Sofa","Table","Bookcases/shelves,Steps","Bed","Flatware","Commodes/chestofdrawers","chest","Othertablewares","WineRelated",
              "Tea&coffee","Box,Tea&coffee","Alchol","Animal","Flower","flower","Nude","Furniture",
              "Tea","coffe","Tankards","Mugs","Basket","bowls","Birds","Plates","Clock","Fireplace","Bench","Vase","Vases","table","tablewares","flatware","plates")
Abstract = c("Abstract","abstract")
#Other


#1 2 3 4 5 6 7
#Cycle through all the observations, and we see which one has the match, and then we check the ones that are not matched.
my.dat$Type_Category = NA
score = matrix(0,nrow=dim(my.dat)[1], ncol=7)
for(i in 1 : dim(my.dat)[1])
{
  my.dat$type[i] = gsub("[[]|[]]","",my.dat$type[i])#Remove the bracket
  my.dat$type[i] = gsub("['|[']","",my.dat$type[i])#Remove the ''
  #my.dat$type[i] = gsub("[ ]","",my.dat$type[i])#Remove the space
  info = unlist(strsplit(my.dat$type[i],split = " |,"))
  n = length(info)
  if(n!=0){
  for(j in 1 : n)
  {
   if(info[j] %in% History)
    {score[i,1] = score[i,1] + 1}#Type History
    if(info[j] %in% Portrait)
    {score[i,2] = score[i,2] + 1}#Type Protrait
    if(info[j] %in% Landscape)
    {score[i,3] = score[i,3] + 1}#Type landscape
    if(info[j] %in% Genre)
    {score[i,4] = score[i,4] + 1}#Type Genre
    if(info[j] %in% StillLife)
    {score[i,5] = score[i,5] + 1}#Type Stilllife
    if(info[j] %in% Abstract)
    {score[i,6] = score[i,6] + 1}#Type Abstract
  }}
  if(n==0 | sum(score[i,])==0){
  score[i,7] = score[i,7] + 1}#Type other
  #"Animal,Figurative"
  #"Animal,Stilllife"
}

variable.type = apply(score,1,which.max)
#problem = which(rowSums(score)== 0)

#problem = which(score[,7] > 0)


#PROBLEM = my.dat$type[problem] 

#problem = which(rowSums(score)>1)

#PROBLEM = my.dat$type[problem] 



######################After data cleanning run this
variable.diff = (as.numeric(my.dat$highEst) - as.numeric(my.dat$lowEst))/as.numeric(my.dat$lowEst)
#######################

#lot-description

#Deal with figure by figure inches/inch.
Famous = c("signed","inscribed","stamped","marked")
variable.famous = rep(0,dim(my.dat)[1])
variable.height = rep(NA,dim(my.dat)[1])
variable.width = rep(NA,dim(my.dat)[1])
testing = list()           # dim(my.dat)[1])
for(i in 1 : dim(my.dat)[1])
    #dim(my.dat)[1])#Change 500 to appropriate number of observations
{
  my.dat$lot_desc[i] = gsub("[()]","",my.dat$lot_desc[i])
  upper.bound = gregexpr("[i]{1}[n]{1}[.]?[c]+[h]+[e]+[s]+",my.dat$lot_desc[i])[[1]][1]
  if(upper.bound != -1){
  low.bound = upper.bound - 20
  index = gregexpr("[0-9]+[/]?[1-9]?",my.dat$lot_desc[i])
  figures = unlist(regmatches(my.dat$lot_desc[i],gregexpr("[0-9]+[/]?[1-9]?",my.dat$lot_desc[i])))
  figures = figures[index[[1]] > low.bound & index[[1]] < upper.bound]
  testing[[i]] = figures
  info = unlist(strsplit(my.dat$lot_desc[i],split = " "))
  n = length(info)
  for(j in 1 : n)
  {
    if(info[j]%in%Famous) #Possible errror info might be null
    {
      variable.famous[i] = 1
      break
    }
  }
  n = length(figures)
  index = c()
  for(k in 1 : n){
    figures[which(is.na(figures))] = 0
    if(regexpr("/",figures[k])[[1]] != -1){
      first = as.numeric(unlist(strsplit(figures[k],split="/")))[1]
      second = as.numeric(unlist(strsplit(figures[k],split="/")))[2]
      figures[k-1] = as.numeric(figures[k-1]) + (first/second)
    }else{
    figures[k] = as.numeric(figures[k])
    index = c(index,k)}
  }
  figures = as.numeric(figures[index])
  variable.height[i] = figures[1]
  variable.width[i] = figures[2]
  }
}

