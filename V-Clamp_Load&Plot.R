library(tidyr)
library(dplyr)
library(ggplot2)
library(plotrix)

## The data comes out of the roboocyte in the following structure: Each voltage run is saved as an independent column in a .dat file.
# the voltage is not saved, need to know how the protocol was in order to know what trace corresponds to which step. Make explicit the V steps made in the vector "vSteps" in the "Roboocyte2dataframe" function.
home = getwd()
comma2point <- function(dataFrameIn) {
  dataFrameOut = NULL
  for (i in 1:ncol(dataFrameIn)){
    iCol = dataFrameIn[,i]
    iCol <- as.vector( as.numeric(gsub(",",".",iCol)) )
    dataFrameOut  <-  cbind(dataFrameOut, iCol)
  }
  dataFrameOut <- as.data.frame(dataFrameOut)
  names(dataFrameOut) = names(dataFrameIn)
  return(dataFrameOut)
}

Roboocyte2dataframe <- function(OOin, Duration){
  print(OOin)
  stepDuration = Duration*100 # duration in s/100
  ooTrace = data[,grep(OOin, names(data))]
  samplingRate = ooTrace[1,1]
  ooTrace = ooTrace[2:stepDuration,] # remove first row
  # Organize data
  trace = ooTrace
  trace = gather(trace ,"name","Current[nA]",1:ncol(trace )) # make 2 columns only
  trace$name = substr(trace$name,1,nchar(trace$name)-5) # remove the ".nA" in the name
  trace  = separate(trace , "name", c("Oocyte","Run")) # get all the runs.
  # make time vector
  trace$`Run` = as.numeric(trace$`Run`)
  time = seq(1,nrow(ooTrace),1)/100
  
  #head(trace)
  
  timeLong = NULL
  for (i in unique(trace$Run)){
    timeLong = append(timeLong, time)
  }
  trace $time = timeLong
  trace$`Current[nA]` = as.numeric(trace$`Current[nA]`)
  
  #
  # Make steps correspond to the voltages. replaces the number w/ the voltage value
  Vindex = 1 
  run = 0 # counter of the IVs runned
  perfusionindex = 2
  IVid = 1
  trace$Perfusion = "235 mOsm"
  trace$"V-step" = 0
  loopRuns = unique(trace$Run) 
  repetitions =  (length(loopRuns)-length(vSteps))/length(vSteps)/(length(Solutions)-1) ## Repetitions per perfusion. The first is done only once. Then each is done this many times. the first run is sol1, next (repetitions-1)/2 is sol2, the following (repetitions-1)/2 is washout. 

  for(i in loopRuns){
    # include the vsteps for every run 1:(length(unique(trace$`Run`))/length(vSteps))
    print(paste(run,Vindex))
    print(vSteps[Vindex])
    trace$`V-step`[which(trace$`Run`==i)] = vSteps[Vindex]
    Vindex = Vindex + 1
    
    # include the information of the perfusion
    # first reps IVs is sol1
    if(run == 0){# first solution only runs one IV   #(i <= length(vSteps) ){
      trace$Perfusion [which(trace$`Run` == i)] = Solutions[1]
      print(Solutions[1])
    }
    
    # next, every 3 IVs are a new solution 
    else if(run>0 && run<=repetitions){  #(i > length(vSteps) && i <= length(vSteps)* ((repetitions-1)/2+1)  ){
      trace$Perfusion [ which(trace$`Run` == i) ] = Solutions[perfusionindex]
      
      print(Solutions[perfusionindex])
    }
    # # Final reps IVs are sol3
    # else if(run ){ #(i > length(vSteps)*((repetitions-1)/2+1) ){
    #   
    # }
    trace$Run[which(trace$`Run`==i)] = IVid  ## do i even need this? -yes, to plot each run separately
    if (Vindex > length(vSteps)){ ## after the END of all the Vsteps, restart the Vindex and pass to the next IV run
      Vindex = 1
      run = run + 1
      IVid = IVid + 1
    }
    if(run>repetitions){
      run = 1
      perfusionindex = perfusionindex + 1
    }
  }
  return(trace)
} # OOin is the name of the input oocyte i.e. "A1". 
##  Duration is the length in seconds of one pulse including the holding voltage steps before and after the pulse. i.e. 500ms+3000ms+500ms = 4000ms = 4 s. 4 should be used. 
##  repetitions is the amount of IVs done per perfusion. the data from the roboocyte has to be loaded and saved as "data". 

cutCtransients <- function(trace){
# for each run
  for (run in unique(trace$Run)){
# for each voltage
    subSetA = dplyr::filter(trace, trace$Run==run) 
    for (iV in unique(subSetA$'V-step')[- which( unique(subSetA$`V-step`) == as.character(Vh) )]  ){
# skip the  loop for step at the holding potential
      if (iV == as.character(Vh) ){
        break
      }
      subSetB = dplyr::filter(subSetA, subSetA$'V-step'==iV) 
      #plot(subSetB$time, subSetB$`V-step`, type="l")
      plot(subSetB$time, subSetB$`Current[nA]`, type = "l")
#  positive (left) transient index
      CapTransientMax =  min( which(subSetB$`Current[nA]`== max(subSetB$`Current[nA]`) ) )
# negative (right) transient index
      CapTransientMin = min( which(subSetB$`Current[nA]`== min(subSetB$`Current[nA]`) ) )
      
      # Do the cutting if the min and max are out of the range bl - sscurrent ± 10%
      baseline = mean(subSetB$`Current[nA]`[1:50])
      ssPulse = mean(subSetB$`Current[nA]`[300:350])
      # first determine if current is outward or inward
      if ( baseline < ssPulse){
        I.direction = 1 # outward
        low = baseline*1.1
        high = ssPulse*1.1
      } else if (baseline > ssPulse){
        I.direction = -1 # inward
        low = ssPulse*1.1
        high = baseline*1.1
      }
      if( max(subSetB$`Current[nA]`) > high && min(subSetB$`Current[nA]`) < low ) {
        # make current values 1 row up and down of the peak equal to the mean before the transient
        subSetB$`Current[nA]`[(CapTransientMax-1):(CapTransientMax+1)] = 
          mean( subSetB$`Current[nA]`[1:(CapTransientMax-5)] ) 
        # make current values 1 row up and down of the peak equal to the  vstep
        subSetB$`Current[nA]`[(CapTransientMin-1):(CapTransientMin+1)] = 
          mean( subSetB$`Current[nA]`[(CapTransientMin+5):nrow(subSetB) ] ) 
        # get index between beginning and end of the subset
        index1 = min(which(trace$Run == run & trace$`V-step` == iV))
        index2 = max(which(trace$Run == run & trace$`V-step` == iV))
        # change data in the current column between indeces 1,2 for the new subSetB
        trace$`Current[nA]`[index1:index2] = subSetB$`Current[nA]`
        # print(run)
        # print(iV)
        # plot(trace$time[index1:index2], trace$`Current[nA]`[index1:index2], type = "l")
        # readline(prompt="Press [enter] to continue")
      }
    }
  }
  print("Transients trimmed")
  return(trace)
} # cuts the transients of all the traces. trace is the input trace organized by "Roboocyte2dataframe". Vh is the holding potential of the experiment

## As input gets the traces from oocyte processed data ("A1-traces.csv"). Identifies for each trace the last run of each perfusion, averages the last second of the pulse and plot the IV. Returns a data frame with the V, I, Run, Perfusion for the inputted oocyte.
IVxTreatment <- function(trace){
  Is = c()
  Vs = c()
  Sols =c()
  runs = c()
  ## Get the last run before the perfusion was changed
  for (iTreatment in unique(trace$Perfusion) ){
    lastRun = trace$Run[ max(which(trace$Perfusion == iTreatment)) ]
    subSetA = dplyr::filter(trace, trace$Run == lastRun)
    # divide by step
    for ( iV in unique(subSetA$V.step) ){
      subSetB = subSetA[which(subSetA$V.step==iV),]
      
      # get mean
      Is = append(Is,mean(subSetB$Current.nA.[250:350]))
      Vs = append(Vs, subSetB$V.step[1])
      Sols = append(Sols, subSetB$Perfusion[1])
      runs = append(runs, subSetB$Run[1])
    }
  }
  
  IV = data.frame("Current nA" = Is, "Voltage mV" = as.numeric(Vs), "Solution" = Sols, "Run" = as.character(runs))
  return(IV)
}

###


########################################################
# MODIFY before start

list.files()
data = read.delim( "230720.dat" , sep = ";")
date = "230720"
vSteps = c("-100","-80","-60","-40","-20","0","20","40","60")
Solutions = c("235 mOsm","100 mOsm","235 mOsm washout",
              "253 mOsm+TEA10mM","110 mOsm+TEA10mM","253 mOsm washout+TEA10mM")
Vh = -60 #holding potential
names(data)


########################################################

# Make a df for each oocyte. Save a plot for every run.

oocytes = unique(substr(names(data),1,2) )
####### If want to omit oocytes, include them in the following vector.####
omitOocyte = c()
oocytes =  oocytes[which(oocytes %in% omitOocyte == FALSE)]

My_Theme =  theme( ######
                   
                   #axis.text.x = element_text(color = "black", size = 20, angle = 0, hjust = 0, vjust = 0.5, face = "plain"),
                   #axis.text.y = element_text(color = "black", size = 20, angle = 0, hjust = 0 , vjust = 0.5, face = "plain"),
                   #axis.title.x = element_text(color = "black", size = 30, angle = 0, hjust = 0.5, vjust = 0, face = "plain"), #element_blank()
                   #axis.title.y = element_text(color = "black", size = 30, angle = 0, hjust = 0, vjust = 0.5, face = "plain"),
                   plot.title = element_text(color = "black", size = 20, angle = 0, hjust = 0.5, vjust = 0, face = "bold"),# element_blank() 
                   legend.text = element_text(size = 20),# element_blank(), # 
                   legend.title = element_text(hjust = 0.1, size = 20),# element_blank(), # 
                   panel.grid.major.y = element_line(), # element_blank(),
                   panel.grid.minor.y = element_blank(),
                   panel.grid.major.x = element_blank(), #
                   panel.grid.minor.x = element_blank()) 
##### 
# oocytes = oocytes[-which(oocytes == "B7")]
# ooi = oocytes[1]
### Plot all the traces and save them






for (ooi in oocytes){ ## loop through oocytes
  # Organize raw data into dataframe
  trace = Roboocyte2dataframe(ooi, 4)
  # cut transients - Making function for that
  trace <-  cutCtransients(trace) 

  ## Plot and save
  # separate by "Run". to plot the v-steps during one perfusion.
  for (run in unique(trace$Run)){ # plot each run
    iplot <- dplyr::filter(trace, trace$Run==run) %>%
      ggplot(aes(`time`, `Current[nA]`, color=`V-step` )) +
      geom_line() +
      ggtitle(dplyr::filter(trace, trace$Run==run)$Perfusion[1]) +
      xlab("Time (s)") +
      scale_color_discrete(breaks=rev(vSteps)) +
      My_Theme
    print(iplot)

    #readline(prompt="Press [enter] to continue")
    #Save
      if (dir.exists(paste(date,"traces",sep = ""))==0 ){
        dir.create(paste(date,"traces",sep = ""))
      }
      ggsave(paste(date,"traces/",ooi,"-trace",run,".pdf",sep = ""), iplot, device = "pdf")
      write.csv(trace, paste(date,"traces/",ooi,"-traces",".csv",sep = ""), row.names = FALSE )
  } 
} # modifies the data and trimms transients and saves images for each run and .csv files for each oocyte.




#
##
###
#### Plot IV
###
##
#




## do one or more (or all!) the oocytes processed previously. Just put the .csv files in the home folder.
list.files() # make sure they are here.

# loop through all those files.
allIVs = NULL

for (file in list.files()[grep( "-traces.csv", list.files())] ){
  print(file)
  trace = read.csv(file)
  ooi = substr( file, 1,2 )
  IV = IVxTreatment(trace)
  ggplot(IV, aes(`Voltage.mV`,`Current.nA`, color = `Solution`))+
    geom_line() +
    geom_point()+
    ggtitle(paste("Oocyte ",ooi)) +
    ylab("Current (nA)") +
    xlab("Voltage (mV)")
  
  ggsave(paste(ooi,"-IV",".pdf",sep = ""), device = "pdf")
  IV$Oocyte = ooi
  allIVs = rbind(allIVs, IV)
} ### makes a plot for each oocyte, creates a csv file for all the IVs.
write.csv(allIVs, paste ( date,"-IVs.csv", sep = "" ), row.names = FALSE)


 #
##
###
#### Pool and stats
###
##
#
#allIVs = read.csv("221110-IVs.csv"  )
list.files()
load( "221110.RData" )
# Remove oocyte from analysis
allIVs = allIVs[-c(which(allIVs$Oocyte == "D7")),]


# Make lists with the wells of the same injection. You can use the following function.
##
makeOOlist <- function(letters = c("A","B","C","D","E","F","G","H"), numbers){
  
  OOlist = NULL
  for(n in numbers){
    OOlist = append( OOlist, c(unite(data.frame(letters, number = rep(n,8) ), "well", sep = ""))[[1]] )
  }
  return(OOlist)
}# for all the numbers add the letters A-H and stack in a list


Injections = list(
  "H2O" = makeOOlist(numbers = c(1) ), #append(makeOOlist(numbers = c(1) ),c("A8","B8","C8","D8" )) ,# c(), #
  "MSL10" = makeOOlist(numbers = c(2,5) ),
  "TIP1" = makeOOlist(numbers = c(3,6)),
  "MSL10TIP1" = makeOOlist(numbers = c(4,7))
)
Injections

IStats = NULL
for(i in 1:length(Injections) ) {
  print(paste(names(Injections)[i]))
  for(sol in unique(allIVs$Solution)  ){
    for(v in unique(allIVs$Voltage.mV) ){
      iIVs = dplyr::filter( allIVs, Oocyte %in% Injections[[i]] & 
                            Solution == sol &
                            Voltage.mV == v) 
      nInjection = nrow(iIVs)
      
      # Don't keep the data for those with 1 or no replicates. Can't do stats on that.
      if ( nInjection <= 1 ){
        print("none or only 1 oocyte for this construct. Skiped from stats")
        break
      } else {
        
        
        
        # Calculate mean & SEM and append to the row with "Voltage.mV","Solution","Run"
        IStats = rbind(IStats,  cbind(iIVs[1,2:3],
                                      "Imean" = mean(iIVs$Current.nA),
                                      "Isem" = std.error(iIVs$Current.nA),
                                      "Construct" = names(Injections)[i],
                                      "n" = nInjection) )
        
        
        
        
      }
    }
  }
} 


# Save injections and traces in a Rdata file.
save(Injections, allIVs, IStats, file = paste ( date,".RData", sep = "" ) )
###
##
#
# Plot everything
unique(IStats$Construct)


plotcRNA =  "MSL10TIP1" #  "TIP1" #   "MSL10" # "H2O"  #  
n = dplyr::filter(IStats, Construct == plotcRNA)$n[1]
dplyr::filter(IStats[ which(grepl("TEA", IStats$Solution) == TRUE) ,], Construct == plotcRNA ) %>% 
  ggplot(aes(`Voltage.mV`, `Imean`, color = `Solution`)  ) +
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin=`Imean`-`Isem`, ymax=`Imean`+`Isem`), width=1.5 ) +
  ggtitle(paste(plotcRNA,"injected n=",n,sep = " ")) +
  xlab("Voltage (mV)")+
  ylab("Current (nA)") +
  My_Theme


ggsave( paste(plotcRNA,"IVS+TEA.pdf",sep = ""))

My_Theme =  theme( ######
                   axis.text.x = element_text(color = "black", size = 20, angle = 0, hjust = 0, vjust = 0.5, face = "plain"),
                   axis.text.y = element_text(color = "black", size = 20, angle = 0, hjust = 0 , vjust = 0.5, face = "plain"),
                   axis.title.x = element_text(color = "black", size = 15, angle = 0, hjust = 0.5, vjust = 0, face = "plain"), #element_blank()
                   axis.title.y = element_text(color = "black", size = 15, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain"),
                   plot.title = element_text(color = "black", size = 20, angle = 0, hjust = 0.5, vjust = 0, face = "bold"),# element_blank() 
                   legend.text = element_text(size = 15),# element_blank(), # 
                   legend.title = element_text(hjust = 0.1, size = 20),# element_blank(), # 
                   panel.grid.major.y = element_line(), # element_blank(),
                   panel.grid.minor.y = element_blank(),
                   panel.grid.major.x = element_line(), #
                   panel.grid.minor.x = element_blank() ) 
######






