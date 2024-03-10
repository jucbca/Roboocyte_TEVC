library(haven)
library(tidyr)
library(ggplot2)

# Load .dat file. It's loaded as a dataframe with values as characters. Decimal point as ",". NEED "."!
list.files()
Oocytes = unique(substr(list.files()[grep("mV", list.files())], 1,3))

date = "220407"
firstSolution = 214
secondSolution = NULL
survey = "y" # ask about perfusion order

AllTraces <- NULL
for(i in unique(Oocytes) ) { # loop through all the oocyte names
  for(j in grep(i, list.files()) ){ # loop through the files for each oocyte
    fileName = list.files()[j]
    print(fileName)
    LoadData = read.delim(fileName , sep = "")[,1:2] 
    # Convert the characters to numeric values. 
    dataNames = names(LoadData)
    trace <- NULL
    for (k in c(1:ncol(LoadData)) ) { # For all columns replace ",">"." and change to numeric.
      #print(k)
      trace = cbind (trace, as.numeric( gsub(",",".",LoadData[,k]) )  )
    }
    trace=as.data.frame(trace) # Change resulting matrix to dataframe
    names(trace) = dataNames   # Give names back
    names(trace)[1] = "Time s"
    names(trace)[2] = "Value"
    trace$Oocyte = i # annotate oocyte well
    trace$HoldingV = substr(fileName, nchar(fileName)-4, nchar(fileName)) # annotate holding potential
    trace$date = date # annotate the date
    # annotate solutions perfused
    trace$Solution <- firstSolution
    survey = "y"
    while (survey == "y"){
      userTime <- readline(prompt = "Enter time(s) of solution change ")
      from = min (which(trace$`Time s` == userTime))
      to = max ( nrow(trace) )
      secondSolution <- readline(prompt = "Osmolarity perfused?  ")
      trace$Solution[from:to] = secondSolution
      survey <- readline(prompt = "More solutions perfused? (y/n): ")
    }
    # plot that trace
    savename <- readline(prompt = "What construct was injected? : ")
    tracePlot <- ggplot(trace, aes(`Time s`, `Value`, color = `Solution`)) +
      geom_line() +
      ylab ("I (nA)") + 
      xlab ("Seconds") + 
      ggtitle(savename) +
      theme(axis.text.x = element_text(angle = 0, hjust = 0 ),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_line(),
            panel.grid.minor.x = element_line())
    print(tracePlot)
    trace$RNA = savename
    saveConfirmation <- readline(prompt = "Wanna save? (y/n): ") #
    if (saveConfirmation == "y"){
      ggsave( paste( dirname(getwd()),"/", date,"_",fileName,savename,".jpg",sep = "") )
      AllTraces = rbind(AllTraces, trace)
    }
    
  }
}

#Save csv file with all the traces for that date in the folder with the dates name
write.csv(AllTraces, paste(dirname(getwd()),"/",date,"/DATA.csv", sep = "") )

