################
# Lake Balance Model Core 
# ML Fischer 2023/03
################

# intial volume difference
mulurulu$vold[1] <- 0
garnpung$vold[1] <- 0
leaghur$vold[1]  <- 0
mungo$vold[1]    <- 0
arumpo$vold[1]    <- 0

for (i in 2:time_months) # a loop that simulates 1 to e.g. 120 months
{
  
  month <- (i - 1) %% 12 + 1 # here we get a vector of which month of the year we are
  # 1. lake mulurulu
  # black
  mulurulu$vold[i]   <- influx_mon[month] - (mulurulu$size *  ETa_mon[month]/1000000) #lake level is old lake leve of previous month + influx - ET
  mulurulu$lev[i]    <- mulurulu$lev[i-1] + (1000* mulurulu$vold[i]/mulurulu$size)
  mulurulu$out       <- 0
  if (mulurulu$lev[i]<0) {mulurulu$lev[i] <- 0}
  
  if (mulurulu$lev[i]>mulurulu$maxlevel) #if the lake reaches its overflow sill
  {
    mulurulu$out    <- (mulurulu$lev[i] - mulurulu$maxlevel) * mulurulu$size/1000 #the outflow is the difference between the hypothetical reached lake level and the overflow level
    mulurulu$lev[i] <- mulurulu$maxlevel
  }
  
  # 2. lake garnpung
  # red
  garnpung$vold[i]   <- mulurulu$out  - (garnpung$size *  ETa_mon[month]/1000000) #lake level is old lake leve of previous month + influx - ET
  garnpung$lev[i]    <- garnpung$lev[i-1] + (1000* garnpung$vold[i]/garnpung$size)
  garnpung$out       <- 0
  if (garnpung$lev[i]<0) {garnpung$lev[i] <- 0}
  if (garnpung$lev[i]>garnpung$maxlevel) #if the lake reaches its overflow sill
  {
    garnpung$out    <- (garnpung$lev[i] - garnpung$maxlevel) * garnpung$size/1000 #the outflow is the difference between the hypothetical reached lake level and the overflow level
    garnpung$lev[i] <- garnpung$maxlevel
  }
  
  
  # 3. lake leaghur
  # green
  leaghur$vold[i]   <- garnpung$out  - (leaghur$size *  ETa_mon[month]/1000000) #lake level is old lake leve of previous month + influx - ET
  leaghur$lev[i]    <- leaghur$lev[i-1] + (1000* leaghur$vold[i]/leaghur$size)
  leaghur$out       <- 0
  if (leaghur$lev[i]<0) {leaghur$lev[i] <- 0}
  if (leaghur$lev[i]>leaghur$mungolevel & mungo$lev[i-1]<=mungo$mungolevel ) #if the lake reaches its overflow sill, and mungo is still below its sill
  {
    leaghur$out    <- (leaghur$lev[i] - leaghur$mungolevel) * leaghur$size/1000 #the outflow is the difference between the hypothetical reached lake level and the overflow level
    leaghur$lev[i] <- leaghur$mungolevel
    
  }
  
  # 4. lake mungo
  # blue
  mungo$vold[i]   <- leaghur$out  - (mungo$size *  ETa_mon[month]/1000000) #lake level is old lake leve of previous month + influx - ET
  mungo$lev[i]    <- mungo$lev[i-1] + (1000* mungo$vold[i]/mungo$size)
  mungo$out       <- 0
  
  if (mungo$lev[i]<0) {mungo$lev[i] <- 0}
  if (mungo$lev[i]>mungo$mungolevel) #if mungo reaches the connective level, then both lakes react
  {
    #mungo vold is here for both lakes, they get split by 
    mungo$vold[i]        <- garnpung$out - (leaghur$size *  ETa_mon[month]/1000000) - (mungo$size *  ETa_mon[month]/1000000) 
 
    mungo$lev[i]      <- mungo$lev[i-1]   + (1000* mungo$vold[i]* (mungo$size/  (leaghur$size + mungo$size))         /mungo$size)
    leaghur$lev[i]    <- leaghur$lev[i-1] + (1000* mungo$vold[i]* (leaghur$size/  (leaghur$size + mungo$size))       /leaghur$size)

    if (mungo$lev[i]   >mungo$maxlevel)     {
      mungo$out    <- (mungo$lev[i] - mungo$maxlevel) * mungo$size/1000 #the outflow is the difference between the hypothetical reached lake level and the overflow level
      mungo$lev[i] <- mungo$maxlevel
    }
    if (leaghur$lev[i] >leaghur$maxlevel) {
      leaghur$lev[i] <- leaghur$maxlevel
      leaghur$out    <- (leaghur$lev[i] - leaghur$maxlevel) * leaghur$size/1000 #the outflow is the difference between the hypothetical reached lake level and the overflow level
      
      }
  }
  
  
  # 5. lake arumpo
  # orange
  arumpo$vold[i]   <- mungo$out   - (arumpo$size *  ETa_mon[month]/1000000) #lake level is old lake leve of previous month + influx - ET
  arumpo$lev[i]    <- arumpo$lev[i-1] + (1000* arumpo$vold[i]/leaghur$size)
  arumpo$out       <- 0
  
  if (arumpo$lev[i]<0) {arumpo$lev[i] <- 0}
  if (arumpo$lev[i]>arumpo$maxlevel) #if the lake reaches its overflow sill
  {
    arumpo$lev[i] <- arumpo$maxlevel
  }
  
  
}