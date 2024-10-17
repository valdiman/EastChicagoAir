
# Calculations (functions) ------------------------------------------------
PUF_PAS_Effective_Volume_Calculations <- function(start_date, end_date, met_data_path, pcb_properties_path, pcb_ppLFER_path, file_path) {
  
  # Specify the method for determining KPUF (1 or 2)
  KPUFMethod <- 1
  
  # Known variables about PUF disk parameters
  diameter <- 0.14
  thickness <- 0.0135
  dpuf <- 21173
  As <- 0.0365
  
  # Calculate PUF parameters based on inputs
  VPUF <- pi * ((diameter^2) / 4) * thickness
  
  # Read in site-specific Met data
  Met_Data <- read.csv(met_data_path, header = TRUE)
  
  # Import PCB properties
  PCB_Properties <- read.csv(pcb_properties_path, header = TRUE)
  PCB_ppLFER <- read.csv(pcb_ppLFER_path, header = TRUE)
  
  # Convert the 'date' column in Met_Data to POSIXct format
  Met_Data$date <- as.POSIXct(Met_Data$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  # Now, find the closest start and end times
  StartTime <- which.min(abs(Met_Data$date - as.POSIXct(start_date, tz = "UTC")))
  EndTime <- which.min(abs(Met_Data$date - as.POSIXct(end_date, tz = "UTC")))
  
  # Length of deployment (number of time steps)
  Length <- EndTime - (StartTime - 1)
  
  # Read in Met variables for the above time series
  T <- Met_Data[StartTime:EndTime, 2]     # Temperature (Kelvin)
  P <- Met_Data[StartTime:EndTime, 3]     # Pressure (Pascals)
  WS <- Met_Data[StartTime:EndTime, 4]    # Wind Speed (m/s)
  Qvap <- Met_Data[StartTime:EndTime, 6]  # Water-Vapor Mixing Ratio (kg/kg)
  
  # Prepare deployment-specific meteorological data for output
  DeploymentMetData <- Met_Data[StartTime:EndTime, ]
  
  # Internal flowrate and wind speed exceedance calculation
  InternalFlowrate <- function(WS) {
    Vi <- rep(0, length(WS))
    WScount <- 0
    for (t in 1:length(WS)) {
      if (WS[t] <= 0.9) {
        Vi[t] <- 0
      } else {
        Vi[t] <- 0.3620 * WS[t] - 0.313
        if (WS[t] > 5) {
          WScount <- WScount + 1
        }
      }
    }
    WS_Exceedence <- WScount / length(Vi) * 100
    return(list(Vi = Vi, WS_Exceedence = WS_Exceedence))
  }
  
  result <- InternalFlowrate(WS)
  Vi <- result$Vi
  WS_Exceedence <- result$WS_Exceedence
  
  # Calculate average values for ViGamma and TGamma
  ViGamma <- mean(Vi)
  TGamma <- mean(T)
  
  # Function to calculate GammaPCB
  GammaPCBCalculation <- function(ViGamma, TGamma, LKOA, LKOA28) {
    Gamma28 <- -0.153 + 0.077 * ViGamma + 0.000668 * TGamma - 0.000310 * ViGamma * TGamma
    GammaPCB <- Gamma28 * (LKOA / LKOA28)
    return(GammaPCB)
  }
  
  # Function to calculate PCB diffusivity
  PCBDiffusivity <- function(T, P, mm) {
    VH2O <- 9.5
    VAir <- 20.1
    mH2O <- 18.015
    mAir <- 28.97
    
    D <- (((10^-3 * T^1.75 * ((1 / mAir) + (1 / mH2O))^0.5) / 
             (P * (1 / 101325) * (VAir^(1 / 3) + VH2O^(1 / 3))^2)) * 
            ((mm / mH2O)^-0.5)) / 100^2
    
    return(D)
  }
  
  # Function to calculate kinematic viscosity (m^2/s)
  Viscosity <- function(T, Qvap, P) {
    Za <- 0.038474
    Tca <- 132.206
    Zcw <- 0.231
    Zw <- 0.0192
    Tcw <- 647.4
    Rd <- 287.058
    Rv <- 461.495
    mH2O <- 18.015
    mAir <- 28.97
    
    MuAir <- (17.78 * ((4.58 * (T / Tca) - 1.67)^0.625) * 10^-7) / Za
    MuWater <- ((7.55 * (T / Tcw) - 0.55) * (Zcw^-1.25) * 10^-7) / Zw
    a <- ifelse(T >= 293.15, 2.5, 3.5)
    Mw <- Qvap * (1 / mH2O) * 1000 * (P / (Rd * T))
    Ma <- (1 / Qvap) * (1 / mAir) * 1000 * 1000
    Xw <- Mw / (Mw + Ma)
    Xa <- Ma / (Mw + Ma)
    DynamicMu <- (Xa * MuAir + Xw * MuWater) * (1 + ((Xw - Xw^2) / a))
    Pv <- Xw * P
    Pd <- Xa * P
    Density <- (Pd / (Rd * T)) + (Pv / (Rv * T))
    Kinematic <- DynamicMu / Density
    
    return(Kinematic)
  }
  
  # Function to calculate mass transfer rate
  MassTransfer <- function(Vi, Kinematic, GammaPCB, D, diameter) {
    Beta <- 1 / 3
    alpha <- ifelse(Vi < 0.5, 0.5, 0.9)
    Nu <- Kinematic^(Beta - alpha)
    kv <- GammaPCB * (D^(1 - Beta)) * (Vi^alpha) * Nu * (diameter^(alpha - 1))
    return(kv)
  }
  
  # Function to calculate KPUF
  KPUFCalculation <- function(LKOA, dU, T, method, Congener_ppLFER) {
    if (method == 1) {
      Rg <- 8.3144  # Gas constant in J/mol*K
      
      # Adjust LKOA by temperature
      LKOAi <- LKOA - (dU / (2.303 * Rg)) * ((1 / T) - (1 / 298.15))
      
      # Calculate PUF/Air equilibrium partition coefficient
      KPUF <- 10^(0.6366 * LKOAi - 3.1774)
      
    } else if (method == 2) {
      cs <- -1.279
      ch <- -354.607
      es <- 0.449
      eh <- 179.41
      ss <- -0.745
      sh <- -692.187
      as <- -2.541
      ah <- -1683.56
      bs <- -0.118
      bh <- -33.83
      ls <- -0.456
      lh <- -365.896
      
      E <- Congener_ppLFER[2]  # Assuming correct index for descriptor
      S <- Congener_ppLFER[3]
      A <- Congener_ppLFER[4]
      B <- Congener_ppLFER[5]
      L <- Congener_ppLFER[7]
      
      # Calculate logKPUF using the Abraham model
      logKPUF <- (cs - (ch / T)) + (es - (eh / T)) * E + (ss - (sh / T)) * S + 
        (as - (ah / T)) * A + (bs - (bh / T)) * B + (ls - (lh / T)) * L
      
      # Calculate KPUF (m^3/g)
      KPUF <- 10^logKPUF / 1000000
    }
    
    return(KPUF)
  }
  
                                                                                                                                                            
  numPCB <- nrow(PCB_Properties)
  PAS_Array <- array(0, dim = c(Length, numPCB, 5))
  
  for (C in 1:numPCB) {
    for (t in 1:Length) {
      mm <- PCB_Properties[C, 2]
      dU <- PCB_Properties[C, 3]
      LKOA <- PCB_Properties[C, 4]
      Congener_ppLFER <- PCB_ppLFER[C, ]
      
      GammaPCB <- GammaPCBCalculation(ViGamma, TGamma, LKOA, PCB_Properties[28, 4])
      D <- PCBDiffusivity(T[t], P[t], mm)
      Kinematic <- Viscosity(T[t], Qvap[t], P[t])
      kv <- MassTransfer(Vi[t], Kinematic, GammaPCB, D, diameter)
      Rs <- kv * As * 86400
      KPUF <- KPUFCalculation(LKOA, dU, T[t], KPUFMethod, Congener_ppLFER) * dpuf
      
      if (t == 1) {
        Veff <- (kv * 3600) * As / VPUF * (VPUF - (0 / KPUF))
      } else {
        Veff <- (kv * 3600) * As / VPUF * (VPUF - (PAS_Array[t - 1, C, 5] / KPUF))
      }
      
      PAS_Array[t, C, 1] <- Rs
      PAS_Array[t, C, 2] <- kv
      PAS_Array[t, C, 3] <- KPUF
      PAS_Array[t, C, 4] <- Veff
      
      if (t == 1) {
        PAS_Array[t, C, 5] <- Veff
      } else {
        PAS_Array[t, C, 5] <- Veff + PAS_Array[t - 1, C, 5]
      }
    }
  }
  
  # Create matrix of all variables for each congener
  PAS_Final <- matrix(0, nrow = 2, ncol = numPCB)
  for (C in 1:numPCB) {
    RsAVG <- mean(PAS_Array[, C, 1])
    Veff_Final <- PAS_Array[Length, C, 5]
    PAS_Final[1, C] <- Veff_Final
    PAS_Final[2, C] <- RsAVG
  }
  
  formatted_start_date <- format(as.POSIXct(start_date), "%Y%m%d%H")
  formatted_end_date <- format(as.POSIXct(end_date), "%Y%m%d%H")
  
  rowID <- matrix(nrow = 2, ncol = 6)
  rowID[1, ] <- c('1', formatted_start_date, formatted_end_date, as.character(Length), as.character(WS_Exceedence), 'Veff')
  rowID[2, ] <- c('1', formatted_start_date, formatted_end_date, as.character(Length), as.character(WS_Exceedence), 'SR')
  
  outData <- cbind(rowID, PAS_Final)
  
  column_names <- c("PUF_ID", "Deployment", "Collection", "Length", "WS_Exceedence", "Type", PCB_Properties$Congener.s..ID)
  colnames(outData) <- column_names
  
  # Create the directory if it does not exist
  dir.create(dirname(file_path), recursive = TRUE)
  
  # Save the data frame to a CSV file
  write.csv(outData, file = file_path, row.names = FALSE)
}

# Run the Model -----------------------------------------------------------
PUF_PAS_Effective_Volume_Calculations(
  start_date = "2014-02-20 01:00:00", 
  end_date = "2014-07-01 01:00:00", 
  # Need to select the folder and name of the file where the met data is stored
  met_data_path = "Output/Data/isd_light/725340-14819/725340-14819-2014-1-filled.csv", 
  pcb_properties_path = "Data/PCB_Properties_MW_DU_KOA.csv", 
  pcb_ppLFER_path = "Data/PCB_LFER_descriptors.csv", 
  # Need to add the folder where the result is going to be saved
  # It should be metdataID-year
  file_path = "Output/Data/Results/725340-14819-2014-27/results.csv"
)

colluding_sheet = read.csv("Output/Data/Results/725340-14819-2012-27/results.csv", header = TRUE)
{
  pcb12 = colluding_sheet[1,18]
  pcb13 = colluding_sheet[1,19]
  pcb18 = colluding_sheet[1,24]
  pcb30 = colluding_sheet[1,36]
  pcb20 = colluding_sheet[1,26]
  pcb28 = colluding_sheet[1,34]
  pcb21 = colluding_sheet[1,27]
  pcb33 = colluding_sheet[1,39]
  pcb26 = colluding_sheet[1,32]
  pcb29 = colluding_sheet[1,35]
  pcb40 = colluding_sheet[1,46]
  pcb71 = colluding_sheet[1,77]
  pcb44 = colluding_sheet[1,50]
  pcb47 = colluding_sheet[1,53]
  pcb65 = colluding_sheet[1,71]
  pcb49 = colluding_sheet[1,55]
  pcb69 = colluding_sheet[1,75]
  pcb50 = colluding_sheet[1,56]
  pcb53 = colluding_sheet[1,59]
  pcb59 = colluding_sheet[1,65]
  pcb62 = colluding_sheet[1,68]
  pcb75 = colluding_sheet[1,81]
  pcb61 = colluding_sheet[1,67]
  pcb70 = colluding_sheet[1,76]
  pcb74 = colluding_sheet[1,80]
  pcb76 = colluding_sheet[1,82]
  pcb85 = colluding_sheet[1,91]
  pcb116 = colluding_sheet[1,122]
  pcb86 = colluding_sheet[1,92]
  pcb97 = colluding_sheet[1,103]
  pcb119 = colluding_sheet[1,125]
  pcb109 = colluding_sheet[1,115]
  pcb87 = colluding_sheet[1,93]
  pcb125 = colluding_sheet[1,131]
  pcb90 = colluding_sheet[1,96]
  pcb101 = colluding_sheet[1,107]
  pcb113 = colluding_sheet[1,119]
  pcb93 = colluding_sheet[1,99]
  pcb100 = colluding_sheet[1,106]
  pcb108 = colluding_sheet[1,114]
  pcb124 = colluding_sheet[1,130]
  pcb128 = colluding_sheet[1,134]
  pcb166 = colluding_sheet[1,172]
  pcb129 = colluding_sheet[1,135]
  pcb138 = colluding_sheet[1,144]
  pcb163 = colluding_sheet[1,169]
  pcb135 = colluding_sheet[1,141]
  pcb151 = colluding_sheet[1,157]
  pcb139 = colluding_sheet[1,145]
  pcb140 = colluding_sheet[1,146]
  pcb147 = colluding_sheet[1,153]
  pcb149 = colluding_sheet[1,155]
  pcb153 = colluding_sheet[1,159]
  pcb168 = colluding_sheet[1,174]
  pcb156 = colluding_sheet[1,162]
  pcb157 = colluding_sheet[1,163]
  pcb171 = colluding_sheet[1,177]
  pcb173 = colluding_sheet[1,179]
  pcb180 = colluding_sheet[1,186]
  pcb193 = colluding_sheet[1,199]
  pcb198 = colluding_sheet[1,204]
  pcb199 = colluding_sheet[1,205]
}

if (pcb12 > pcb13) {
  colluding_sheet = colluding_sheet[,-19]
} else if (pcb13 > pcb12) {
  colluding_sheet[1,18] = pcb13
  colluding_sheet = colluding_sheet[,-19]
} 

if (pcb18 > pcb30) {
  colluding_sheet = colluding_sheet[,-35]
} else if (pcb30 > pcb18) {
  colluding_sheet[1,24] = pcb30
  colluding_sheet = colluding_sheet[,-35]
}

if (pcb20 > pcb28) {
  colluding_sheet = colluding_sheet[,-33]
} else if (pcb28 > pcb20) {
  colluding_sheet[1,25] = pcb28
  colluding_sheet = colluding_sheet[,-33]
}

if (pcb21 > pcb33) {
  colluding_sheet = colluding_sheet[,-36]
} else if (pcb33 > pcb21) {
  colluding_sheet[1,26] = pcb33
  colluding_sheet = colluding_sheet[,-36]
}

if (pcb26 > pcb29) {
  colluding_sheet = colluding_sheet[,-33]
} else if (pcb29 > pcb26) {
  colluding_sheet[1,31] = pcb29
  colluding_sheet = colluding_sheet[,-33]
}
if (pcb40 > pcb71) {
  colluding_sheet = colluding_sheet[,-72]
} else if (pcb71 > pcb40) {
  colluding_sheet[1,41] = pcb71
  colluding_sheet = colluding_sheet[,-72]
}
if (pcb49 > pcb69) {
  colluding_sheet = colluding_sheet[,-70]
} else if (pcb69 > pcb49) {
  colluding_sheet[1,50] = pcb69
  colluding_sheet = colluding_sheet[,-70]
}
if (pcb50 > pcb53) {
  colluding_sheet = colluding_sheet[,-54]
} else if (pcb53 > pcb50) {
  colluding_sheet[1,51] = pcb53
  colluding_sheet = colluding_sheet[,-54]
}
if (pcb85 > pcb116) {
  colluding_sheet = colluding_sheet[,-114]
} else if (pcb116 > pcb85) {
  colluding_sheet[1,83] = pcb116
  colluding_sheet = colluding_sheet[,-114]
}
if (pcb87 > pcb125) {
  colluding_sheet = colluding_sheet[,-122]
} else if (pcb125 > pcb87) {
  colluding_sheet[1,85] = pcb125
  colluding_sheet = colluding_sheet[,-122]
}
if (pcb93 > pcb100) {
  colluding_sheet = colluding_sheet[,-98]
} else if (pcb100 > pcb93) {
  colluding_sheet[1,91] = pcb100
  colluding_sheet = colluding_sheet[,-98]
}
if (pcb108 > pcb124) {
  colluding_sheet = colluding_sheet[,-120]
} else if (pcb124 > pcb108) {
  colluding_sheet[1,105] = pcb124
  colluding_sheet = colluding_sheet[,-120]
}
if (pcb128 > pcb166) {
  colluding_sheet = colluding_sheet[,-160]
} else if (pcb166 > pcb128) {
  colluding_sheet[1,122] = pcb166
  colluding_sheet = colluding_sheet[,-160]
}
if (pcb135 > pcb151) {
  colluding_sheet = colluding_sheet[,-145]
} else if (pcb151 > pcb135) {
  colluding_sheet[1,129] = pcb151
  colluding_sheet = colluding_sheet[,-145]
}
if (pcb139 > pcb140) {
  colluding_sheet = colluding_sheet[,-134]
} else if (pcb140 > pcb139) {
  colluding_sheet[1,133] = pcb140
  colluding_sheet = colluding_sheet[,-134]
}
if (pcb147 > pcb149) {
  colluding_sheet = colluding_sheet[,-142]
} else if (pcb149 > pcb147) {
  colluding_sheet[1,140] = pcb149
  colluding_sheet = colluding_sheet[,-142]
}
if (pcb153 > pcb168) {
  colluding_sheet = colluding_sheet[,-158]
} else if (pcb168 > pcb153) {
  colluding_sheet[1,144] = pcb168
  colluding_sheet = colluding_sheet[,-158]
}
if (pcb156 > pcb157) {
  colluding_sheet = colluding_sheet[,-148]
} else if (pcb157 > pcb156) {
  colluding_sheet[1,147] = pcb157
  colluding_sheet = colluding_sheet[,-148]
}
if (pcb171 > pcb173) {
  colluding_sheet = colluding_sheet[,-161]
} else if (pcb173 > pcb171) {
  colluding_sheet[1,159] = pcb173
  colluding_sheet = colluding_sheet[,-161]
}
if (pcb180 > pcb193) {
  colluding_sheet = colluding_sheet[,-180]
} else if (pcb193 > pcb180) {
  colluding_sheet[1,167] = pcb193
  colluding_sheet = colluding_sheet[,-180]
}
if (pcb198 > pcb199) {
  colluding_sheet = colluding_sheet[,-185]
} else if (pcb199 > pcb198) {
  colluding_sheet[1,184] = pcb199
  colluding_sheet = colluding_sheet[,-185]
}

if (pcb44 > pcb47 & pcb44 > pcb65) {
  colluding_sheet = colluding_sheet[,-c(48,65)]
} else if (pcb47 > pcb44 & pcb47 > pcb65) {
  colluding_sheet[1,45] = pcb47
  colluding_sheet = colluding_sheet[,-c(48,65)]
} else if (pcb65 > pcb44 & pcb65 > pcb47) {
  colluding_sheet[1,45] = pcb65
  colluding_sheet = colluding_sheet[,-c(48,65)]
}

if (pcb59 > pcb62 & pcb59 > pcb75) {
  colluding_sheet = colluding_sheet[,-c(61,71)]
} else if (pcb62 > pcb59 & pcb62 > pcb75) { 
  colluding_sheet[1,58] = pcb62
  colluding_sheet = colluding_sheet[,-c(61,71)]
} else if (pcb75 > pcb59 & pcb75 > pcb62) { 
  colluding_sheet[1,58] = pcb75
  colluding_sheet = colluding_sheet[,-c(61,71)]
}

if (pcb61 > pcb70 & pcb61 > pcb74 & pcb61 > pcb76) {
  colluding_sheet = colluding_sheet[,-c(66,69,70)]
} else if (pcb70 > pcb61 & pcb70 > pcb74 & pcb70 > pcb76) { 
  colluding_sheet[1,60] = pcb70
  colluding_sheet = colluding_sheet[,-c(66,69,70)]
} else if (pcb74 > pcb61 & pcb74 > pcb70 & pcb74 > pcb76) { 
  colluding_sheet[1,60] = pcb74
  colluding_sheet = colluding_sheet[,-c(66,69,70)]
} else if (pcb76 > pcb61 & pcb76 > pcb74 & pcb76 > pcb70) {
  colluding_sheet[1,60] = pcb76
  colluding_sheet = colluding_sheet[,-c(66,69,70)]
}

if (pcb86 > pcb97 & pcb86 > pcb119 & pcb86 > pcb109) {
  colluding_sheet = colluding_sheet[,-c(88,99,108)]
} else if (pcb97 > pcb86 & pcb97 > pcb119 & pcb97 > pcb109) { 
  colluding_sheet[1,77] = pcb97
  colluding_sheet = colluding_sheet[,-c(88,99,108)]
} else if (pcb119 > pcb86 & pcb119 > pcb97 & pcb119 > pcb109) { 
  colluding_sheet[1,77] = pcb119
  colluding_sheet = colluding_sheet[,-c(88,99,108)]
} else if (pcb109 > pcb86 & pcb109 > pcb97 & pcb109 > pcb119) {
  colluding_sheet[1,77] = pcb109
  colluding_sheet = colluding_sheet[,-c(88,99,108)]
}

if (pcb90 > pcb101 & pcb90 > pcb113) {
  colluding_sheet = colluding_sheet[,-c(90,101)]
} else if (pcb101 > pcb90 & pcb101 > pcb113) { 
  colluding_sheet[1,81] = pcb101
  colluding_sheet = colluding_sheet[,-c(90,101)]
} else if (pcb113 > pcb90 & pcb113 > pcb101) { 
  colluding_sheet[1,81] = pcb113
  colluding_sheet = colluding_sheet[,-c(90,101)]
}

if (pcb129 > pcb138 & pcb129 > pcb163) {
  colluding_sheet = colluding_sheet[,-c(120,141)]
} else if (pcb138 > pcb129 & pcb138 > pcb163) { 
  colluding_sheet[1,111] = pcb138
  colluding_sheet = colluding_sheet[,-c(120,141)]
} else if (pcb163 > pcb129 & pcb163 > pcb138) { 
  colluding_sheet[1,111] = pcb163
  colluding_sheet = colluding_sheet[,-c(120,141)]
}

colluding_sheet = colluding_sheet[,-175]

file_path = "Output/Data/Results/725340-14819-2012-27/results.csv"
write.csv(colluding_sheet, file = file_path, row.names = FALSE)

