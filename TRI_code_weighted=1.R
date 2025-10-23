# To modify the calculation of stress factors, change the threshold values (lines 33, 37, 42, 48 and 58). The values of the published item appear by default.
# To modify the TRI weights, change lines 66 to 73. The values of the published item appear by default.

#################################################################################################################

rm(list=ls())

# function to calculate the 40th percentile

  percentil = function(x, threshold=0.4){
    quantile(x, probs=threshold)
  }
   
  DCM = function(x){
    mean((x-mean(x))^2)
  } 

#################################################################################################################  
### obtaining the results shown in Table 2
  
# data entry

  library(readxl)
  datos <- read_excel("TRI_data.xlsx", sheet = "datos") # the data is publicly available at https://digibug.ugr.es/handle/10481/107272
  attach(datos)

# calculation of stress factors 
  
  # head(datos)
  # View(datos)
  
  # factor 1: it is considered a stress factor if fewer than 12 players are called up
    threshold1 = 12
    F1 = ifelse(Convocados < threshold1, 1, 0)
  
  # factor 2: it is considered stressful if the victory/defeat is by 5 points or less; this situation is coded as 1 (otherwise 0)
    threshold2 = 5
    F2 = ifelse(abs(Resultado) <= threshold2, 1, 0)
    #F2 = ifelse(abs(Resultado) < 6, 1, 0)
  
  # factor 3: it will be considered a stress factor if the losing streak exceeds three matches
    threshold3 = -3
    F3 = ifelse(Rachas < threshold3, 1, 0)
    #F3 = ifelse(Rachas < -3, 1, 0)
  
  # factor 4: The assessment will be considered unsatisfactory when the score is below the 40th percentile (low performance)
    games = as.double(tapply(Indice, Equipo, max)) 
    threshold4 = 0.4
    percentiles = as.double(tapply(Valoracion, Equipo, function(x) percentil(x, threshold = threshold4)))
    valoraciones = c()
    for (i in 1:19){ valoraciones = c(valoraciones, rep(percentiles[i], games[i]))}
    F4 = ifelse(Valoracion < valoraciones, 1, 0)
    
  # factor 5: the change of coach is considered stressful in the four matches following his replacement (this is already encoded directly in the database)
    F5 = Entrenador

  # factor 6: It will be considered a stressful factor if there are three or fewer days between matches (counting Euroleague and Copa del Rey)
    threshold6 = 4
    F6 = ifelse(DiasR < threshold6, 1, 0)
  
  # factor 7: playing away from home is considered a stressful factor 
    F7 = ifelse(Local == 0, 1, 0)
  
  # add stressful factors
    m = 7
    weighted = 1 # weights obtained from the frequency of occurrence of each stress factor observed in Table 3 (and 7)
      w1 = 0.15789 # 0.10159 
      w2 = 0.16667 # 0.13642
      w3 = 0.13157 # 0.05494
      w4 = 0.16667 # 0.20062
      w5 = 0.04385 # 0.022303
      w6 = 0.16667 # 0.21139
      w7 = 0.16667 # 0.27271
    if (weighted == 1){
      SF = w1*F1 + w2*F2 + w3*F3 + w4*F4 + w5*F5 + w6*F6 + w7*F7 # stress factors have different weights
    } else {
      SF = (F1 + F2 + F3 + F4 + F5 + F6 + F7)/m # all stress factors carry equal weight
    }
  
# calculation of the resilience index (Option A)
  
  RP = ifelse(Resultado > 0, 1, -1) 
  
  IR = SF*RP
    IR_A = IR
  IR_media = tapply(IR, Equipo, mean)
  IR_mediana = tapply(IR, Equipo, median)
  DCM_media = tapply(IR, Equipo, DCM)
    DCM_media_A = DCM_media
  
  IRm = IR/7
  IRm_media = tapply(IRm, Equipo, mean)
  IRm_mediana = tapply(IRm, Equipo, median)
  
  opcionA = data.frame(IR_media, DCM_media, IRm_media, IR_mediana, IRm_mediana)
  opcionA = data.frame(IR_media, DCM_media, IRm_media*100, order(order(IR_media, -DCM_media, decreasing = TRUE)))
  
# calculation of the resilience index (Option B)
  
  RP = ifelse(Resultado > 0, 1, 0) 
  
  IR = SF*RP
    IR_B = IR
  IR_media = tapply(IR, Equipo, mean)
  IR_mediana = tapply(IR, Equipo, median)
  DCM_media = tapply(IR, Equipo, DCM)
    DCM_media_B = DCM_media
  
  IRm = IR/7
  IRm_media = tapply(IRm, Equipo, mean)
  IRm_mediana = tapply(IRm, Equipo, median)
  
  opcionB = data.frame(IR_media, DCM_media, IRm_media, IR_mediana, IRm_mediana)
  opcionB = data.frame(IR_media, DCM_media, IRm_media*100, order(order(IR_media, -DCM_media, decreasing = TRUE)))  
  
# results of Table 2
  
  names(opcionA) = c("TRI", "DCM", "(TRI/m)*100", "Ranking")
  opcionA # results shown in Table 2
  
  names(opcionB) = c("TRI", "DCM", "(TRI/m)*100", "Ranking")
  opcionB # results shown in Table 2
  
  Games = tapply(Indice, Equipo, max)
  ACB_position = c(1, 2, 15, 4, 5, 3, 13, 19, 11, 9, 7, 8, 10, 6, 14, 17, 16, 12, 18)
  
  Table11 = cbind(Games, ACB_position, round(opcionA, digits=2), round(opcionB, digits=2)) # Table 2
  Table11
  
#################################################################################################################
### obtaining the results shown in Table 3
  
  F1.aparicion = tapply(F1, Equipo, max) # 1f a 1 appears, it means that the team suffers from that factor at some point
  F2.aparicion = tapply(F2, Equipo, max) 
  F3.aparicion = tapply(F3, Equipo, max) 
  F4.aparicion = tapply(F4, Equipo, max) 
  F5.aparicion = tapply(F5, Equipo, max) 
  F6.aparicion = tapply(F6, Equipo, max) 
  F7.aparicion = tapply(F7, Equipo, max) 
    Factores.Aparicion = data.frame(F1.aparicion, F2.aparicion, F3.aparicion, F4.aparicion, F5.aparicion, F6.aparicion, F7.aparicion)
    Factores.Aparicion # results shown in Table 3
    mi = rowSums(Factores.Aparicion)
    aparicion = colSums(Factores.Aparicion)
    pesos1 = aparicion/sum(aparicion) # option to weigh stressors
        
  Table3 = cbind(rbind(Factores.Aparicion, aparicion), c(mi, NA)) # Table 3 (1 = Yes, 0 = No)
  names(Table3) = c("F1", "F2", "F3", "F4", "F5", "F6", "F7", "TOTAL")
  Table3
  
#################################################################################################################
### obtaining the results shown in Table 4
    
  miBIS = rep(mi, times=Games)
  
  IRm_A = IR_A/miBIS
  IRm_media_A = tapply(IRm_A, Equipo, mean)
  
  opcionA_mi = data.frame(IRm_media_A*100, order(order(IRm_media_A, -DCM_media_A, decreasing = TRUE)))
  names(opcionA_mi) = c("(TRI/mi)*100", "Ranking")
  
  IRm_B = IR_B/miBIS
  IRm_media_B = tapply(IRm_B, Equipo, mean)
  
  opcionB_mi = data.frame(IRm_media_B*100, order(order(IRm_media_B, -DCM_media_B, decreasing = TRUE)))
  names(opcionB_mi) = c("(TRI/mi)*100", "Ranking")
  
  wins = c(94.44, 88.88, 33.33, 66.66, 63.88, 75, 38.88, 19.44, 47.22, 47.22, 55.55, 50, 47.22, 61.11, 33.33, 27.77, 30.55, 44.44, 25)
  Table12 = cbind(wins, mi, round(opcionA_mi, digits=2), round(opcionB_mi, digits=2)) # Table 4
  Table12
    
#################################################################################################################
### obtaining the Figures 1 to 2
  
  OptA_position_m = opcionA[,4] 
  OptB_position_m = opcionB[,4]
  
  team = c("Real Madrid", "FC Barcelona", "Fuenlabrada", "Valencia", "Baskonia", "Tenerife", "Zaragoza", "Guipuzkoa", "Unicaja", "Andorra", "Joventut", "Gran Canaria", "Manresa", "Burgos", "Obradoiro", "Bilbao", "Betis", "Murcia", "Estudiantes")
  
  # Figure 1
  
    # win.graph()
    plot(ACB_position, OptA_position_m, xlab="ACB classification", ylab="Option A classification", col="blue", lwd=3, xlim=c(0, 22), ylim=c(0,21), xaxp = c(1,19,18), yaxp = c(1,19,18))
    text(ACB_position, OptA_position_m, labels = team, pos=4) 
    grid(nx = NULL, ny = NULL,
       lty = 2,      # Tipo de ínea
       col = "gray", # Color
       lwd = 2)      # Ancho de línea
    abline(a=0, b=1, col="red", lwd=2)
    # savePlot("Figure1_top", type="jpg")
    # savePlot("Figure1_top", type="eps")
  
    # win.graph()
    plot(ACB_position, OptB_position_m, xlab="ACB classification", ylab="Option B classification", col="blue", lwd=3, xlim=c(0, 22), ylim=c(0,21), xaxp = c(1,19,18), yaxp = c(1,19,18))
    text(ACB_position, OptB_position_m, labels = team, pos=4)
    grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 2)
    abline(a=0, b=1, col="red", lwd=2)
    # savePlot("Figure1_bottom", type="jpg")
    # savePlot("Figure1_bottom", type="eps")
  
  # Figure 2
  
    # win.graph()
    plot(OptA_position_m, OptB_position_m, xlab="Option A classification", ylab="Option B classification", col="blue", lwd=3, xlim=c(0, 22), ylim=c(0,21), xaxp = c(1,19,18), yaxp = c(1,19,18))
    text(OptA_position_m, OptB_position_m, labels = team, pos=4)
    grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 2)
    abline(a=0, b=1, col="red", lwd=2)
    # savePlot("Figure2", type="jpg")
    # savePlot("Figure2", type="eps")   
  
#################################################################################################################
### obtaining the results shown in Table 13
    
  OptA_position_mi = opcionA_mi[,2] 
  OptB_position_mi = opcionB_mi[,2] 
  
  TRIs = data.frame(ACB_position, OptA_position_m, OptB_position_m, OptA_position_mi, OptB_position_mi)
  cor(TRIs, method="spearman") # results shown in Table 13
    cor.test(ACB_position, OptA_position_m, method="spearman")
    cor.test(ACB_position, OptB_position_m, method="spearman")
    cor.test(ACB_position, OptA_position_mi, method="spearman")
    cor.test(ACB_position, OptB_position_mi, method="spearman")
    cor.test(OptA_position_m, OptB_position_m, method="spearman")
    cor.test(OptA_position_m, OptA_position_mi, method="spearman")
    cor.test(OptA_position_m, OptB_position_mi, method="spearman")
    cor.test(OptB_position_m, OptA_position_mi, method="spearman")
    cor.test(OptB_position_m, OptB_position_mi, method="spearman")
    cor.test(OptA_position_mi, OptB_position_mi, method="spearman")

#################################################################################################################
### obtaining the Figure 3
    
  TRI_A = opcionA_mi[,1]
    cor.test(TRI_A, wins)
      
    # win.graph()
    plot(TRI_A, wins, xlab="TRI (option A)", ylab="% wins", col="blue", lwd=3, xlim=c(-4, 8), ylim=c(0,100))
    text(TRI_A, wins, labels = team, pos=4)
    grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 2)
    reg = lm(wins ~ TRI_A)
    abline(reg, col="red", lwd=2)
    # savePlot("Figure3_top", type="jpg")
    # savePlot("Figure3_top", type="eps")
    # savePlot("Figure3_top", type="pdf")
    
  TRI_B = opcionB_mi[,1]
    cor.test(TRI_B, wins)
    
    # win.graph()
    plot(TRI_B, wins, xlab="TRI (option B)", ylab="% wins", col="blue", lwd=3, xlim=c(-1, 9), ylim=c(0,100))
    text(TRI_B, wins, labels = team, pos=4)
    grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 2)
    reg = lm(wins ~ TRI_B)
    abline(reg, col="red", lwd=2)
    # savePlot("Figure3_bottom", type="jpg")
    # savePlot("Figure3_bottom", type="eps")
    # savePlot("Figure3_bottom", type="pdf")
    
#################################################################################################################
### obtaining the results shown in Table 14 
    
  # independent variables
    
    AA = c(27.07692308, 27.52631579, 27.36842105, 25.6, 24.10526316, 27.29411765, 27.76470588, 28.9375, 26.0625, 26,
           26.52380952, 25.76190476, 30.17647059, 26.14285714, 25.11764706, 26.15, 25.94444444, 25.625, 26.5) 
    NP = c(10, 5, 9, 7, 7, 12, 9, 4, 10, 9, 12, 11, 4, 15, 8, 13, 7, 12, 8) 
    ATR = c(2.076923077, 2.684210526, 1.947368421, 2.1, 3.263157895, 1.352941176, 1.823529412, 2, 1.6875, 1.904761905, 
            1.761904762, 2.19047619, 5.529411765, 1.285714286, 2.235294118, 1.95, 2.5, 1.72, 3.166666667) 
    CSC = c(1, 1, 2, 1, 4, 3, 2, 5, 10, 1, 3, 1, 10, 2, 1, 3, 3, 1, 3) 
    Euroligue = c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1) 
    Europe = c(0, 0, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0) 
    
    reorganisation = c("08_Guipuzkoa", "02_FCBarcelona", "13_Manresa", "07_Zaragoza", "11_Joventut", "17_Betis", "14_Burgos",
                  "06_Tenerife", "15_Obradoiro", "12_GranCanaria", "10_Andorra", "19_Estudiantes", "01_RMadrid", "16_Bilbao",
                  "05_Baskonia", "18_Murcia", "09_Unicaja", "03_Fuenlabrada", "04_Valencia")
    
    the_data = round(data.frame(AA, NP, ATR, CSC, Euroligue, Europe), digits=2)
    the_data = data.frame(reorganisation, the_data)
    library(dplyr)
    the_data = arrange(the_data, reorganisation)
    
  # dependent variables    
  
    the_data = data.frame(the_data, round(TRI_A, digits=2), round(TRI_B, digits=2))
    names(the_data) = c("Team", "AA", "NP", "ATR", "CSC", "Euroligue", "Europe", "Euroligue", "Europe")  # Table 6
    the_data
    
  # multicollinearity
    
    library(multiColl)
    
    AA = the_data[,2]
    NP = the_data[,3]
    ATR = the_data[,4]
    CSC = the_data[,5]
    Euroleague = the_data[,6]
    Europe = the_data[,7]
    
    cte = rep(1, length(AA))
    X = cbind(cte, AA, NP, ATR, CSC, Euroleague, Europe)
      RdetR(X, dummy=T, pos = c(6,7))
      data.frame(CVs(X), CVs(X) < 0.1) # the variable AA may be related to the constant (non-essential)
      data.frame(VIF(X, dummy=T, pos = c(6,7)), VIF(X, dummy=T, pos = c(6,7)) > 10)
      data.frame(CN(X), CN(X) > 20)  
    
    AAc = AA - mean(AA) # centre the variable that I suspect is causing the problem
    Xc = cbind(cte, AAc, NP, ATR, CSC, Euroleague, Europe)
      RdetR(Xc, dummy=T, pos = c(6,7)) # this remains unchanged (as we know)
      data.frame(CVs(Xc), CVs(Xc) < 0.1) # problem resolved
      data.frame(VIF(Xc, dummy=T, pos = c(6,7)), VIF(Xc, dummy=T, pos = c(6,7)) > 10) # this remains unchanged (as we know)
      data.frame(CN(Xc), CN(Xc) > 20)  
    
  # regression
    
    regA = lm(TRI_A~AAc+NP+ATR+CSC+Euroleague+Europe)
    summary(regA) # information shown in Table 7
    
    regB = lm(TRI_B~AAc+NP+ATR+CSC+Euroleague+Europe) 
    summary(regB) # information shown in Table 7
    
  # H0: homoscedasticity (information shown in Table 7)
    
    library(lmtest)
    bptest(regA) # H0 is not rejected
    bptest(regB) # H0 is not rejected
    
  # H0: normality (information shown in Table 7)
    
    eA = regA$residuals
    ks.test(eA, pnorm, 0, sd(eA)) # H0 is not rejected
    
    eB = regB$residuals
    ks.test(eB, pnorm, 0, sd(eB)) # H0 is not rejected

#################################################################################################################
### obtaining the Figure 4
    
  # residuals interpretation
    
      residuos = data.frame(team, round(eA, digits=3), round(eB, digits=3))
      names(residuos) = c("Equipo", "Option A", "Option B")
      residuos
      
      # win.graph()
      barplot(eA, names.arg = team, las=2, ylab="Residuals of the Option A model", col="blue", cex.names = 0.7)
      # savePlot("Figure4_top", type="jpg")
      # savePlot("Figure4_top", type="eps")
      # savePlot("Figure4_top", type="pdf")
      
      # win.graph()
      barplot(eB, names.arg = team, las=2, ylab="Residuals of the Option B model", col="blue", cex.names = 0.7)
      # savePlot("Figure4_bottom", type="jpg")
      # savePlot("Figure4_bottom", type="eps")
      # savePlot("Figure4_bottom", type="pdf")
    
#################################################################################################################
### obtaining the results shown in Table 10   
    
  F1.suma = tapply(F1, Equipo, sum) 
  F2.suma = tapply(F2, Equipo, sum) 
  F3.suma = tapply(F3, Equipo, sum) 
  F4.suma = tapply(F4, Equipo, sum) 
  F5.suma = tapply(F5, Equipo, sum) 
  F6.suma = tapply(F6, Equipo, sum) 
  F7.suma = tapply(F7, Equipo, sum)
  SF.suma = F1.suma + F2.suma + F3.suma + F4.suma +F5.suma + F6.suma + F7.suma
  F1.PORC = F1.suma*100/SF.suma
  F2.PORC = F2.suma*100/SF.suma
  F3.PORC = F3.suma*100/SF.suma 
  F4.PORC = F4.suma*100/SF.suma 
  F5.PORC = F5.suma*100/SF.suma 
  F6.PORC = F6.suma*100/SF.suma 
  F7.PORC = F7.suma*100/SF.suma
    Factores.SUMA.PORC = data.frame(F1.suma, F2.suma, F3.suma, F4.suma, F5.suma, F6.suma, F7.suma, SF.suma, F1.PORC, F2.PORC, F3.PORC, F4.PORC, F5.PORC, F6.PORC, F7.PORC)
    names(Factores.SUMA.PORC) = c("ST1", "ST2", "ST3", "ST4", "ST5", "ST6", "ST7", "Total ST", "%ST1",  "%ST2", "%ST3", "%ST4", "%ST5", "%ST6", "%ST7")
    Factores.SUMA.PORC  # results shown in Table 10
    #colSums(Factores.SUMA.PORC)
    promedios = colMeans(Factores.SUMA.PORC)
    pesos2 = promedios[-c(1:8)] # option to weigh stressors
    
  Table10 = round(rbind(Factores.SUMA.PORC, promedios), digits=2) # Table 10
  Table10