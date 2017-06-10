library("zoo")
library("xts")
library("quantmod")

start <- as.Date("1995-01-02")
end <- as.Date("2016-12-30")

# DAX symbols
DAX.symb <- c("PSM.DE", "DPW.DE", "MRK.DE", "DTE.DE",
              "SAP.DE", "BAYN.DE", "VOW3.DE", "CBK.DE",
              "VNA.DE", "EOAN.DE", "BMW.DE", "DAI.DE",
              "CON.DE", "ADS.DE", "BEI.DE", "HEN3.DE",
              "MUV2.DE", "DB1.DE", "ALV.DE", "SIE.DE",
              "IFX.DE", "DBK.DE", "RWE.DE", "FRE.DE",
              "LIN.DE", "FME.DE", "TKA.DE", "LHA.DE",
              "BAS.DE", "HEI.DE")

DAX.env <- new.env()
getSymbols(DAX.symb, src = "yahoo", env = DAX.env, verbose = T, from = start, to = end)

DAX.data <- xts()
for (symb in DAX.symb) {
  DAX.data <- merge.xts(DAX.data, get(symb, envir = DAX.env)[, paste0(symb, ".Close")], join = "outer")
}
colnames(DAX.data) <- DAX.symb

saveRDS(DAX.data, file = "DAX_data.RDS")

#Symbol    Company name    Last price    Change    % change    Volume        
#PSM.DE   ProSiebenSat.1 Media SE  36.91  -0.04   -0.11%   907,782     
#DPW.DE   Deutsche Post AG  31.91  -0.03   -0.11%   2,032,449     
#MRK.DE   Merck Kommanditgesellschaft auf Aktien  106.80  0.15   0.14%   244,920     
#DTE.DE   Deutsche Telekom AG  17.12  0.03   0.18%   7,832,261     
#SAP.DE   SAP SE  96.00  0.19   0.20%   1,540,772     
#BAYN.DE   Bayer Aktiengesellschaft  119.05  0.30   0.25%   1,514,873     
#VOW3.DE   Volkswagen Aktiengesellschaft  132.60  0.35   0.26%   981,308     
#CBK.DE   Commerzbank AG  9.48  0.03   0.29%   16,495,053     
#VNA.DE   Vonovia SE  35.99  0.10   0.29%   855,293     
#EOAN.DE   E.ON SE  8.80  -0.03   -0.33%   15,157,954     
#BMW.DE   Bayerische Motoren Werke Aktiengesellschaft  84.43  0.32   0.38%   984,708     
#DAI.DE   Daimler AG  65.46  0.30   0.46%   2,641,147     
#CON.DE   Continental Aktiengesellschaft  198.70  1.40   0.71%   302,844     
#ADS.DE   Adidas AG  170.00  1.25   0.74%   910,658     
#BEI.DE   Beiersdorf Aktiengesellschaft  96.29  0.71   0.74%   314,909     
#HEN3.DE   Henkel AG &amp; Co. KGaA  125.70  1.00   0.80%   330,725     
#MUV2.DE   Münchener Rückversicherungs-Gesellschaft Aktiengesellschaft  177.30  1.45   0.82%   348,438     
#DB1.DE   Deutsche Börse Aktiengesellschaft  92.77  -0.78   -0.83%   339,590     
#ALV.DE   Allianz SE  173.85  1.80   1.05%   1,154,287     
#SIE.DE   Siemens Aktiengesellschaft  129.40  1.45   1.13%   1,412,237     
#IFX.DE   Infineon Technologies AG  20.39  0.24   1.17%   3,644,462     
#DBK.DE   Deutsche Bank AG  15.81  0.19   1.22%   11,583,710     
#RWE.DE   RWE Aktiengesellschaft  19.73  -0.26   -1.30%   6,268,180     
#FRE.DE   Fresenius SE &amp; Co. KGaA  77.76  1.00   1.30%   898,831     
#LIN.DE   Linde Aktiengesellschaft  179.30  2.40   1.36%   481,374     
#FME.DE   Fresenius Medical Care AG &amp; Co. KGAA  86.83  1.40   1.64%   703,760     
#TKA.DE   ThyssenKrupp AG  24.27  0.48   2.02%   2,726,788     
#LHA.DE   Deutsche Lufthansa Aktiengesellschaft  18.60  0.38   2.06%   6,496,461     
#BAS.DE   BASF SE  86.59  2.33   2.77%   2,949,451     
#HEI.DE   HeidelbergCement AG  84.44  2.56   3.13%   1,268,289   

