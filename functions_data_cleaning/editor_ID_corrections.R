# This will load and the corrected files and make the required changes.
editor_ID_corrections <- function(DATAFILE) {
  # DATAFILE<-ALLDATA  
  library(tidyverse)
  DATAFILE$editor_id<-as.numeric(as.character(DATAFILE$editor_id))
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="hockey" & DATAFILE$FIRST_NAME=="p"]<-2829
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="vannes" & DATAFILE$FIRST_NAME=="egbert"]<-1039
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="mcgraw" & DATAFILE$FIRST_NAME=="kevin"]<-2048
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="mcwilliams" & DATAFILE$FIRST_NAME=="scott"]<-3404
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="adkins-regan" & DATAFILE$FIRST_NAME=="elizabeth"]<-928
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="rossberg" & DATAFILE$FIRST_NAME=="sxel"]<-183
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="cole" & DATAFILE$FIRST_NAME=="blaine"]<-255
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="drake" & DATAFILE$FIRST_NAME=="bert"]<-263
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="kimball" & DATAFILE$FIRST_NAME=="bruce"]<-302
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="law" & DATAFILE$FIRST_NAME=="beverly"]<-307
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="loiselle" & DATAFILE$FIRST_NAME=="bette"]<-310
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="sandercock" & DATAFILE$FIRST_NAME=="brett"]<-349
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="eckert" & DATAFILE$FIRST_NAME=="christopher"]<-440
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="elphick" & DATAFILE$FIRST_NAME=="chris"]<-442
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="korner" & DATAFILE$FIRST_NAME=="christian"]<-499
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="schneider" & DATAFILE$FIRST_NAME=="christopher"]<-565
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="johnson" & DATAFILE$FIRST_NAME=="douglas"]<-741
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="reznick" & DATAFILE$FIRST_NAME=="david"]<-839
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="adkins-regan" & DATAFILE$FIRST_NAME=="elizabeth"]<-928
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="svensson" & DATAFILE$FIRST_NAME=="erik"]<-1031
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="bazzaz" & DATAFILE$FIRST_NAME=="fakhri"]<-1052
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="woodward" & DATAFILE$FIRST_NAME=="f"]<-1144
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="bortolotti" & DATAFILE$FIRST_NAME=="gary"]<-1153
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="possingham" & DATAFILE$FIRST_NAME=="hugh"]<-1347
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="rogers" & DATAFILE$FIRST_NAME=="hugo"]<-1354
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="hanski" & DATAFILE$FIRST_NAME=="ilkka"]<-1398
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="bronstein" & DATAFILE$FIRST_NAME=="judith"]<-1485
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="cavender-bares" & DATAFILE$FIRST_NAME=="jeannine"]<-1506
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="clark" & DATAFILE$FIRST_NAME=="james"]<-1512
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="clutton-brock" & DATAFILE$FIRST_NAME=="juliet"]<-1515
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="grace" & DATAFILE$FIRST_NAME=="james"]<-1604
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="levinton" & DATAFILE$FIRST_NAME=="jeffrey"]<-1711
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="lloyd" & DATAFILE$FIRST_NAME=="jon"]<-1719
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="sauer" & DATAFILE$FIRST_NAME=="john"]<-1854
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="walters" & DATAFILE$FIRST_NAME=="jeffrey"]<-1936
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="mcgraw" & DATAFILE$FIRST_NAME=="kevin"]<-2048
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="thrall" & DATAFILE$FIRST_NAME=="peter"]<-2930
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="schmidt" & DATAFILE$FIRST_NAME=="kenneth"]<-2082
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="scribner" & DATAFILE$FIRST_NAME=="kim"]<-2085
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="smith" & DATAFILE$FIRST_NAME=="kimberly"]<-2094
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="fahrig" & DATAFILE$FIRST_NAME=="lenore"]<-2144
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="giraldeau" & DATAFILE$FIRST_NAME=="luc-alain"]<-2155
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="ball" & DATAFILE$FIRST_NAME=="marilyn"]<-2287
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="hellberg" & DATAFILE$FIRST_NAME=="michael"]<-2409
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="lawton" & DATAFILE$FIRST_NAME=="marcy"]<-2443
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="leibold" & DATAFILE$FIRST_NAME=="mathew"]<-2445
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="ryan" & DATAFILE$FIRST_NAME=="michael"]<-2547
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="buchmann" & DATAFILE$FIRST_NAME=="nina"]<-2632
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="sodhi" & DATAFILE$FIRST_NAME=="navjot"]<-2693
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="sala" & DATAFILE$FIRST_NAME=="osvaldo"]<-2737
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="kareiva" & DATAFILE$FIRST_NAME=="peter"]<-2845
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="lundberg" & DATAFILE$FIRST_NAME=="per"]<-2861
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="matson" & DATAFILE$FIRST_NAME=="pamela"]<-2864
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="stouffer" & DATAFILE$FIRST_NAME=="philip"]<-2928
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="brumfield" & DATAFILE$FIRST_NAME=="robb"]<-2998
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="crozier" & DATAFILE$FIRST_NAME=="ross"]<-3027
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="montgomerie" & DATAFILE$FIRST_NAME=="robert"]<-3160
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="safran" & DATAFILE$FIRST_NAME=="rebecca"]<-3208
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="sage" & DATAFILE$FIRST_NAME=="rowan"]<-3209
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="zink" & DATAFILE$FIRST_NAME=="robert"]<-3261
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="brown" & DATAFILE$FIRST_NAME=="sandra"]<-3290
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="mcwilliams" & DATAFILE$FIRST_NAME=="scott"]<-3404
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="steppan" & DATAFILE$FIRST_NAME=="scott"]<-3481
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="day" & DATAFILE$FIRST_NAME=="troy"]<-3533
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="hirose" & DATAFILE$FIRST_NAME=="tadaki"]<-3566
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="williams" & DATAFILE$FIRST_NAME=="tony"]<-3661
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="berger" & DATAFILE$FIRST_NAME=="uta"]<-3663
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="grimm" & DATAFILE$FIRST_NAME=="volker"]<-3684
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="smith" & DATAFILE$FIRST_NAME=="val"]<-3700
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="karasov" & DATAFILE$FIRST_NAME=="william"]<-3756
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="koenig" & DATAFILE$FIRST_NAME=="walter"]<-3760
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="schlesinger" & DATAFILE$FIRST_NAME=="william"]<-3794
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="smith" & DATAFILE$FIRST_NAME=="james"]<-3907
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="bolnick" & DATAFILE$FIRST_NAME=="daniel"]<-631
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="hall" & DATAFILE$FIRST_NAME=="spencer"]<-3346
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="hughes" & DATAFILE$FIRST_NAME=="kimberly"]<-2017
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="martinezdelrio" & DATAFILE$FIRST_NAME=="carlos"]<-520
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="mcgraw" & DATAFILE$FIRST_NAME=="kevin"]<-2048
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="mcwilliams" & DATAFILE$FIRST_NAME=="scott"]<-3404
  
  
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="ritland" & DATAFILE$FIRST_NAME=="kermit"]<-2076
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="young" & DATAFILE$YEAR==2014 & DATAFILE$JOURNAL=="MARECOL"]<-602
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="jarvis" & DATAFILE$FIRST_NAME=="paul"]<-2840
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="odonnell" & DATAFILE$FIRST_NAME=="sean"]<-3420
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="hurst" & DATAFILE$FIRST_NAME=="gregory"]<-1208
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="fincham" & DATAFILE$FIRST_NAME=="a"]<-72
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="henderson-sellers" & DATAFILE$FIRST_NAME=="ann"]<-100
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="mou" & DATAFILE$FIRST_NAME=="pu"]<-2527
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="rossberg" & DATAFILE$FIRST_NAME=="axel"]<-183
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="mou" & DATAFILE$FIRST_NAME=="pu"]<-2527
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="thrall" & DATAFILE$FIRST_NAME=="peter"]<-2938
 
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="vandermaarel" & (DATAFILE$FIRST_NAME=="e"| DATAFILE$FIRST_NAME=="eddy")]<-1033
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="hepinstall-cymerman"& DATAFILE$FIRST_NAME=="j"]<-1533
  
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="dyer" & DATAFILE$FIRST_NAME=="andrew" &
                       DATAFILE$INST=="university of south carolina aiken"]<-3849
  
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="field" & DATAFILE$FIRST_NAME=="chris" &
                       DATAFILE$JOURNAL=="gcb"]<-447
  
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="franklin" & DATAFILE$FIRST_NAME=="janet" &
                       DATAFILE$JOURNAL=="leco"]<-3889
  
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="houck" & DATAFILE$FIRST_NAME=="lynne" &
                       DATAFILE$JOURNAL=="amnat"]<-2174
  
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="lawes" & DATAFILE$FIRST_NAME=="michael" &
                       DATAFILE$JOURNAL=="plantecol"]<-2442
  
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="lack" & DATAFILE$FIRST_NAME=="andrew" &
                       DATAFILE$MIDDLE_NAME=="j"]<-130
  
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="lomolino" & DATAFILE$FIRST_NAME=="mark" &
                        DATAFILE$MIDDLE_NAME=="v"]<-2453
  
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="long" & DATAFILE$FIRST_NAME=="stephen" &
                       DATAFILE$JOURNAL=="gcb"]<-3395
  
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="niemela" & DATAFILE$FIRST_NAME=="j" &
                       DATAFILE$JOURNAL=="leco"]<-1787
  
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="rozema" & DATAFILE$FIRST_NAME=="j" &
                       DATAFILE$JOURNAL=="plantecol"]<-1849
  
  
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="tjoelker" & DATAFILE$FIRST_NAME=="m" &
                       DATAFILE$JOURNAL=="newphyt"]<-2577
  
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="wagner" & DATAFILE$FIRST_NAME=="helene" &
                       DATAFILE$JOURNAL=="leco"]<-1373
  
  
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="vandermeijden" & DATAFILE$FIRST_NAME=="eddy"]<-1036
  
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="dekroon" & DATAFILE$FIRST_NAME=="hans"]<-1296
  
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="vangroenendael" & DATAFILE$FIRST_NAME=="jan"]<-1924
  
  DATAFILE$editor_id[DATAFILE$LAST_NAME=="hansson" & DATAFILE$FIRST_NAME=="lennart" &
                       DATAFILE$INST=="swedish university of agricultural sciences"]<-2162
  
 DATAFILE$editor_id<-as.factor(DATAFILE$editor_id)

 return(DATAFILE)
}