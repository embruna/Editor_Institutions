# This will load and the corrected files and make the required changes.
editor_ID_corrections <- function(datafile) {
  # datafile<-alldata
  library(tidyverse)
  datafile$editor_id <- as.numeric(as.character(datafile$editor_id))
  datafile$editor_id[datafile$last_name == "hockey" &
    datafile$first_name == "p"] <- 2829
  datafile$editor_id[datafile$last_name == "vannes" &
    datafile$first_name == "egbert"] <- 1039
  datafile$editor_id[datafile$last_name == "mcgraw" &
    datafile$first_name == "kevin"] <- 2048
  datafile$editor_id[datafile$last_name == "mcwilliams" &
    datafile$first_name == "scott"] <- 3404
  datafile$editor_id[datafile$last_name == "adkins-regan" &
    datafile$first_name == "elizabeth"] <- 928
  datafile$editor_id[datafile$last_name == "rossberg" &
    datafile$first_name == "sxel"] <- 183
  datafile$editor_id[datafile$last_name == "cole" &
    datafile$first_name == "blaine"] <- 255
  datafile$editor_id[datafile$last_name == "drake" &
    datafile$first_name == "bert"] <- 263
  datafile$editor_id[datafile$last_name == "kimball" &
    datafile$first_name == "bruce"] <- 302
  datafile$editor_id[datafile$last_name == "law" &
    datafile$first_name == "beverly"] <- 307
  datafile$editor_id[datafile$last_name == "loiselle" &
    datafile$first_name == "bette"] <- 310
  datafile$editor_id[datafile$last_name == "sandercock" &
    datafile$first_name == "brett"] <- 349
  datafile$editor_id[datafile$last_name == "eckert" &
    datafile$first_name == "christopher"] <- 440
  datafile$editor_id[datafile$last_name == "elphick" &
    datafile$first_name == "chris"] <- 442
  datafile$editor_id[datafile$last_name == "korner" &
    datafile$first_name == "christian"] <- 499
  datafile$editor_id[datafile$last_name == "schneider" &
    datafile$first_name == "christopher"] <- 565
  datafile$editor_id[datafile$last_name == "johnson" &
    datafile$first_name == "douglas"] <- 741
  datafile$editor_id[datafile$last_name == "reznick" &
    datafile$first_name == "david"] <- 839
  datafile$editor_id[datafile$last_name == "adkins-regan" &
    datafile$first_name == "elizabeth"] <- 928
  datafile$editor_id[datafile$last_name == "svensson" &
    datafile$first_name == "erik"] <- 1031
  datafile$editor_id[datafile$last_name == "bazzaz" &
    datafile$first_name == "fakhri"] <- 1052
  datafile$editor_id[datafile$last_name == "woodward" &
    datafile$first_name == "f"] <- 1144
  datafile$editor_id[datafile$last_name == "bortolotti" &
    datafile$first_name == "gary"] <- 1153
  datafile$editor_id[datafile$last_name == "possingham" &
    datafile$first_name == "hugh"] <- 1347
  datafile$editor_id[datafile$last_name == "rogers" &
    datafile$first_name == "hugo"] <- 1354
  datafile$editor_id[datafile$last_name == "hanski" &
    datafile$first_name == "ilkka"] <- 1398
  datafile$editor_id[datafile$last_name == "bronstein" &
    datafile$first_name == "judith"] <- 1485
  datafile$editor_id[datafile$last_name == "cavender-bares" &
    datafile$first_name == "jeannine"] <- 1506
  datafile$editor_id[datafile$last_name == "clark" &
    datafile$first_name == "james"] <- 1512
  datafile$editor_id[datafile$last_name == "clutton-brock" &
    datafile$first_name == "juliet"] <- 1515
  datafile$editor_id[datafile$last_name == "grace" &
    datafile$first_name == "james"] <- 1604
  datafile$editor_id[datafile$last_name == "levinton" &
    datafile$first_name == "jeffrey"] <- 1711
  datafile$editor_id[datafile$last_name == "lloyd" &
    datafile$first_name == "jon"] <- 1719
  datafile$editor_id[datafile$last_name == "sauer" &
    datafile$first_name == "john"] <- 1854
  datafile$editor_id[datafile$last_name == "walters" &
    datafile$first_name == "jeffrey"] <- 1936
  datafile$editor_id[datafile$last_name == "mcgraw" &
    datafile$first_name == "kevin"] <- 2048
  datafile$editor_id[datafile$last_name == "thrall" &
    datafile$first_name == "peter"] <- 2930
  datafile$editor_id[datafile$last_name == "schmidt" &
    datafile$first_name == "kenneth"] <- 2082
  datafile$editor_id[datafile$last_name == "scribner" &
    datafile$first_name == "kim"] <- 2085
  datafile$editor_id[datafile$last_name == "smith" &
    datafile$first_name == "kimberly"] <- 2094
  datafile$editor_id[datafile$last_name == "fahrig" &
    datafile$first_name == "lenore"] <- 2144
  datafile$editor_id[datafile$last_name == "giraldeau" &
    datafile$first_name == "luc-alain"] <- 2155
  datafile$editor_id[datafile$last_name == "ball" &
    datafile$first_name == "marilyn"] <- 2287
  datafile$editor_id[datafile$last_name == "hellberg" &
    datafile$first_name == "michael"] <- 2409
  datafile$editor_id[datafile$last_name == "lawton" &
    datafile$first_name == "marcy"] <- 2443
  datafile$editor_id[datafile$last_name == "leibold" &
    datafile$first_name == "mathew"] <- 2445
  datafile$editor_id[datafile$last_name == "ryan" &
    datafile$first_name == "michael"] <- 2547
  datafile$editor_id[datafile$last_name == "buchmann" &
    datafile$first_name == "nina"] <- 2632
  datafile$editor_id[datafile$last_name == "sodhi" &
    datafile$first_name == "navjot"] <- 2693
  datafile$editor_id[datafile$last_name == "sala" &
    datafile$first_name == "osvaldo"] <- 2737
  datafile$editor_id[datafile$last_name == "kareiva" &
    datafile$first_name == "peter"] <- 2845
  datafile$editor_id[datafile$last_name == "lundberg" &
    datafile$first_name == "per"] <- 2861
  datafile$editor_id[datafile$last_name == "matson" &
    datafile$first_name == "pamela"] <- 2864
  datafile$editor_id[datafile$last_name == "stouffer" &
    datafile$first_name == "philip"] <- 2928
  datafile$editor_id[datafile$last_name == "brumfield" &
    datafile$first_name == "robb"] <- 2998
  datafile$editor_id[datafile$last_name == "crozier" &
    datafile$first_name == "ross"] <- 3027
  datafile$editor_id[datafile$last_name == "montgomerie" &
    datafile$first_name == "robert"] <- 3160
  datafile$editor_id[datafile$last_name == "safran" &
    datafile$first_name == "rebecca"] <- 3208
  datafile$editor_id[datafile$last_name == "sage" &
    datafile$first_name == "rowan"] <- 3209
  datafile$editor_id[datafile$last_name == "zink" &
    datafile$first_name == "robert"] <- 3261
  datafile$editor_id[datafile$last_name == "brown" &
    datafile$first_name == "sandra"] <- 3290
  datafile$editor_id[datafile$last_name == "mcwilliams" &
    datafile$first_name == "scott"] <- 3404
  datafile$editor_id[datafile$last_name == "steppan" &
    datafile$first_name == "scott"] <- 3481
  datafile$editor_id[datafile$last_name == "day" &
    datafile$first_name == "troy"] <- 3533
  datafile$editor_id[datafile$last_name == "hirose" &
    datafile$first_name == "tadaki"] <- 3566
  datafile$editor_id[datafile$last_name == "williams" &
    datafile$first_name == "tony"] <- 3661
  datafile$editor_id[datafile$last_name == "berger" &
    datafile$first_name == "uta"] <- 3663
  datafile$editor_id[datafile$last_name == "grimm" &
    datafile$first_name == "volker"] <- 3684
  datafile$editor_id[datafile$last_name == "smith" &
    datafile$first_name == "val"] <- 3700
  datafile$editor_id[datafile$last_name == "karasov" &
    datafile$first_name == "william"] <- 3756
  datafile$editor_id[datafile$last_name == "koenig" &
    datafile$first_name == "walter"] <- 3760
  datafile$editor_id[datafile$last_name == "schlesinger" &
    datafile$first_name == "william"] <- 3794
  datafile$editor_id[datafile$last_name == "smith" &
    datafile$first_name == "james"] <- 3907
  datafile$editor_id[datafile$last_name == "bolnick" &
    datafile$first_name == "daniel"] <- 631
  datafile$editor_id[datafile$last_name == "hall" &
    datafile$first_name == "spencer"] <- 3346
  datafile$editor_id[datafile$last_name == "hughes" &
    datafile$first_name == "kimberly"] <- 2017
  datafile$editor_id[datafile$last_name == "martinezdelrio" &
    datafile$first_name == "carlos"] <- 520
  datafile$editor_id[datafile$last_name == "mcgraw" &
    datafile$first_name == "kevin"] <- 2048
  datafile$editor_id[datafile$last_name == "mcwilliams" &
    datafile$first_name == "scott"] <- 3404


  datafile$editor_id[datafile$last_name == "ritland" &
    datafile$first_name == "kermit"] <- 2076
  datafile$editor_id[datafile$last_name == "young" &
    datafile$YEAR == 2014 & datafile$journal == "MARECOL"] <- 602
  datafile$editor_id[datafile$last_name == "jarvis" &
    datafile$first_name == "paul"] <- 2840
  datafile$editor_id[datafile$last_name == "odonnell" &
    datafile$first_name == "sean"] <- 3420
  datafile$editor_id[datafile$last_name == "hurst" &
    datafile$first_name == "gregory"] <- 1208
  datafile$editor_id[datafile$last_name == "fincham" &
    datafile$first_name == "a"] <- 72
  datafile$editor_id[datafile$last_name == "henderson-sellers" &
    datafile$first_name == "ann"] <- 100
  datafile$editor_id[datafile$last_name == "mou" &
    datafile$first_name == "pu"] <- 2527
  datafile$editor_id[datafile$last_name == "rossberg" &
    datafile$first_name == "axel"] <- 183
  datafile$editor_id[datafile$last_name == "mou" &
    datafile$first_name == "pu"] <- 2527
  datafile$editor_id[datafile$last_name == "thrall" &
    datafile$first_name == "peter"] <- 2938

  datafile$editor_id[datafile$last_name == "vandermaarel" &
    (datafile$first_name == "e" | datafile$first_name == "eddy")] <- 1033
  datafile$editor_id[datafile$last_name == "hepinstall-cymerman" &
    datafile$first_name == "j"] <- 1533

  datafile$editor_id[datafile$last_name == "dyer" &
    datafile$first_name == "andrew" &
    datafile$inst == "university of south carolina aiken"] <- 3849

  datafile$editor_id[datafile$last_name == "field" &
    datafile$first_name == "chris" &
    datafile$journal == "gcb"] <- 447

  datafile$editor_id[datafile$last_name == "franklin" &
    datafile$first_name == "janet" &
    datafile$journal == "leco"] <- 3889

  datafile$editor_id[datafile$last_name == "houck" &
    datafile$first_name == "lynne" &
    datafile$journal == "amnat"] <- 2174

  datafile$editor_id[datafile$last_name == "lawes" &
    datafile$first_name == "michael" &
    datafile$journal == "plantecol"] <- 2442

  datafile$editor_id[datafile$last_name == "lack" &
    datafile$first_name == "andrew" &
    datafile$middle_name == "j"] <- 130

  datafile$editor_id[datafile$last_name == "lomolino" &
    datafile$first_name == "mark" &
    datafile$middle_name == "v"] <- 2453

  datafile$editor_id[datafile$last_name == "long" &
    datafile$first_name == "stephen" &
    datafile$journal == "gcb"] <- 3395

  datafile$editor_id[datafile$last_name == "niemela" &
    datafile$first_name == "j" &
    datafile$journal == "leco"] <- 1787

  datafile$editor_id[datafile$last_name == "rozema" &
    datafile$first_name == "j" &
    datafile$journal == "plantecol"] <- 1849


  datafile$editor_id[datafile$last_name == "tjoelker" &
    datafile$first_name == "m" &
    datafile$journal == "newphyt"] <- 2577

  datafile$editor_id[datafile$last_name == "wagner" &
    datafile$first_name == "helene" &
    datafile$journal == "leco"] <- 1373


  datafile$editor_id[datafile$last_name == "vandermeijden" &
    datafile$first_name == "eddy"] <- 1036

  datafile$editor_id[datafile$last_name == "dekroon" &
    datafile$first_name == "hans"] <- 1296

  datafile$editor_id[datafile$last_name == "vangroenendael" &
    datafile$first_name == "jan"] <- 1924

  datafile$editor_id[datafile$last_name == "hansson" &
    datafile$first_name == "lennart" &
    datafile$inst == "swedish university of agricultural sciences"] <- 2162

  datafile$editor_id <- as.factor(datafile$editor_id)

  return(datafile)
}
