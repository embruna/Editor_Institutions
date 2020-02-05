#FUNCTION TO CORRECT EDITOR NAMES, IDs, and INSTITUTIONS
  editor_cleaner <- function(DATAFILE) {
    
    
    # DATAFILE<-ALLDATA
    # SOME NOTES:
    
    str(DATAFILE$editor_id)
    DATAFILE$INST<-as.factor(DATAFILE$INST)
    levels(DATAFILE$INST) <- c(levels(DATAFILE$INST),"University of Missouri Columbia",
                               "leibniz institute for zoo and wildlife research",
                               "kansas state university",
                               "CNRS Centre dEcologie Fonctionnelle et Evolutive",
                               "Forestry and Forest Products Research Institute",
                               "University of Minnesota Duluth",
                               "University of Minnesota Crookston",
                               "University of Toronto Mississauga",
                               "State University of New York College of Environmental Science and Forestry",
                               "Calyx, Inc.","University of North Carolina Charlotte",
                               "Smithsonian National Museum of Natural History",
                               "Smithsonian National Zoological Park",
                               "Laboratoire Associe de Modelisation des Plantes",
                               "Southern Illinois University",
                               "universite libre de bruxelles",
                               "Free University Amsterdam",
                               "southern illinois u",
                               "Aarhus University",
                               "University of St. Andrews",
                               "university of st andrews",
                               "university of uppsala",
                               "NERC Centre for Population Biology",
                               "Massey University",
                               "University of Bialystok",
                               "manaaki whenua landcare research")
    
    
    #### 
    # These corrections are from PJ review of files
    #####
    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="Bever" & DATAFILE$YEAR==2012 & DATAFILE$JOURNAL=="OECOL"),]
    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="Lichstein" & DATAFILE$YEAR==2012 & DATAFILE$JOURNAL=="OECOL"),]
    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="Gough" & DATAFILE$YEAR==2012 & DATAFILE$JOURNAL=="OECOL"),]
    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="Heimpel" & DATAFILE$YEAR==2012 & DATAFILE$JOURNAL=="OECOL"),]
    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="Ibanez" & DATAFILE$YEAR==2012 & DATAFILE$JOURNAL=="OECOL"),]
    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="Layman" & DATAFILE$YEAR==2012 & DATAFILE$JOURNAL=="OECOL"),]
    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="Le Galliard" & DATAFILE$YEAR==2012 & DATAFILE$JOURNAL=="OECOL"),]

    DATAFILE$INST[DATAFILE$LAST_NAME=="Voigt"& DATAFILE$JOURNAL=="OECOL" &
                    (DATAFILE$YEAR==2013|DATAFILE$YEAR==2014)]<-"leibniz institute for zoo and wildlife research"

    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="Masters" & DATAFILE$YEAR==2010 & DATAFILE$JOURNAL=="JBIOG"),]
    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="Holland" & DATAFILE$YEAR==1985 & DATAFILE$JOURNAL=="JBIOG"),]
    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="Lambers" & DATAFILE$YEAR==1993 & DATAFILE$JOURNAL=="PLANTECOL"),]
    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="Bond" & DATAFILE$YEAR==2004 & DATAFILE$JOURNAL=="PLANTECOL"),]
    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="Kollmann" & DATAFILE$YEAR==2007 & DATAFILE$JOURNAL=="PLANTECOL"),]
    
    DATAFILE$CITY[DATAFILE$LAST_NAME=="Overton"& DATAFILE$JOURNAL=="LECO"]<-NA
    
    DATAFILE$geo.code[DATAFILE$LAST_NAME=="Betts"& DATAFILE$JOURNAL=="LECO"]<-"CAN"
    
    DATAFILE$LAST_NAME[DATAFILE$FIRST_NAME=="Kimberly" & DATAFILE$JOURNAL=="LECO" & DATAFILE$YEAR==2004]<-"With"
    
    DATAFILE$MIDDLE_NAME[DATAFILE$FIRST_NAME=="Kimberly" & DATAFILE$JOURNAL=="LECO" & DATAFILE$YEAR==2004]<-"A"
    
    DATAFILE$editor_id[DATAFILE$FIRST_NAME=="Kimberly" & DATAFILE$JOURNAL=="LECO" & DATAFILE$YEAR==2004]<-"2113"
    
    DATAFILE$LAST_NAME[DATAFILE$LAST_NAME=="Lookinbill" & DATAFILE$JOURNAL=="LECO"]<-"Lookingbill"
    
    
    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="Duggan" & DATAFILE$YEAR==2008),] #not in the front matter
    
    DATAFILE$LAST_NAME[DATAFILE$LAST_NAME=="VanDer" & DATAFILE$JOURNAL=="LECO"]<-"VanDerMaarel"
    DATAFILE$LAST_NAME[DATAFILE$LAST_NAME=="MOYLE" & DATAFILE$FIRST_NAME=="PETER"]<-"Moyle"
    DATAFILE$LAST_NAME[DATAFILE$LAST_NAME=="LI" & DATAFILE$FIRST_NAME=="Feng-Min"]<-"Li"
    DATAFILE$LAST_NAME[DATAFILE$LAST_NAME=="DeWalt" & DATAFILE$FIRST_NAME=="Saara"]<-"Dewalt"
    DATAFILE$LAST_NAME[DATAFILE$LAST_NAME=="DeVictor" & DATAFILE$FIRST_NAME=="Vincent"]<-"Devictor"
    DATAFILE$LAST_NAME[DATAFILE$LAST_NAME=="COE" & DATAFILE$FIRST_NAME=="M"]<-"Coe"
    DATAFILE$LAST_NAME[DATAFILE$LAST_NAME=="MONOD"]<-"Monod"
    
    DATAFILE$FIRST_NAME[DATAFILE$LAST_NAME=="Moyle" & DATAFILE$FIRST_NAME=="PETER"]<-"Peter"
    DATAFILE$FIRST_NAME[DATAFILE$LAST_NAME=="MONOD"]<-"Theodore"
    
    DATAFILE$UNIT[DATAFILE$LAST_NAME=="Meffe" & DATAFILE$JOURNAL=="CONBIO" & DATAFILE$INST=="University of Montana"]<-NA
    DATAFILE$UNIT[DATAFILE$LAST_NAME=="Whitmore"]<-"Oxford Forestry Institute"
    DATAFILE$UNIT[DATAFILE$LAST_NAME=="Gustafson"& DATAFILE$FIRST_NAME=="E"]<-"North Central Research Station"
    DATAFILE$UNIT[DATAFILE$LAST_NAME=="With"& DATAFILE$FIRST_NAME=="Kimberly" & DATAFILE$YEAR==2004]<-"Division of Biology"
    DATAFILE$INST[DATAFILE$LAST_NAME=="With"& DATAFILE$FIRST_NAME=="Kimberly" & DATAFILE$YEAR==2004]<-"kansas state university"
    DATAFILE$STATE[DATAFILE$LAST_NAME=="With"& DATAFILE$FIRST_NAME=="Kimberly" & DATAFILE$YEAR==2004]<-"KS"	
    DATAFILE$CITY[DATAFILE$LAST_NAME=="With"& DATAFILE$FIRST_NAME=="Kimberly" & DATAFILE$YEAR==2004]<-"Manhattan"	
    
    
    DATAFILE$editor_id[DATAFILE$LAST_NAME=="VanDerMaarel" & DATAFILE$JOURNAL=="LECO"]<-"1033"
    DATAFILE$editor_id[DATAFILE$LAST_NAME=="Smith" & DATAFILE$FIRST_NAME=="Melinda"]<-"2566"
    DATAFILE$editor_id[DATAFILE$LAST_NAME=="Smith" & DATAFILE$FIRST_NAME=="Danny"]<-NA
    DATAFILE$editor_id[DATAFILE$LAST_NAME=="Moyle" & DATAFILE$FIRST_NAME=="Peter"]<-2874
    DATAFILE$editor_id[DATAFILE$LAST_NAME=="Li" & DATAFILE$FIRST_NAME=="Feng-Min"]<-1098
    DATAFILE$editor_id[DATAFILE$LAST_NAME=="Lee" & DATAFILE$FIRST_NAME=="Carol"]<-3860
    DATAFILE$editor_id[DATAFILE$LAST_NAME=="Fry" & DATAFILE$FIRST_NAME=="James"]<-3891
    DATAFILE$editor_id[DATAFILE$LAST_NAME=="Dewalt" & DATAFILE$FIRST_NAME=="Saara"]<-3311
    DATAFILE$editor_id[DATAFILE$LAST_NAME=="Devictor" & DATAFILE$FIRST_NAME=="Vincent"]<-3681
    DATAFILE$editor_id[DATAFILE$LAST_NAME=="Coe" & DATAFILE$FIRST_NAME=="M"]<-2332
    
    DATAFILE$INST[DATAFILE$LAST_NAME=="VanDerMaarel" & DATAFILE$JOURNAL=="LECO"]<-"university of uppsala"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Sprules" & DATAFILE$FIRST_NAME=="Gary"]<-"University of Toronto Mississauga"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Wagner" & DATAFILE$FIRST_NAME=="Helene"]<-"University of Toronto Mississauga"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Kotanen" & DATAFILE$FIRST_NAME=="Peter"]<-"University of Toronto Mississauga"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Loiselle" & DATAFILE$FIRST_NAME=="Bette"]<-"University of Missouri St Louis"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Ricklefs" & DATAFILE$FIRST_NAME=="Robert"]<-"University of Missouri St Louis"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Renner" & DATAFILE$FIRST_NAME=="Susanne"]<-"University of Missouri St Louis"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Sork" & DATAFILE$FIRST_NAME=="Victoria"]<-"University of Missouri St Louis"
    # DATAFILE$INST[DATAFILE$LAST_NAME=="Parmentier"]<-"universite libre de bruxelles"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Debussche"]<-"CNRS Centre dEcologie Fonctionnelle et Evolutive"
    # levels(DATAFILE$INST) <- c(levels(DATAFILE$INST),"Forestry and Forest Products Research Institute","University of Minnesota Duluth","University of Minnesota Crookston")
    DATAFILE$INST[DATAFILE$LAST_NAME=="Fujimori"]<-"Forestry and Forest Products Research Institute"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Johnson" & DATAFILE$FIRST_NAME=="Lucinda"]<-"University of Minnesota Duluth"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Parmentier" & DATAFILE$JOURNAL=="BITR"]<-"universite libre de bruxelles"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Moen" & DATAFILE$FIRST_NAME=="Ron"]<-"University of Minnesota Duluth"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Sterner" & DATAFILE$FIRST_NAME=="Robert"]<-"University of Minnesota Duluth"
    DATAFILE$INST[DATAFILE$LAST_NAME=="VanDerHeijden" & DATAFILE$FIRST_NAME=="Marcel"]<-"Free University Amsterdam"
    DATAFILE$INST[DATAFILE$LAST_NAME=="VanDamme" & DATAFILE$FIRST_NAME=="Raoul"]<-"University of Antwerp"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Badyaev" & DATAFILE$FIRST_NAME=="Alexander"]<-"University of Arizona"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Davidowitz" & DATAFILE$FIRST_NAME=="Goggy"]<-"University of Arizona"
    DATAFILE$INST[DATAFILE$LAST_NAME=="McGraw" & DATAFILE$FIRST_NAME=="Kevi"]<-"Arizona State University"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Jones" & DATAFILE$INST=="Ascot"]<-"NERC Centre for Population Biology"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Jones" & DATAFILE$INST=="Cardiff"]<-"Cardiff University"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Raubenheimer" & DATAFILE$FIRST_NAME=="David"]<-"Massey University"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Niu" & DATAFILE$FIRST_NAME=="Shuli"]<-"Chinese Academy of Sciences"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Konarzewski" & DATAFILE$INST=="Bialystok"]<-"University of Bialystok"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Newman" & DATAFILE$INST=="Bristol"]<-"University of Bristol"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Bergelson" & DATAFILE$INST=="Chicago"]<-"University of Chicago"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Turnbull" & DATAFILE$INST=="Christchurch"]<-"University of Canterbury"
    DATAFILE$INST[DATAFILE$LAST_NAME=="McGlone" & DATAFILE$INST=="Christchurch"]<-"manaaki whenua landcare research"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Boggs" & DATAFILE$INST=="Colorado"]<-"Stanford University"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Wiersma" & DATAFILE$FIRST_NAME=="Jochum"]<-"University of Minnesota Crookston"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Smith" & DATAFILE$FIRST_NAME=="Madeleine"]<-"University of Minnesota Crookston"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Sims" & DATAFILE$FIRST_NAME=="Albert"]<-"University of Minnesota Crookston"
    DATAFILE$INST[DATAFILE$JOURNAL=="AMNAT" & DATAFILE$LAST_NAME=="Case"]<-"University of California San Diego"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Noon"]<-"Colorado State University"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Burgess"]<-"State University of New York College of Environmental Science and Forestry"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Fragoso"]<-"State University of New York College of Environmental Science and Forestry"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Yanai"]<-"State University of New York College of Environmental Science and Forestry"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Hall" & DATAFILE$FIRST_NAME=="Charles" & DATAFILE$JOURNAL=="CONBIO"]<-
      "State University of New York College of Environmental Science and Forestry"
    DATAFILE$INST[DATAFILE$INST=="University of Missouri"]<-"University of Missouri Columbia" 
    DATAFILE$INST[DATAFILE$JOURNAL=="AMNAT" & DATAFILE$LAST_NAME=="Case"]<-"University of California San Diego"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Noon"]<-"Colorado State University"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Bowers" & DATAFILE$CITY=="Vadnais Heights"]<-"Calyx, Inc."
    DATAFILE$INST[DATAFILE$LAST_NAME=="Debussche"]<-"CNRS Centre dEcologie Fonctionnelle et Evolutive"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Whitmore"]<-"University of Oxford"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Hails"]<-"University of Oxford"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Leamy"]<-"University of North Carolina Charlotte"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Gustafson"& DATAFILE$FIRST_NAME=="E"]<-"US Forest Service"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Labandeira"& DATAFILE$FIRST_NAME=="Conrad"]<-"Smithsonian National Museum of Natural History"
    DATAFILE$INST[DATAFILE$LAST_NAME=="SEIDENSTZICKER"& DATAFILE$FIRST_NAME=="JOHN"]<-"Smithsonian National Zoological Park"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Kleiman"& DATAFILE$FIRST_NAME=="Devra"]<-"Smithsonian National Zoological Park"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Houllier" & (DATAFILE$YEAR>1998 & DATAFILE$YEAR<2003)]<-NA
    DATAFILE$INST[DATAFILE$LAST_NAME=="Houllier" & (DATAFILE$YEAR>1998 & DATAFILE$YEAR<2003)]<-"Laboratoire Associe de Modelisation des Plantes"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Watling"& DATAFILE$INST=="Adelaide"]<-"University of Adelaide"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Boutin" & DATAFILE$INST=="Alberta"]<-"University of Alberta"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Fox" & DATAFILE$INST=="Alberta"]<-"University of Calgary"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Patek" & DATAFILE$INST=="Amherst"]<-"University of Massachusetts at Amherst"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Mcgraw" & DATAFILE$INST=="Arizona"]<-"Arizona State University"
    
    DATAFILE$COUNTRY[DATAFILE$LAST_NAME=="VanDerHeijden"]<-"Switzerland"
    # levels(DATAFILE$INST) <- c(levels(DATAFILE$INST),"State University of New York College of Environmental Science and Forestry")
    DATAFILE$COUNTRY[DATAFILE$LAST_NAME=="Bieber"]<-"Austria"
    DATAFILE$COUNTRY[DATAFILE$FIRST_NAME=="Jeannine" & DATAFILE$LAST_NAME=="Cavender-Bares"]<-"USA"
    DATAFILE$COUNTRY[DATAFILE$LAST_NAME=="VanDerHeijden"]<-"Switzerland"
    DATAFILE$COUNTRY[DATAFILE$LAST_NAME=="Gandon"]<-"France"
    DATAFILE$COUNTRY[DATAFILE$LAST_NAME=="Westing"&DATAFILE$FIRST_NAME=="Arthur"]<-"Sweden"
    DATAFILE$COUNTRY[DATAFILE$LAST_NAME=="Galdon"&DATAFILE$FIRST_NAME=="Luis"]<-"Spain"
    
    DATAFILE$CITY[DATAFILE$LAST_NAME=="VanDerHeijden" & DATAFILE$INST=="Amsterdam"]<-"Amsterdam"
    
    
  return(DATAFILE)
  
}