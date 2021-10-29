### 01 TEST ANALYSE POUR NORD KIVU probleme d'effectifs

## 01.01 Check if the problem is with capital spelling
LOC_AGG_DT$C_localite_jointure <- paste(LOC_AGG_DT$C_zone_sante, LOC_AGG_DT$C_nom_localite_final)

LOC_AGG_DT %>% filter(C_province == "Nord-Kivu") %>% pull(C_localite_jointure) %>% unique %>% length
LOC_AGG_DT %>% filter(C_province == "Nord-Kivu") %>% pull(C_localite_jointure) %>% tolower %>% unique %>% length

## 01.02 Check on one variable with no skip logic (F1_lieu_de_soin) if we have a difference before and after merging with the coverage map
F1_total <- FULL_ZS %>% filter(C_province == "Nord-Kivu" & variable == "F1_lieu_de_soin")
FULL_ZS %>% filter(C_province == "Nord-Kivu" & variable == "F1_lieu_de_soin") %>% pull(n) %>% sum

### Renamed the FULL_ZS as merged in line 1403 of main script
# OLD VERSION
FULL_ZS_merged = FULL_ZS_bind[ZS[,c("province","zs", cover_status), with=F], 
                              on=c("C_province==province","C_zone_sante==zs")]
# NEW VERSION
FULL_ZS_merged <- dplyr::left_join(FULL_ZS_bind, ZS, by="C_zone_sante")

FULL_ZS_merged %>% filter(C_province == "Nord-Kivu" & variable == "F1_lieu_de_soin") %>% pull(n) %>% sum

## 01.03 Check if the problem occurs when renaming the variables as R_names
ZS_total <- FULL_ZS %>% filter(C_province == "Nord-Kivu") %>% pull(C_zone_sante)
ZS_fichier_couverture <- ZS %>% filter(province == "Nord-Kivu") %>% pull(zs)

ZS_total %>% unique
ZS_fichier_couverture

F1_total %<>% as.data.frame(stringsAsFactors == FALSE)

F1_subbed <- F1_total %>% filter(N > 0) 
F1_subbed$C_zone_sante <- as.character(F1_subbed$C_zone_sante)

ZS$C_zone_sante <- ZS$zs

### 01.04 Run the aggregation function just on this variable
keycols <- c("C_province", "C_zone_sante", "F1_lieu_de_soin")
setkeyv(LOC_AGG_DT, keycols)

a = LOC_AGG_DT[CJ(C_province, C_zone_sante, levels(LOC_AGG_DT[["F1_lieu_de_soin"]]), unique = TRUE),
               .(n = .N), by = .EACHI]

a = a[!is.na(a[["F1_lieu_de_soin"]])]

a = a[a[["F1_lieu_de_soin"]] != "SL"]

a = a[a[, .(N = sum(n)), by = c("C_province", "C_zone_sante")],
      on=c("C_province", "C_zone_sante")]

a[, prop := round(n/N*100,2)]

a[, variable := "F1_lieu_de_soin"]

a


#### 02 TEST analyse SK et TNG effectifs aggregation
LOC_AGG_DT %>% filter(C_province == "Sud-Kivu") %>% pull(C_localite_jointure) %>% unique %>% length

# 02.01 See if the problem occurs from renaming
## AVANT DE CHANGER LES NOMS
KI_CLEAN_DT$C_localite_jointure <- paste0(KI_CLEAN_DT$`0.2.b)  Quelle est la zone de santé dans laquelle se trouve la zone d'intérêt pour cette enquête ?`,
                                         KI_CLEAN_DT$`Localité sectionnée`)

KI_CLEAN_DT %>% filter(`0.2.a) Quelle est la province dans laquelle se trouve la zone d'intérêt pour cette enquête ?` == "Sud-Kivu") %>% pull(C_localite_jointure) %>% unique %>% length


## APRES AVOIR CHANGE LES NOMS
KI_CLEAN_DT$C_localite_jointures <- paste0(KI_CLEAN_DT$C_zone_sante, KI_CLEAN_DT$C_nom_localite_final)

KI_CLEAN_DT_no_dupl %>% filter(C_province == "Tanganyika") %>% pull(C_localite_jointures) %>% unique %>% length

localites_excel <- read.csv2("./input/data/csv_jointure_SK.csv", stringsAsFactors = FALSE, encoding = "UTF-8")

## Length of unique ones
KI_CLEAN_DT_no_dupl <- KI_CLEAN_DT[,-311]
KI_CLEAN_DT_no_dupl  %>% filter(K1_temps_acces_source_eau == "Plus de 2 heures ? une demi-journ?e", C_province == "Tanganyika") %>% select(C_nom_localite_final)

KI_CLEAN_DT %>% filter(C_province == "Sud-Kivu") %>% pull(C_localite_jointures) %>% unique %>% length

## Length of total ones
localites_excel$localite.jointure  %>% length
KI_CLEAN_DT %>% filter(C_province == "Sud-Kivu") %>% pull(C_localite_jointures)%>% length

### Find the culprits
KI_CLEAN_DT_SK <- KI_CLEAN_DT %>% filter(C_province == "Sud-Kivu")
KI_CLEAN_DT_SK$C_localite_jointures[!KI_CLEAN_DT_SK$C_localite_jointures %in% localites_excel$localite.jointure]
KI_CLEAN_DT_SK$C_localite_jointures[!KI_CLEAN_DT_SK$C_localite_jointures %in% localites_excel$localite.jointure] %>% unique


localites_excel$localite.jointure[!localites_excel$localite.jointure %in% KI_CLEAN_DT_SK$C_localite_jointures]
localites_excel$localite.jointure[!localites_excel$localite.jointure %in% KI_CLEAN_DT_SK$C_localite_jointures] %>% unique

### 03 TEST SUBSET VARIABLES THAT SHOULD HAVE X EFFECTIF BEFORE AND AFTER ANALYSIS 
#"D3_date_dernier_mouvement_pdi"; "D3_date_dernier_mouvement_retournes";  "N1_presence_comite_communautaire";  "N1_acces_comite_communautaire_pdi" "N1_participation_structure_communautaire_pdi"
LOC_AGG_DT_kl <- LOC_AGG_DT %>% filter(C_province == "Tanganyika")
LOC_AGG_DT_kl$N1_presence_comite_communautaire %>% table
LOC_AGG_DT_kl$N1_comite_communautaire_type[LOC_AGG_DT_kl$N1_presence_comite_communautaire!="Oui"]<-"SL"
LOC_AGG_DT_kl$N1_comite_communautaire_type %>% table
LOC_AGG_DT_kl$E3_variation_prix_cereale %>% table # effective N (augmentation 30 jours) = 127
LOC_AGG_DT_kl$E3_augmentation_prix_cereale_raisons_conflits %>% table # 1 = 3, 0 = 124, TOTAL = 127
LOC_AGG_DT_kl$E7_pertubation_activites_subsistance %>% table # Oui = 60
LOC_AGG_DT_kl$E7_perte_acces_activites_subsistance_travail_journalier %>% table # N = 60
LOC_AGG_DT_kl$F1_temps_pied_structure_sante %>% table # 47 en NC + 7 Plus d'une demi j = 54
LOC_AGG_DT_kl$F1_type_structure_sante_hopital %>% table # 48 en 

# NORTH KIVU CHECKS
LOC_AGG_DT_nk <- LOC_AGG_DT %>% filter(C_province == "Nord-Kivu")
LOC_AGG_DT_nk$D1_groupe_population_present_pdi %>% table
LOC_AGG_DT_nk$G4_relation_deplaces_pnd[LOC_AGG_DT_nk$D1_groupe_population_present_pdi != "Oui"] <- "SL"
LOC_AGG_DT_nk$G4_relation_deplaces_pnd %>% table
#I1_lieux_principal_pdi_retournes_site_spontane
#I1_type_abris_pdi_retournes_sursitespontane
LOC_AGG_DT_nk$D2_proportion_groupe_moitiee_hote %>% table
LOC_AGG_DT_nk$D1_groupe_population_present_pnd %>% table
LOC_AGG_DT_nk$D1_groupe_population_present_retourne %>% table # 30 NC
LOC_AGG_DT_nk$D1_groupe_population_present_pnd %>% table #272 oui
## SHELTER TESTS
LOC_AGG_DT_nk$I1_abris_principal_hote %>% table #272 N
LOC_AGG_DT_nk$I1_lieux_principal_pdi_retournes_site_spontane %>% table # 27 oui
LOC_AGG_DT_nk$I1_type_abris_pdi_retournes_sursitespontane %>% table # N = 27 dont un est blank

# SOUTH KIVU TESTS
LOC_AGG_DT_sk <- LOC_AGG_DT %>% filter(C_province == "Sud-Kivu")
LOC_AGG_DT_sk$D1_groupe_population_present_pdi %>% table
LOC_AGG_DT_sk$D3_date_dernier_mouvement_pdi %>% table
LOC_AGG_DT_sk$F1_temps_pied_structure_sante %>% table # 47 en NC + 7 Plus d'une demi j = 54
LOC_AGG_DT_sk$F1_type_structure_sante_hopital %>% table # 48 en 
LOC_AGG_DT_sk$D2_hote_partie_localite %>% table # 195 Non, pas de NCSL
LOC_AGG_DT_sk$D2_raison_partir_hote %>% table # SL = 195

#GENERAL TESTS
LOC_AGG_DT$I2_abris_detruits %>% table#574 Non+ 1 SL = 575, 517 OUi
LOC_AGG_DT$I2_raison_abris_detruits %>% table # 575 SL
LOC_AGG_DT$D1_groupe_population_present_pdi %>% table # 302 Non, 66 NC = 368
LOC_AGG_DT$D3_date_dernier_mouvement_pdi %>% table # SL = 368, should have the option last month and SL = to "no" in previous
