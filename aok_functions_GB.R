
#In this section we define the aggregation functions we use. The detail of each function is detailled in REACH_validation_aok_function.pdf.

#A simple function to test if a result is not 
'%!in%' <- function(x,y)!('%in%'(x,y))

aok_mode <-function(x) {
  
  if(all(is.na(x))){return("")}
  
  else{
    
    x <- x[x %!in% c("")]
    
    if(length(x) == 0){return("")}
    
    else{
      
      x = table(x)
      
      modes = sum(x == max(x))
      
      if(modes == 1){return(names(which(x == max(x))))}
      
      else {return("NC")}
    }}}

aok_yes <-function(x) {
  
  if(all(is.na(x))){return("")}
  
  else{
    
    x <- x[x %!in% c("")]
    
    if(length(x) == 0){return("")}
    
    else{
      
      x = table(x)
      
      modes = sum(x == max(x))
      
      if(modes == 1){return(names(which(x == max(x))))}
      
      else if(modes > 1){
        if("Oui" %in% names(which(x == max(x)))){"Oui"} else {return("NC")}
      }
      else {return("NC")}
    }
  }
  }

aok_no <-function(x) {
  
  if(all(is.na(x))){return("")}
  
  else{
    
    x <- x[x %!in% c("")]
    
    if(length(x) == 0){return("")}
    
    else{
      
      x = table(x)
      
      modes = sum(x == max(x))
      
      if(modes == 1){return(names(which(x == max(x))))}
      
      else if(modes > 1){
        if("Non" %in% names(which(x == max(x)))){"Non"} else {return("NC")}
      }
      else {return("NC")}
    }}}

aok_false <-function(x) {
  
  if(all(is.na(x))){return("")}
  
  else{
    
    x <- x[x %!in% c("")]
    
    if(length(x) == 0){return("")}
    
    else{
      
      x = table(x)
      
      modes = sum(x == max(x))
      
      if(modes == 1){return(names(which(x == max(x))))}
      
      else if(modes > 1){
        if("0" %in% names(which(x == max(x)))){"0"} else {return("NC")}
      }
      else {return("NC")}
    }}}

aok_true <-function(x) {
  
  if(all(is.na(x))){return("")}
  
  else{
    
    x <- x[x %!in% c("")]
    
    if(length(x) == 0){return("")}
    
    else{
      
      x = table(x)
      
      modes = sum(x == max(x))
      
      if(modes == 1){return(names(which(x == max(x))))}
      
      else if(modes > 1){
        if("1" %in% names(which(x == max(x)))){"1"} else {return("NC")}
      }
      else {return("NC")}
    }}}


aok_recent <- function(x) {
  
  if(all(is.na(x))){return("")}
  
  else{
    
    x <- x[x %!in% c("")]
    
    if(length(x) == 0){return("")}
    
    else{
      
      x = factor(x, levels = c("Au cours du dernier mois",
                               "Au cours des derniers 2-3 mois",
                               "Au cours des derniers 4-6 mois",
                               "Au cours des derniers 7 mois ?? 1 an",
                               "Il y a plus d'un an",
                               "Pas de r?ponse / ne souhaite pas r?pondre",
                               "Je ne sais pas"), ordered = T)
      
      y = c("Au cours du dernier mois",
            "Au cours des derniers 2-3 mois",
            "Au cours des derniers 4-6 mois",
            "Au cours des derniers 7 mois ?? 1 an",
            "Il y a plus d'un an")
      
      x = table(x)
      
      modes = sum(x == max(x))
      
      if(modes == 1){names(x[x == max(x)])}
      
      else if(modes > 1 & sum(x) <= 2){
        if(any(y %in% names(x[x == max(x)]))){names(x[x == max(x)])[1]} else {return("NC")}
      }
      else {return("NC")}
    }}}

aok_recent_trimestre <- function(x) {
  
  if(all(is.na(x))){return("")}
  
  else{
    
    x <- x[x %!in% c("")]
    
    if(length(x) == 0){return("")}
    
    else{
      
      
      x = factor(x, levels = c(label_debut_dernier_mois,
                               label_T4,
                               label_T3,
                               label_T2,
                               label_T1,
                               label_T0), ordered = T)
      
      y = c(label_debut_dernier_mois,
            label_T4,
            label_T3,
            label_T2,
            label_T1,
            label_T0)
      x = table(x)
      
      modes = sum(x == max(x))
      
      if(modes == 1){names(x[x == max(x)])}
      
      else if(modes > 1 & sum(x) <= 2){
        if(any(y %in% names(x[x == max(x)]))){names(x[x == max(x)])[1]} else {return("NC")}
      }
      else {return("NC")}
    }}}

aok_longest1 <- function(x) {
  
  if(all(is.na(x))){return("")}
  
  else{
    
    x <- x[x %!in% c("")]
    
    if(length(x) == 0){return("")}
    
    else{
      
      x = factor(x, levels = c("Plus d'une demi-journ?e",
                               "Plus de 2 heures ? une demi-journ?e",
                               "45 minutes ? 2 heures",
                               "Moins de 45 minutes",
                               "Pas de r?ponse / ne souhaite pas r?pondre",
                               "Je ne sais pas"), ordered = T)
      
      y = c("Plus d'une demi-journ?e", "Plus de 2 heures ? une demi-journ?e")
      
      x = table(x)
      
      modes = sum(x == max(x))
      
      if(modes == 1){names(x[x == max(x)])}
      
      else if(modes > 1 & sum(x) <= 2){
        if(any(y %in% names(x[x == max(x)]))){names(x[x == max(x)])[1]} else {return("NC")}
      }
      else {return("NC")}
    }}}

aok_longest2 <- function(x) {
  
  if(all(is.na(x))){return("")}
    
    x <- x[x %!in% c("")]
    
    if(length(x) == 0){return("")}
 
      x = factor(x, levels = c("Plus d'une demi-journ?e / pas de centre de sant? disponible",
                               "Plus de deux heures ? une demi-journ?e",
                               "Entre 45 minutes et 2 heures",
                               "Moins de 45 minutes",
                               "Pas de r?ponse / ne souhaite pas r?pondre",
                               "Je ne sais pas"), ordered = T)
      
      y = c("Plus d'une demi-journ?e / pas de centre de sant? disponible",
            "Plus de deux heures ? une demi-journ?e")
      
      x = table(x)
      
      modes = sum(x == max(x))
      
      if(modes == 1){names(x[x == max(x)])}
      
      else if(modes > 1 & sum(x) <= 2){
        if(any(y %in% names(x[x == max(x)]))){names(x[x == max(x)])[1]} else {return("NC")}
      }
      else {return("NC")}
    }

