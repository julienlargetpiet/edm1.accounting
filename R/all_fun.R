#' cost_and_taxes
#'
#' Allow to calculate basic variables related to cost and taxes from a bunch of products (elements). So put every variable you know in the following order:
#' @param qte is the quantity of elements
#' @param pu is the price of a single elements without taxes
#' @param prix_ht is the duty-free price of the whole set of elements
#' @param tva is the percentage of all taxes
#' @param prix_ttc is the price of all the elements with taxes
#' @param prix_tva is the cost of all the taxes
#' @param pu_ttc is the price of a single element taxes included
#' @param adjust is the discount percentage
#' @param prix_d_ht is the free-duty price of an element after discount
#' @param prix_d_ttc is the price with taxes of an element after discount
#' @param pu_d is the price of a single element after discount and without taxes
#' @param pu_d_ttc is the free-duty price of a single element after discount
#' @examples
#'
#' print(cost_and_taxes(pu=45, prix_ttc=2111, qte=23))
#' 
#' # [1]   23.000000   45.000000   45.000000    1.039614 2111.000000 1076.000000
#' #[7]   45.000000          NA          NA          NA          NA          NA
#'
#' @export

cost_and_taxes <- function(qte=NA, pu=NA, prix_ht=NA, tva=NA, prix_ttc=NA,
                           prix_tva=NA, pu_ttc=NA, adjust=NA, prix_d_ht=NA,
                           prix_d_ttc=NA, pu_d=NA, pu_d_ttc=NA){
  
  val_l <- c(qte, pu, prix_ht, tva, prix_ttc, prix_tva, pu_ttc, adjust,
             prix_d_ht, prix_d_ttc, pu_d, pu_d_ttc)
  
  already <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  
  rtnl <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
  
  for (i in 1:length(already)){
    
    if (is.na(val_l[i]) == FALSE){
      
      already[i] <- 1
      
      rtnl[i] <- val_l[i]
      
    }
    
  }
  
  for (i in 1:16){
    
    if (is.na(prix_ttc) == FALSE & is.na(prix_d_ttc) == FALSE & already[8] == 0){
      
      adjust <- prix_ttc / prix_d_ttc - 1
      
      already[8] <- 1
      
      rtnl[8] <- adjust
      
    }

    if (is.na(prix_ht) == FALSE & is.na(prix_d_ht) == FALSE & already[8] == 0){
      
      adjust <- prix_ht / prix_d_ht - 1
      
      already[8] <- 1
      
      rtnl[8] <- adjust
      
    }
    
    if (is.na(pu_ttc) == FALSE & is.na(pu_d_ttc) == FALSE & already[8] == 0){
      
      adjust <- pu_ttc / pu_d_ttc - 1
      
      already[8] <- 1
      
      rtnl[8] <- adjust
      
    }

    if (is.na(pu) == FALSE & is.na(pu_d) == FALSE & already[8] == 0){
      
      adjust <- pu / pu_d - 1
      
      already[8] <- 1
      
      rtnl[8] <- adjust
      
    }

    if (is.na(qte) == FALSE & is.na(pu_d) == 0 & already[9] == 0){
      
      prix_d_ht <- qte * pu_d
      
      already[9] <- 0
      
      rtnl[9] <- prix_d_ht
      
    }
    
    if (is.na(qte) == FALSE & is.na(pu_d_ttc) == 0 & already[10] == 0){
      
      prix_d_ttc <- qte * pu_d_ttc
      
      already[10] <- 0
      
      rtnl[10] <- prix_d_ht
      
    }
    
    if (is.na(prix_d_ttc) == FALSE & is.na(qte) == FALSE & already[12] == 0){
      
      pu_d_ttc <- prix_d_ttc / qte
      
      already[12] <- 1
      
      rtnl[12] <- pu_d_ttc
      
    }
    
    if (is.na(prix_d_ht) == FALSE & is.na(qte) == FALSE & already[11] == 0){
      
      pu_d <- prix_d_ht / qte
      
      already[11] <- 1
      
      rtnl[11] <- pu_d
      
    }
    
    if (is.na(adjust) == FALSE & is.na(prix_ttc) == FALSE & already[10] == 0){
      
      prix_d_ttc <- prix_ttc * (1 - adjust)
      
      already[10] <- 1
      
      rtnl[10] <- prix_d_ttc
      
    }
    
    if (is.na(adjust) == FALSE & is.na(prix_ht) == FALSE & already[9] == 0){
      
      prix_d_ht <- prix_ht * (1 - adjust)
      
      already[9] <- 1
      
      rtnl[9] <- prix_d_ht
      
    }
    
    if (is.na(adjust) == FALSE & is.na(prix_d_ht) == FALSE & already[3] == 0){
      
      prix_ht <- prix_d_ht * (1 / (1 - adjust))
      
      already[3] <- 1
      
      rtnl[3] <- prix_ht
      
    }
    
    if (is.na(adjust) == FALSE & is.na(prix_d_ttc) == FALSE & already[5] == 0){
      
      prix_ttc <- prix_d_ttc * (1 / (1 - adjust))
      
      already[5] <- 1
      
      rtnl[5] <- prix_ttc
      
    }
    
    if (is.na(pu) == FALSE & is.na(pu_ttc) == FALSE & already[4] == 0){
      
      tva <- pu_ttc / pu - 1
      
      already[4] <- 1
      
      rtnl[4] <- tva
      
    }

    if (is.na(pu_d_ttc) == FALSE & is.na(pu_d) == FALSE & already[4] == 0){
      
      tva <- pu_d_ttc / pu_d - 1
      
      already[4] <- 1
      
      rtnl[4] <- tva
      
    }

    if (is.na(prix_d_ttc) == FALSE & is.na(prix_d_ht) == FALSE & already[4] == 0){
      
      tva <- prix_d_ttc / prix_d_ht - 1
      
      already[4] <- 1
      
      rtnl[4] <- tva
      
    }
    
    if (is.na(prix_ht) == FALSE & is.na(pu) == FALSE & already[1] == 0){
      
      qte <- prix_ht / pu
      
      rtnl[1] <- as.integer(qte)
      
      already[1] <- 1
      
    }
    
    if (is.na(prix_ttc) == FALSE & is.na(pu_ttc) == FALSE & already[1] == 0){
      
      qte <- prix_ttc / pu_ttc
      
      rtnl[1] <- as.integer(qte)
      
      already[1] <- 1
      
    }
    
    if (is.na(prix_d_ht) == FALSE & is.na(pu_d) == FALSE & already[1] == 0){
      
      qte <- prix_d_ht / pu_d
      
      rtnl[1] <- as.integer(qte)
      
      already[1] <- 1
      
    }
    
    if (is.na(prix_d_ttc) == FALSE & is.na(pu_d_ttc) == FALSE & already[1] == 0){
      
      qte <- prix_d_ttc / pu_d_ttc
      
      rtnl[1] <- as.integer(qte)
      
      already[1] <- 1
      
    }
    
    if (is.na(prix_ht) == FALSE & is.na(qte) == FALSE & already[2] == 0){
      
      pu <- prix_ht / qte
      
      rtnl[2] <- pu
      
      already[2] <- 1
      
    }
    
    if (is.na(prix_ttc) == FALSE & is.na(qte) == FALSE & already[7] == 0){
      
      pu_ttc <- prix_ttc / qte
      
      rtnl[7] <- pu
      
      already[7] <- 1
      
    }
    
    if (is.na(pu) == FALSE & is.na(qte) == FALSE & already[3] == 0){
      
      prix_ht <- pu * qte
      
      rtnl[3] <- pu
      
      already[3] <- 1
      
    }
    
    if (is.na(pu_ttc) == FALSE & is.na(qte) == FALSE & already[5] == 0){
      
      prix_ttc <- pu_ttc * qte
      
      rtnl[5] <- pu
      
      already[5] <- 1
      
    }
    
    if (is.na(pu) == FALSE & is.na(qte) == FALSE & already[3] == 0){
      
      prix_ht <- pu * qte
      
      rtnl[3] <- prix_ht
      
      already[3] <- 1
      
    }
    
    if (is.na(prix_ht) == FALSE & is.na(prix_ttc) == FALSE & already[4] == 0){
      
      tva <- prix_ttc / prix_ht - 1
      
      rtnl[4] <- tva
      
      already[4] <- 1
      
    }
    
    if (is.na(tva) == FALSE & is.na(prix_ttc) == FALSE & already[3] == 0){
      
      prix_ht <- prix_ttc / (1 + tva)
      
      rtnl[3] <- prix_ht
      
      already[3] <- 1
      
    }
    
    if (is.na(tva) == FALSE & is.na(prix_ht) == FALSE & already[5] == 0){
      
      prix_ttc <- prix_ht * (1 + tva) 
      
      rtnl[5] <- prix_ttc
      
      already[5] <- 1
      
    }  
    
    if (is.na(prix_ht) == FALSE & is.na(prix_ttc) == FALSE & already[6] == 0){
      
      prix_tva <- prix_ttc - prix_ht
      
      rtnl[6] <- prix_tva
      
      already[6] <- 1
      
    }
    
    if (is.na(tva) == FALSE & is.na(prix_ttc) == FALSE & already[6] == 0){
      
      prix_tva <- tva * prix_ht
      
      rtnl[6] <- prix_tva
      
      already[6] <- 1
      
    }
    
  }
  
  return(rtnl)
  
}

