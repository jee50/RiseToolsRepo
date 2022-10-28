#' Send for Sequencing Function
#'
#' This function eliminates manual calculations related to sample preparation of plasmid or PCR product sequencing
#' @param method "PCR" or "plasmid", refers to type of sample being prepared
#' @param length length of amplicon, required for PCR samples. Defaults to 0
#' @param concentration DNA concentration of samples, in ng/uL
#' @return table with volumes of DNA, water, and primer required for sequencing
#' @export


send_for_sequencing <- function(method, length=0, concentration){
  final_vol <-  15
  primer_vol <- 0

  if(method == "plasmid"){
    final_mass = 450

  }

  if(method == "PCR"){
    primer_vol = 2.5
    if(length < 500){
      final_mass = 10
    }
    else if(length < 1000){
      final_mass = 20
    }
    else if(length < 2000){
      final_mass = 40
    }
    else if(length < 4000){
      final_mass = 60
    }
    else if(length > 4000){
      final_mass = 500
    }
  }

  sample_vol <- final_mass/concentration
  water_vol <- final_vol-primer_vol-sample_vol


  return(cbind(sample_vol, water_vol, primer_vol))

}

