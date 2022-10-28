#' Codon Usage Comparison Function
#'
#' This function assists in determining if synonymous mutations are likely silent
#' @param reference_codons list of reference codons from sequence alignments
#' @param mutant_codons list of corresponding mutated codons from sequence alignments
#' @param species "staph" or "e.coli", specifies which codon usage table will be used
#' @return summary table comparing usage frequencies of reference codons to corresponding mutated codons
#' @export

codon_checker <- function(reference_codons, mutant_codons, species){
  reference_sequence <- data.frame(codon = reference_codons)
  mutant_sequence <- data.frame(codon = mutant_codons)


  if (species == "staph") {
    load(file = 'data/staph.rda')
    codon_table <- staph
  }

  else if (species == "e.coli") {
    load(file = 'data/ecoli.rda')
    codon_table <- ecoli
  }

  ids_ref <- match(reference_sequence$codon, codon_table$codon)
  ref_freq <- codon_table[ids_ref,]

  ids_mut <- match(mutant_sequence$codon, codon_table$codon)
  mut_freq <- codon_table[ids_mut,]

  summary_table <- cbind(ref_freq, mut_freq)
  names(summary_table) <- c("Ref. Codon", "Ref. Frequency",
                            "Mutant Codon", "Mutant Frequency")

  summary_table$`Freq difference` <- as.numeric(summary_table$`Ref. Frequency`) - as.numeric(summary_table$`Mutant Frequency`)

  return(summary_table)
}

