import(tidyverse)
import(naniar)
import(phreeqc)
import(purrr)
import(stats)
import(tibble)
import(tidyr)
import(dplyr)
import(ggplot2)


##### Defining functions #####

runModel = function(sol1,
                    chrg.sol1,
                    sol2,
                    chrg.sol2){

  #defining database

  phrLoadDatabase("database/minteq.v4.dat")

  #### SOLUTION 1 ####
  sol1.name <- "SOLUTION 1"

  names(sol1)[names(sol1) == "Alkalinity (mmol/L)"] = "Alkalinity"

  sol1 = tibble(as.data.frame(t(sol1)))


  names(sol2)[names(sol2) == "Alkalinity (mmol/L)"] = "Alkalinity"

  sol2 = tibble(as.data.frame(t(sol2)))
  #sol2 = sol2new

  ### Solution 1 conc. input ###
  # sol1 <- tibble(temp = 20,
  #                pH = 4.74,
  #                units = "mg/L",
  #                Na = 4,
  #                K = 0.4,
  #                Mg = 0.9,
  #                Ca = 1.3,
  #                Al = 0.68,
  #                Fe = 1.23,
  #                Mn = 0.05,
  #                Cl = 4.75,
  #                S = 0.6534,
  #                F = 0.043,
  #                `N(5)` = 0.2259,
  #                P = 0.0028,
  #                Alkalinity = NA,
  #                `O(0)` = 8.68
  # )

  ## Solution 1 unit input
  sol1.unit <- tibble(

                      Alkalinity = "mmol/L",
                      temp = NA,
                      pH = NA,
                      units = NA,
                      Na = NA,
                      K = NA,
                      Mg = NA,
                      Ca = NA,
                      Al = NA,
                      Fe = NA,
                      Cl = NA,
                      S = NA,
                      F = NA,
                      `N(5)` = NA,
                      P = NA,
                      `O(0)` = NA
  )

  ## ion selection for charge balance ##
  chrg.sol1 <- chrg.sol1

  ## Volume 1 ##
  sol1_vol <- 50


  #### SOLUTION 2 ####
  sol2.name <- "SOLUTION 2"


  ## Solution 2 conc. input
  # sol2 <- tibble(temp = 20,
  #                pH = 7.27,
  #                units = "mg/L",
  #                Na = 7.5,
  #                K = 6.5,
  #                Mg = 4.63,
  #                Ca = 28.6,
  #                Al = 0.02,
  #                Fe = 0.09,
  #                Cl = 12.35,
  #                S = 6.16,
  #                F = 0,
  #                `N(5)` = 0.92615,
  #                P = 0.083,
  #                Alkalinity = 1.563,
  #                `O(0)` = 8.68
  # )
  #
  # sol2 = sol2new
  # sol2new
  # all.equal(sol2,sol2new)
  # sol2new[1,!names(sol2new) %in% c("units","Alkalinity")] = as.numeric(sol2new[1,!names(sol2new) %in% c("units","Alkalinity")])
  # sol2new = sol2new[,!names(sol2new) %in% c("Mn")]
  #
  # sol2new = sol2new %>%
  #   mutate(temp = as.numeric(temp),
  #          pH = as.numeric(pH),
  #          Na = as.numeric(Na),
  #          K = as.numeric(K),
  #          Mg = as.numeric(Mg),
  #          Ca = as.numeric(Ca),
  #          Al = as.numeric(Al),
  #          Fe = as.numeric(Fe),
  #          Cl = as.numeric(Cl),
  #          S = as.numeric(S),
  #          F = as.numeric(F),
  #          `N(5)` = as.numeric(`N(5)`),
  #          P = as.numeric(P),
  #          Alkalinity = as.numeric(Alkalinity),
  #          `O(0)` = as.numeric(`O(0)`))
  #
  # sol2ed = sol2 %>%
  #   bind_rows(sol2new)

  #browser()

  ## Solution 2 unit input
  sol2.unit <- tibble(units = NA,
                      Alkalinity = "mmol/L",
                      pH = NA,
                      temp = NA,
                      Na = NA,
                      K = NA,
                      Mg = NA,
                      Ca = NA,
                      Al = NA,
                      Fe = NA,
                      Cl = NA,
                      S = NA,
                      F = NA,
                      `N(5)` = NA,
                      P = NA,
                      `O(0)` = NA
  )

  ## ion selection for charge balance ##
  chrg.sol2 <- chrg.sol2


  ## Volume 2 ##
  sol2_vol <- 50



  #### Mix interval ####
  interval <- 5#precentage mixing





  # END INITIAL INPUT ------------------------------------------------------


  #### Function for solution specifications ####
  specifications.fun <- function(df, df.unit, chrg.bal.ion = NA){

    df.tall <- gather(data = df, label, value)
    df.unit.tall <- gather(data = df.unit, label, unit)
    df.join.tall <- left_join(df.tall, df.unit.tall)

    df.return <- df.join.tall %>%
      filter(!is.na(value)) %>%
      mutate(charge = case_when(label == chrg.bal.ion ~ "charge", TRUE ~ "")) %>%
      mutate(unit = case_when(is.na(unit) ~ "", TRUE ~ unit))

    return(df.return)
  }


  sol1.pre_txt.df <- specifications.fun(sol1, sol1.unit, chrg.bal.ion = chrg.sol1)

  sol2.pre_txt.df <- specifications.fun(sol2, sol2.unit, chrg.bal.ion = chrg.sol2)

  #### Convert table to text ####
  tbl_to_text.fun <- function(df = sol1.pre_txt.df, sol.name = "SOLUTION X"){
    lst <- c()
    lst[1] <- sol.name
    for(i in 2:nrow(df)){
      x <- paste("\t",df[i,], collapse = "\t")
      lst[i] <- x
    }
    lst[i+1] <- "END"
    writeLines(lst)
    return(lst)
  }

  sol1.txt <- tbl_to_text.fun(sol1.pre_txt.df, sol.name = sol1.name)

  sol2.txt <- tbl_to_text.fun(sol2.pre_txt.df, sol.name = sol2.name)



  #### Mix Solutions ####
  tot_vol <- sol1_vol+sol2_vol

  s1.frac <- seq(0, tot_vol, interval)/tot_vol
  s2.frac <- rev(s1.frac)

  MIX <- as.list(seq_along(s2.frac))
  for(i in seq_along(s2.frac)){
    MIX[[i]] <- c(paste("MIX ", i, sep=""),
                  paste("\t",1, "\t",s1.frac[i], sep=""),
                  paste("\t",2, "\t",s2.frac[i], sep=""),
                  paste("SAVE solution ",i+2, sep=""),
                  "END",
                  "")
  }


  input1 <- c(sol1.txt, "", "SAVE solution 1", "", "END", "", sol2.txt, "", "SAVE solution 2", "", "END", "", unlist(MIX))
  writeLines(input1)


  #### Run simulations ####

  phrSetOutputStringsOn(TRUE)
  phrRunString(input1)
  output <- phrGetOutputStrings()
  #as.data.frame(output[69:234])
  #writeLines(output)

  #browser()


  #### Extract data ####


  #### Produce total element list ####
  total.list <- phrGetComponentList()


  #### Produce phase list ####
  phase.output.fun <- function(output){
    startLine <- which(grepl("Saturation indices", output))+4
    endLine <- which(grepl("For a gas, SI = log10", output))-2
    output.lst <- list()
    for(i in seq_along(startLine)){
      phase.output <- output[startLine[i]:endLine[i]]
      #writeLines(phase.output)
      phase.output.lst <- strsplit(phase.output, split = " ")

      phase.output.df.lst <- lapply(phase.output.lst, function(x) replace_with_na(data.frame(x), replace = list(x = "")) %>% filter(!is.na(x)) %>% t() %>% as.data.frame())

      ## Transform list to dataframe
      phase.output.df <- do.call(rbind, phase.output.df.lst)

      names(phase.output.df) <- c("Phase", "SI", "log_IAP", "log_K", "formula")
      row.names(phase.output.df) <- 1:nrow(phase.output.df)

      phase.output.df <- phase.output.df %>%
        mutate(sim = i)

      output.lst[[i]] <- phase.output.df
    }

    output.df <- do.call(rbind, output.lst) %>%
      mutate_at(.vars = vars("SI", "log_IAP", "log_K"), .funs = as.numeric) %>%
      as_tibble()
    return(output.df)
  }

  i=1

  #### Produce species list ####
  species.output.fun <- function(output){

    startLine <- which(grepl("Distribution of species", output))+5
    endLine <- which(grepl("Saturation indices", output))-2
    tot.output.lst <- list()
    output.lst <- list()
    for(i in seq_along(startLine)){
      species.output <- output[startLine[i]:endLine[i]]
      species.output.lst <- strsplit(species.output, split = " ")
      # lapply(species.output.lst, function(x) replace_with_na(data.frame(x), replace = list(x = "")))
      # lapply(species.output.lst, function(x) replace_with_na(data.frame(x), replace = list(x = "")) %>% filter(!is.na(x)))
      all.species.output.df.lst <- lapply(species.output.lst, function(x) replace_with_na(data.frame(x), replace = list(x = "")) %>% filter(!is.na(x)) %>% t() %>% as.data.frame())

      table_len <- lapply(all.species.output.df.lst, function(x) ncol(x)) %>% unlist()

      tot.species.output.df.lst <- all.species.output.df.lst[table_len == 2]
      species.output.df.lst <- all.species.output.df.lst[table_len == 7]


      ## Transform list to dataframe
      tot.species.output.df <- do.call(rbind, tot.species.output.df.lst)
      names(tot.species.output.df) <- c("tot_species", "Molality")
      row.names(tot.species.output.df) <- 1:nrow(tot.species.output.df)
      tot.species.output.df <- tot.species.output.df %>%
        mutate(sim = i)


      species.output.df <- do.call(rbind, species.output.df.lst)
      names(species.output.df) <- c("Species", "Molality", "Activity", "Log_Molality", "Log_Activity", "Log_Gamma", "mole_V_cm3.mol")
      row.names(species.output.df) <- 1:nrow(species.output.df)
      species.output.df <- species.output.df %>%
        mutate(sim = i)

      tot.output.lst[[i]] <- tot.species.output.df
      output.lst[[i]] <- unique.data.frame(species.output.df)

    }

    tot.output.df <- do.call(rbind, tot.output.lst) %>%
      as_tibble() %>%
      mutate_at(.vars = vars("Molality"), .funs = as.numeric)

    output.df <- do.call(rbind, output.lst) %>%
      as_tibble() %>%
      mutate_at(.vars = vars("Molality", "Activity", "Log_Molality", "Log_Activity", "Log_Gamma", "mole_V_cm3.mol"), .funs = as.numeric) %>%
      group_by(sim) %>%
      mutate(pH = case_when(Species == "H+" ~ Log_Molality*-1)) %>% ## add pH
      fill(pH, .direction = "downup") %>%
      ungroup()

    species.output.lst <- list(tot.output.df, output.df)

    return(species.output.lst)
  }

  #### Extract species ####
  species.output.lst <- species.output.fun(output)
  tot.species.output.df <- species.output.lst[[1]]#total per redox element
  species.output.df <- species.output.lst[[2]]#totel per species

  #### Extract phase ####
  phase.output.df <- phase.output.fun(output)

  #### Add pH to total species and phase table ####
  pH.df <- species.output.df %>%
    select(sim, pH) %>%
    unique.data.frame()

  tot.species.output.df <- left_join(tot.species.output.df, pH.df, by = "sim")

  phase.output.df <- left_join(phase.output.df, pH.df, by = "sim")


  #### Provide solution information ####
  sim_info.fun <- function(df){
    df1 <- df %>%
      mutate(sim_name = case_when(sim == 1 ~ "sol_1",
                                  sim == 2 ~ "sol_2",
                                  TRUE ~ paste0("mix_", sim-2)))

    sim_name <- unique(df1$sim_name)
    sol1_frac.seq <- c(s1.frac[length(s1.frac)], s1.frac[1], s1.frac)
    sol2_frac.seq <- c(s2.frac[length(s2.frac)], s2.frac[1], s2.frac)
    sol1_vol.seq <- sol1_vol*sol1_frac.seq
    sol2_vol.seq <- sol2_vol*sol2_frac.seq

    sim.tbl <- tibble(sim_name, sol1_frac = sol1_frac.seq, sol2_frac = sol2_frac.seq, sol1_vol = sol1_vol.seq, sol2_vol = sol2_vol.seq)

    df2 <- left_join(df1, sim.tbl) %>% as_tibble()
    return(df2)
  }

  #### Add simulation information ####
  tot.species.output.df <- sim_info.fun(tot.species.output.df)
  species.output.df <- sim_info.fun(species.output.df)
  phase.output.df <- sim_info.fun(phase.output.df)


  #### Build element list ####
  tot.species.output.df <- tot.species.output.df %>%
    mutate(element = gsub("[[:digit:]]","", tot_species)) %>%
    mutate(element = gsub("[[:punct:]]","", element))



  #### Find elements in species ####
  #
  # ## Remove double letters elements from species
  species <- unique(species.output.df$Species)
  tmp.species <- species
  # for(i in seq_along(species)){
  #   tmp.species <- gsub(tmp.species[i], "", tmp.species)
  # }

  ## Build tmp species table
  species.df <- tibble(Species = species, tmp.species = tmp.species)

  elmt.char.df <- as.data.frame(matrix(NA, nrow = nrow(species.df), ncol = length(total.list))) %>% as_tibble()
  names(elmt.char.df) <- total.list

  species.df <- bind_cols(species.df, elmt.char.df)


  ## Find elements in table
  species_element_lg.tbl <- species.df %>%
    gather(element, elm.lg, eval(total.list)) %>%
    rowwise() %>%
    mutate(elm.lg2 = grepl(element, Species) & nchar(element) == 2)  %>%
    mutate(elm.lg1 = grepl(element, tmp.species) & nchar(element) == 1) %>%
    mutate(elm.lg = elm.lg2 | elm.lg1) %>%
    ungroup() %>%
    unique.data.frame() %>%
    select(-elm.lg2, -elm.lg1, -tmp.species) %>%
    spread(element, elm.lg)

  ## Join together found elements with species table
  species.output.df <- left_join(species.output.df, species_element_lg.tbl, by = "Species")


  #### Find elements in phases ####

  ## Remove double letters elements from phase
  phases <- unique(phase.output.df$formula)
  tmp.phases <- phases
  # for(i in seq_along(elmt.2char)){
  #   tmp.phases <- gsub(elmt.2char[i], "", tmp.phases)
  # }

  ## Build tmp phases table
  phases.df <- tibble(formula = phases, tmp.formula = tmp.phases)

  elmt.char.df <- as.data.frame(matrix(NA, nrow = nrow(phases.df), ncol = length(total.list))) %>% as_tibble()
  names(elmt.char.df) <- total.list

  phases.df <- bind_cols(phases.df, elmt.char.df)


  ## Find elements in table
  phases_element_lg.tbl <- phases.df %>%
    gather(element, elm.lg, eval(total.list)) %>%
    rowwise() %>%
    mutate(elm.lg2 = grepl(element, formula) & nchar(element) == 2)  %>%
    mutate(elm.lg1 = grepl(element, tmp.formula) & nchar(element) == 1) %>%
    mutate(elm.lg = elm.lg2 | elm.lg1) %>%
    ungroup() %>%
    unique.data.frame() %>%
    select(-elm.lg2, -elm.lg1, -tmp.formula) %>%
    spread(element, elm.lg)

  ## Join together found elements with phases table
  phase.output.df <- left_join(phase.output.df, phases_element_lg.tbl, by = "formula")


  return(list(
    "elements" = total.list,
    "species" = species.output.df,
    "phases" = phase.output.df
  ))


}
