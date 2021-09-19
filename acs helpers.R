
#########################################################################
#########################################################################

acs_join_labels <- function(x, year = 2019, dataset="acs5", type=NULL) {
  if(exists("acs2019")&year==2019){
    labels <- acs2019
  }else{
    labels <- load_variables(cache = TRUE, year = year, dataset = dataset)
  } 
  x %<>% left_join(labels, by = c("variable"="name")) 
  if(any(str_detect(x$variable, "A_"))){
    x %<>% mutate(concept = str_remove_all(concept, "\\(|\\)| ALONE| HOU.*"))
  }else{
    x %<>% select(-concept)
  }
  x %<>% 
    mutate(label = str_remove(label, ".*?!!"))
  
  x
}

#########################################################################
#########################################################################

acs_sum_estimates <- function(x){
  if(count(distinct(x, concept))>1) stop("concept must be a single value")
  x %>% 
    group_by(across(-c(estimate, moe, variable))) %>% 
    summarize(estimate=sum(estimate, na.rm = T), moe=sum(moe, na.rm = T)) %>% 
    ungroup()
}

#########################################################################
#########################################################################

acs_fct_collapse <- function(x, type="race", summarize=F){
  if(type=="race"){
    x %<>% mutate(concept = as.factor(concept)) %>%
      mutate(race = fct_collapse(concept,
                                 white = c("White, Not Hispanic Or Latino","White"),
                                 Black = "Black Or African American",
                                 `American Indian` ='American Indian And Alaska Native',
                                 Asian = c("Asian",
                                           "Native Hawaiian And Other Pacific Islander"),
                                 Other = c("Some Other Race",
                                           "Two Or More Races"),
                                 `Latinx` = "Hispanic Or Latino"))
  }
  if(type=="race_bw"){
    x %<>% mutate(concept = as.factor(concept)) %>%
      mutate(race = fct_collapse(concept,
                                 white = c("White, Not Hispanic Or Latino","White"),
                                 Black = "Black Or African American",
                                 other_level = "other"))
  }
  if(type=="cb"){
    x %<>% 
      mutate(`label 1`=fct_collapse(`label 1`,
                                    "Cost burdened"=c("30.0 to 34.9 percent",  
                                                      "35.0 to 39.9 percent",  
                                                      "40.0 to 49.9 percent", 
                                                      "50.0 percent or more"),
                                    other_level = "Not cost burdened"))
  }
  # if(type=="age"){
  #   
  # }
  if(summarize==T){
    x %<>% acs_sum_estimates()
  }
  
  x
}

#########################################################################
#########################################################################

acs_bin_vals <- function(x, lab="label", type="inc_labs"){
  if( !any(str_detect(x[[lab]], "\\$")) ) stop("No $ value supplied")
  
  x %>% mutate(.,bins = case_when(
    str_detect(.[[lab]], "Less")~paste0("$0 to $", str_remove(.[[lab]], ".*\\$")),
    str_detect(.[[lab]], "more")~paste0(str_remove(.[[lab]], " .*")," to NA"),
    T~as.character(.[[lab]]))
  ) %>%
    mutate(bins = str_remove_all(bins, "\\$|,")) %>%
    separate(bins, c("lower", "upper"), sep=" to ", convert=T) #%>%
  # mutate(!!lab:=fct_relevel(as.factor(.[[lab]]), type))
  
}

#########################################################################
## to use in map fn, use map(~map2()) with list of concept-var pairs
#########################################################################

acs_get_allgeos <- function(concept, var, geo, collapse_sex=T, place=F){
  df <- map(list("county","tract"), 
            ~get_acs(
              geography = .x,
              variables = NULL,
              table = var,
              cache_table = T,
              year = 2019,
              state = as.numeric(str_sub(geo, 1,2)),
              county = as.numeric(str_sub(geo, 3,5))
            )
  )
  if(place==T){
    df$place <- get_acs(
      geography = "place",
      variables = NULL,
      table = var,
      cache_table = T,
      year = 2019,
      state = as.numeric(str_sub(geo, 1,2))
    )
  }
  df %<>% reduce(bind_rows)
  df %<>% 
    acs_join_labels() %>% 
    filter(!label == "Total:") %>% 
    mutate(
      concept = concept,
      label = str_remove(label, "Estimate:!!"),
      label = str_remove(label, "Total:!!"),
      label = fct_inorder(as.factor(label)),
      variable = str_remove(variable, "_.*")) %>% 
    separate(col=label, 
             into= c("label 1", "label 2", "label 3", 
                     "label 4", "label 5", "label 6"), 
             sep="!!") 
  if(str_detect(df$`label 1`, "Male")&collapse_sex==T){
    df %<>%
      filter(!(`label 1`=="Male:"&is.na(`label 2`)),
             !(`label 1`=="Female:"&is.na(`label 2`))) %>% 
      select(-`label 1`) %>%
      acs_sum_estimates()
  }
  
  df
  
}

#########################################################################
#########################################################################

acs_make_neighbors <- function(x, type="count"){
  #x is a dataframe with tracts or zips 
  # if(!exists(neighborhood_by_tract)) stop("dataframe named neighborhood_by_tract is missing")
  
  if(type == "count" & "GEOID" %in% names(x)){
    x %<>% 
      left_join(neighborhood_by_tract) %>% 
      mutate(NAME=str_remove(NAME, ".*?[0123456789], ")) %>% 
      group_by(across(-c(GEOID,estimate, moe))) %>% 
      summarise(estimate=sum(estimate, na.rm = T),
                moe=sum(moe, na.rm=T)) %>% 
      ungroup()
  }
  x
}

#########################################################################
#########################################################################

county_geoids <- list("21111", "21029", "21185", "21211", 
                      "18019", "18043", "18061")

inc_labs <-c("Less than $10,000",
             "$10,000 to $14,999",
             "$15,000 to $19,999",
             "$20,000 to $24,999",
             "$25,000 to $29,999",
             "$30,000 to $34,999",
             "$35,000 to $39,999",
             "$40,000 to $44,999",
             "$45,000 to $49,999",
             "$50,000 to $59,999",
             "$60,000 to $74,999",
             "$75,000 to $99,999",
             "$100,000 to $124,999",
             "$125,000 to $149,999",
             "$150,000 to $199,999",
             "$200,000 or more")
inc_labs_2 <-c("Less than $5,000",
               "$5,000 to $9,999",
               "$10,000 to $14,999",
               "$15,000 to $19,999",
               "$20,000 to $24,999",
               "$25,000 to $34,999",
               "$35,000 to $49,999",
               "$50,000 to $74,999",
               "$75,000 to $99,999",
               "$100,000 to $149,999",
               "$150,000 or more")
val_labs <- c("Less than $10,000",
              "$10,000 to $14,999",
              "$15,000 to $19,999",
              "$20,000 to $24,999",
              "$25,000 to $29,999",
              "$30,000 to $34,999",
              "$35,000 to $39,999",
              "$40,000 to $49,999",
              "$50,000 to $59,999",
              "$60,000 to $69,999",
              "$70,000 to $79,999",
              "$80,000 to $89,999",
              "$90,000 to $99,999",
              "$100,000 to $124,999",
              "$125,000 to $149,999",
              "$150,000 to $174,999",
              "$175,000 to $199,999",
              "$200,000 to $249,999",
              "$250,000 to $299,999",
              "$300,000 to $399,999",
              "$400,000 to $499,999",
              "$500,000 to $749,999",
              "$750,000 to $999,999",
              "$1,000,000 to $1,499,999",
              "$1,500,000 to $1,999,999",
              "$2,000,000 or more")







