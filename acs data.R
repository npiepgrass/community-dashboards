source("./packages.R")
source("./acs helpers.R")




### Get all the data
## function: for any var, get all geos
varlist <- list(
  c("Enrollment by Race", paste0("B14007", LETTERS[1:9])),
  c("Health Insurance by Age by Race", paste0("C27001", LETTERS[1:9])),
  c("Income by Race", paste0("B19001", LETTERS[1:9])),
  c("Income by Source", paste0("B190", 51:60)),
  # c("Aggregate Income by Source", paste0("B190", c(61:67,69,70),"_001")),
  c("Commute Time","B08303"),
  c("Internet by Age by Computer","B28005"),
  c("Earnings by Occupation","B24011"),
  c("Occupation","C24010"),
  c("Income by Tenure","B25118"),
  c("Income by Presence of Children","B19131"),
  c("Income by Age","B19037"),
  c("Poverty by Age","17024"),
  c("Poverty by Education","B17003"),
  c("Education by Years","B15003"),
  c("Education by Race",paste0("C15002", LETTERS[1:9])),
  c("Home Value","B25075"),
  c("Rent by Income","B25070"),
  c("Enrolled by Graduated by Employment","B14005")
)
now()
z<-map(county_geoids,
    ~map2(.x, varlist, function(x,y){
           if(length(y)==2){
            acs_get_allgeos(y[1],y[2],x)
           }else if (length(y)>2) {
             map(y[2:length(y)],
                 ~acs_get_allgeos(y[1],.x,x)) %>% reduce(bind_rows)
           }
          }
        )
      )
now()
#6:04:09
z %>% 
  map(~reduce(.x,bind_rows)) %>% 
  reduce(bind_rows) %>% distinct(concept)



acs_make_neighbors <- function(x, type="count"){
  
}


disconnected_youth <- map(county_geoids,
                          ~acs_get_allgeos(
                            "16-19 Enroll/Employment", 
                            "B14005", 
                            .x)
                          ) %>% 
  reduce(bind_rows)

cost_burdened <- map(county_geoids,
                     ~acs_get_allgeos(
                       "Rent by Income", 
                       "B25070", 
                       .x)
                     ) %>% 
  reduce(bind_rows)


disconnected_youth


food_access <- read_excel("./FoodAccessResearchAtlasData2019.xlsx",sheet = 3)
nh_tract <- read_excel("./Neighborhood by Tract-Jefferson.xlsx")
nh_tract_cty <- read_excel("./Neighborhood by Tract-Outer.xlsx")


x<-cost_burdened %>% 
  left_join(nh_tract) %>% 
  group_by(GEOID) %>% mutate(total=sum(estimate,na.rm=T)) %>% ungroup() %>% 
  mutate(`label 1`=fct_collapse(`label 1`,
                                "Cost burdened"=c("30.0 to 34.9 percent",  
                                                  "35.0 to 39.9 percent",  
                                                  "40.0 to 49.9 percent", 
                                                  "50.0 percent or more"),
                                other_level = "Not cost burdened")) %>% 
  filter(`label 1`=="Cost burdened") %>% 
  group_by(across(c(-estimate, -moe))) %>%
  summarise(estimate=sum(estimate,na.rm=T)) %>% 
  ungroup() %>% 
  select(GEOID, NAME, NEIGHBOR, concept, `label 1`, estimate, total) %>% 
  filter(!is.na(NEIGHBOR)) %>% 
  group_by(NEIGHBOR, concept, `label 1`) %>% 
  summarise(`Cost Burdened`=sum(estimate,na.rm=T), total=sum(total,na.rm = T)) %>% 
  mutate(`Pct Cost Burdened`=paste0(round(`Cost Burdened`/total*100),"%")) %>% 
  ungroup() %>% 
  select(NEIGHBOR, `Cost Burdened`, `Pct Cost Burdened`) 
  
  
y<-disconnected_youth %>%
  left_join(nh_tract) %>% 
  filter(!is.na(NEIGHBOR)) %>% 
  group_by(GEOID) %>% 
  mutate(total=sum(estimate[is.na(`label 3`)],na.rm=T)) %>%
  ungroup() %>%
  filter(str_detect(`label 2`, "Not"),
         str_detect(`label 3`, "Not"),
         str_detect(`label 4`, "Not")) %>% 
  group_by(NEIGHBOR) %>% 
  summarise(`Disconnected Youth`=sum(estimate,na.rm=T), total=sum(total,na.rm = T)) %>%
  ungroup() %>% 
  mutate(`Pct Disconnected Youth`=paste0(round(`Disconnected Youth`/total*100),"%")) %>% 
  filter(total>200) %>% 
  select(NEIGHBOR, `Disconnected Youth`, `Pct Disconnected Youth`)


z<-food_access %>% 
  left_join(nh_tract, by = c("CensusTract"="GEOID")) %>% 
  filter(!is.na(NEIGHBOR)) %>% 
  select(NEIGHBOR, GEOID=CensusTract, Pop2010, lalowi1) %>% 
  group_by(NEIGHBOR) %>% 
  summarise(total=sum(Pop2010), 
            `Low-Access, Low-Income`= sum(as.numeric(lalowi1),na.rm = T)) %>% 
  mutate(`Pct Low-Access, Low-Income`=paste0(round(`Low-Access, Low-Income`/total*100),"%")) %>% 
  select(NEIGHBOR,`Low-Access, Low-Income`,`Pct Low-Access, Low-Income`)

reduce(list(x,y,z), full_join) %>% 
  write.csv(file="Neighborhood Data.csv")














