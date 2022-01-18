---
title: "Lab 4 Homework"
author: "Ellyn Paris"
date: "2022-01-17"
output:
  html_document: 
    theme: spacelab
    keep_md: yes
---



## Instructions
Answer the following questions and complete the exercises in RMarkdown. Please embed all of your code and push your final work to your repository. Your final lab report should be organized, clean, and run free from errors. Remember, you must remove the `#` for the included code chunks to run. Be sure to add your name to the author header above.  

Make sure to use the formatting conventions of RMarkdown to make your report neat and clean!  

## Load the tidyverse

```r
library(tidyverse)
```

## Data
For the homework, we will use data about vertebrate home range sizes. The data are in the class folder, but the reference is below.  

**Database of vertebrate home range sizes.**  
Reference: Tamburello N, Cote IM, Dulvy NK (2015) Energy and the scaling of animal space use. The American Naturalist 186(2):196-211. http://dx.doi.org/10.1086/682070.  
Data: http://datadryad.org/resource/doi:10.5061/dryad.q5j65/1  

**1. Load the data into a new object called `homerange`.**

```r
getwd()
```

```
## [1] "/Users/ellyn/Desktop/BIS15W2022_eparis/lab4"
```

```r
homerange <- readr::read_csv("data/Tamburelloetal_HomeRangeDatabase.csv")
```

```
## Rows: 569 Columns: 24
```

```
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (16): taxon, common.name, class, order, family, genus, species, primarym...
## dbl  (8): mean.mass.g, log10.mass, mean.hra.m2, log10.hra, dimension, preyma...
```

```
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```


**2. Explore the data. Show the dimensions, column names, classes for each variable, and a statistical summary. Keep these as separate code chunks.**  

```r
dim(homerange)
```

```
## [1] 569  24
```

```r
names(homerange)
```

```
##  [1] "taxon"                      "common.name"               
##  [3] "class"                      "order"                     
##  [5] "family"                     "genus"                     
##  [7] "species"                    "primarymethod"             
##  [9] "N"                          "mean.mass.g"               
## [11] "log10.mass"                 "alternative.mass.reference"
## [13] "mean.hra.m2"                "log10.hra"                 
## [15] "hra.reference"              "realm"                     
## [17] "thermoregulation"           "locomotion"                
## [19] "trophic.guild"              "dimension"                 
## [21] "preymass"                   "log10.preymass"            
## [23] "PPMR"                       "prey.size.reference"
```

```r
str(homerange)
```

```
## spec_tbl_df [569 × 24] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
##  $ taxon                     : chr [1:569] "lake fishes" "river fishes" "river fishes" "river fishes" ...
##  $ common.name               : chr [1:569] "american eel" "blacktail redhorse" "central stoneroller" "rosyside dace" ...
##  $ class                     : chr [1:569] "actinopterygii" "actinopterygii" "actinopterygii" "actinopterygii" ...
##  $ order                     : chr [1:569] "anguilliformes" "cypriniformes" "cypriniformes" "cypriniformes" ...
##  $ family                    : chr [1:569] "anguillidae" "catostomidae" "cyprinidae" "cyprinidae" ...
##  $ genus                     : chr [1:569] "anguilla" "moxostoma" "campostoma" "clinostomus" ...
##  $ species                   : chr [1:569] "rostrata" "poecilura" "anomalum" "funduloides" ...
##  $ primarymethod             : chr [1:569] "telemetry" "mark-recapture" "mark-recapture" "mark-recapture" ...
##  $ N                         : chr [1:569] "16" NA "20" "26" ...
##  $ mean.mass.g               : num [1:569] 887 562 34 4 4 ...
##  $ log10.mass                : num [1:569] 2.948 2.75 1.531 0.602 0.602 ...
##  $ alternative.mass.reference: chr [1:569] NA NA NA NA ...
##  $ mean.hra.m2               : num [1:569] 282750 282.1 116.1 125.5 87.1 ...
##  $ log10.hra                 : num [1:569] 5.45 2.45 2.06 2.1 1.94 ...
##  $ hra.reference             : chr [1:569] "Minns, C. K. 1995. Allometry of home range size in lake and river fishes. Canadian Journal of Fisheries and Aquatic Sciences 52 "Minns, C. K. 1995. Allometry of home range size in lake and river fishes. Canadian Journal of Fisheries and Aquatic Sciences 52 "Minns, C. K. 1995. Allometry of home range size in lake and river fishes. Canadian Journal of Fisheries and Aquatic Sciences 52 "Minns, C. K. 1995. Allometry of home range size in lake and river fishes. Canadian Journal of Fisheries and Aquatic Sciences 52 ...
##  $ realm                     : chr [1:569] "aquatic" "aquatic" "aquatic" "aquatic" ...
##  $ thermoregulation          : chr [1:569] "ectotherm" "ectotherm" "ectotherm" "ectotherm" ...
##  $ locomotion                : chr [1:569] "swimming" "swimming" "swimming" "swimming" ...
##  $ trophic.guild             : chr [1:569] "carnivore" "carnivore" "carnivore" "carnivore" ...
##  $ dimension                 : num [1:569] 3 2 2 2 2 2 2 2 2 2 ...
##  $ preymass                  : num [1:569] NA NA NA NA NA NA 1.39 NA NA NA ...
##  $ log10.preymass            : num [1:569] NA NA NA NA NA ...
##  $ PPMR                      : num [1:569] NA NA NA NA NA NA 530 NA NA NA ...
##  $ prey.size.reference       : chr [1:569] NA NA NA NA ...
##  - attr(*, "spec")=
##   .. cols(
##   ..   taxon = col_character(),
##   ..   common.name = col_character(),
##   ..   class = col_character(),
##   ..   order = col_character(),
##   ..   family = col_character(),
##   ..   genus = col_character(),
##   ..   species = col_character(),
##   ..   primarymethod = col_character(),
##   ..   N = col_character(),
##   ..   mean.mass.g = col_double(),
##   ..   log10.mass = col_double(),
##   ..   alternative.mass.reference = col_character(),
##   ..   mean.hra.m2 = col_double(),
##   ..   log10.hra = col_double(),
##   ..   hra.reference = col_character(),
##   ..   realm = col_character(),
##   ..   thermoregulation = col_character(),
##   ..   locomotion = col_character(),
##   ..   trophic.guild = col_character(),
##   ..   dimension = col_double(),
##   ..   preymass = col_double(),
##   ..   log10.preymass = col_double(),
##   ..   PPMR = col_double(),
##   ..   prey.size.reference = col_character()
##   .. )
##  - attr(*, "problems")=<externalptr>
```

```r
summary(homerange) 
```

```
##     taxon           common.name           class              order          
##  Length:569         Length:569         Length:569         Length:569        
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##                                                                             
##                                                                             
##                                                                             
##                                                                             
##     family             genus             species          primarymethod     
##  Length:569         Length:569         Length:569         Length:569        
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##                                                                             
##                                                                             
##                                                                             
##                                                                             
##       N              mean.mass.g        log10.mass     
##  Length:569         Min.   :      0   Min.   :-0.6576  
##  Class :character   1st Qu.:     50   1st Qu.: 1.6990  
##  Mode  :character   Median :    330   Median : 2.5185  
##                     Mean   :  34602   Mean   : 2.5947  
##                     3rd Qu.:   2150   3rd Qu.: 3.3324  
##                     Max.   :4000000   Max.   : 6.6021  
##                                                        
##  alternative.mass.reference  mean.hra.m2          log10.hra     
##  Length:569                 Min.   :0.000e+00   Min.   :-1.523  
##  Class :character           1st Qu.:4.500e+03   1st Qu.: 3.653  
##  Mode  :character           Median :3.934e+04   Median : 4.595  
##                             Mean   :2.146e+07   Mean   : 4.709  
##                             3rd Qu.:1.038e+06   3rd Qu.: 6.016  
##                             Max.   :3.551e+09   Max.   : 9.550  
##                                                                 
##  hra.reference         realm           thermoregulation    locomotion       
##  Length:569         Length:569         Length:569         Length:569        
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##                                                                             
##                                                                             
##                                                                             
##                                                                             
##  trophic.guild        dimension        preymass         log10.preymass   
##  Length:569         Min.   :2.000   Min.   :     0.67   Min.   :-0.1739  
##  Class :character   1st Qu.:2.000   1st Qu.:    20.02   1st Qu.: 1.3014  
##  Mode  :character   Median :2.000   Median :    53.75   Median : 1.7304  
##                     Mean   :2.218   Mean   :  3989.88   Mean   : 2.0188  
##                     3rd Qu.:2.000   3rd Qu.:   363.35   3rd Qu.: 2.5603  
##                     Max.   :3.000   Max.   :130233.20   Max.   : 5.1147  
##                                     NA's   :502         NA's   :502      
##       PPMR         prey.size.reference
##  Min.   :  0.380   Length:569         
##  1st Qu.:  3.315   Class :character   
##  Median :  7.190   Mode  :character   
##  Mean   : 31.752                      
##  3rd Qu.: 15.966                      
##  Max.   :530.000                      
##  NA's   :502
```


**3. Change the class of the variables `taxon` and `order` to factors and display their levels.**  

```r
homerange$taxon <- as.factor(homerange$taxon)
homerange$order <- as.factor(homerange$order)
```


```r
levels(homerange$taxon)
```

```
## [1] "birds"         "lake fishes"   "lizards"       "mammals"      
## [5] "marine fishes" "river fishes"  "snakes"        "tortoises"    
## [9] "turtles"
```

```r
levels(homerange$order)
```

```
##  [1] "accipitriformes"    "afrosoricida"       "anguilliformes"    
##  [4] "anseriformes"       "apterygiformes"     "artiodactyla"      
##  [7] "caprimulgiformes"   "carnivora"          "charadriiformes"   
## [10] "columbidormes"      "columbiformes"      "coraciiformes"     
## [13] "cuculiformes"       "cypriniformes"      "dasyuromorpha"     
## [16] "dasyuromorpia"      "didelphimorphia"    "diprodontia"       
## [19] "diprotodontia"      "erinaceomorpha"     "esociformes"       
## [22] "falconiformes"      "gadiformes"         "galliformes"       
## [25] "gruiformes"         "lagomorpha"         "macroscelidea"     
## [28] "monotrematae"       "passeriformes"      "pelecaniformes"    
## [31] "peramelemorphia"    "perciformes"        "perissodactyla"    
## [34] "piciformes"         "pilosa"             "proboscidea"       
## [37] "psittaciformes"     "rheiformes"         "roden"             
## [40] "rodentia"           "salmoniformes"      "scorpaeniformes"   
## [43] "siluriformes"       "soricomorpha"       "squamata"          
## [46] "strigiformes"       "struthioniformes"   "syngnathiformes"   
## [49] "testudines"         "tetraodontiformes\xa0" "tinamiformes"
```

**4. What taxa are represented in the `homerange` data frame? Make a new data frame `taxa` that is restricted to taxon, common name, class, order, family, genus, species.**  

```r
taxa <- select(homerange,"taxon","common.name","class","order","family","genus","species")
taxa
```

```
## # A tibble: 569 × 7
##    taxon         common.name             class   order   family   genus  species
##    <fct>         <chr>                   <chr>   <fct>   <chr>    <chr>  <chr>  
##  1 lake fishes   american eel            actino… anguil… anguill… angui… rostra…
##  2 river fishes  blacktail redhorse      actino… cyprin… catosto… moxos… poecil…
##  3 river fishes  central stoneroller     actino… cyprin… cyprini… campo… anomal…
##  4 river fishes  rosyside dace           actino… cyprin… cyprini… clino… fundul…
##  5 river fishes  longnose dace           actino… cyprin… cyprini… rhini… catara…
##  6 river fishes  muskellunge             actino… esocif… esocidae esox   masqui…
##  7 marine fishes pollack                 actino… gadifo… gadidae  polla… pollac…
##  8 marine fishes saithe                  actino… gadifo… gadidae  polla… virens 
##  9 marine fishes lined surgeonfish       actino… percif… acanthu… acant… lineat…
## 10 marine fishes orangespine unicornfish actino… percif… acanthu… naso   litura…
## # … with 559 more rows
```


**5. The variable `taxon` identifies the large, common name groups of the species represented in `homerange`. Make a table the shows the counts for each of these `taxon`.**  

```r
table(taxa$taxon)
```

```
## 
##         birds   lake fishes       lizards       mammals marine fishes 
##           140             9            11           238            90 
##  river fishes        snakes     tortoises       turtles 
##            14            41            12            14
```

```r
names(homerange)
```

```
##  [1] "taxon"                      "common.name"               
##  [3] "class"                      "order"                     
##  [5] "family"                     "genus"                     
##  [7] "species"                    "primarymethod"             
##  [9] "N"                          "mean.mass.g"               
## [11] "log10.mass"                 "alternative.mass.reference"
## [13] "mean.hra.m2"                "log10.hra"                 
## [15] "hra.reference"              "realm"                     
## [17] "thermoregulation"           "locomotion"                
## [19] "trophic.guild"              "dimension"                 
## [21] "preymass"                   "log10.preymass"            
## [23] "PPMR"                       "prey.size.reference"
```


**6. The species in `homerange` are also classified into trophic guilds. How many species are represented in each trophic guild.**  

```r
homerange$trophic.guild <- as.factor(homerange$trophic.guild)
```


```r
is.factor(homerange$trophic.guild)
```

```
## [1] TRUE
```

```r
levels(homerange$trophic.guild)
```

```
## [1] "carnivore" "herbivore"
```

```r
table(homerange$trophic.guild)
```

```
## 
## carnivore herbivore 
##       342       227
```

```r
homerange %>% 
  count(trophic.guild)
```

```
## # A tibble: 2 × 2
##   trophic.guild     n
##   <fct>         <int>
## 1 carnivore       342
## 2 herbivore       227
```

```r
homerange %>% 
  group_by(trophic.guild,class) %>% 
  count()
```

```
## # A tibble: 8 × 3
## # Groups:   trophic.guild, class [8]
##   trophic.guild class              n
##   <fct>         <chr>          <int>
## 1 carnivore     actinopterygii    93
## 2 carnivore     aves             116
## 3 carnivore     mammalia          80
## 4 carnivore     reptilia          53
## 5 herbivore     actinopterygii    20
## 6 herbivore     aves              24
## 7 herbivore     mammalia         158
## 8 herbivore     reptilia          25
```


**7. Make two new data frames, one which is restricted to carnivores and another that is restricted to herbivores.**  

```r
carnivores <- filter(homerange,trophic.guild == "carnivore")
herbivores <- filter(homerange,trophic.guild == "herbivore")
carnivores
```

```
## # A tibble: 342 × 24
##    taxon   common.name   class   order  family genus species primarymethod N    
##    <fct>   <chr>         <chr>   <fct>  <chr>  <chr> <chr>   <chr>         <chr>
##  1 lake f… american eel  actino… angui… angui… angu… rostra… telemetry     16   
##  2 river … blacktail re… actino… cypri… catos… moxo… poecil… mark-recaptu… <NA> 
##  3 river … central ston… actino… cypri… cypri… camp… anomal… mark-recaptu… 20   
##  4 river … rosyside dace actino… cypri… cypri… clin… fundul… mark-recaptu… 26   
##  5 river … longnose dace actino… cypri… cypri… rhin… catara… mark-recaptu… 17   
##  6 river … muskellunge   actino… esoci… esoci… esox  masqui… telemetry     5    
##  7 marine… pollack       actino… gadif… gadid… poll… pollac… telemetry     2    
##  8 marine… saithe        actino… gadif… gadid… poll… virens  telemetry     2    
##  9 marine… giant treval… actino… perci… caran… cara… ignobi… telemetry     4    
## 10 lake f… rock bass     actino… perci… centr… ambl… rupest… mark-recaptu… 16   
## # … with 332 more rows, and 15 more variables: mean.mass.g <dbl>,
## #   log10.mass <dbl>, alternative.mass.reference <chr>, mean.hra.m2 <dbl>,
## #   log10.hra <dbl>, hra.reference <chr>, realm <chr>, thermoregulation <chr>,
## #   locomotion <chr>, trophic.guild <fct>, dimension <dbl>, preymass <dbl>,
## #   log10.preymass <dbl>, PPMR <dbl>, prey.size.reference <chr>
```

```r
herbivores
```

```
## # A tibble: 227 × 24
##    taxon  common.name   class   order  family  genus species primarymethod N    
##    <fct>  <chr>         <chr>   <fct>  <chr>   <chr> <chr>   <chr>         <chr>
##  1 marin… lined surgeo… actino… perci… acanth… acan… lineat… direct obser… <NA> 
##  2 marin… orangespine … actino… perci… acanth… naso  litura… telemetry     8    
##  3 marin… bluespine un… actino… perci… acanth… naso  unicor… telemetry     7    
##  4 marin… redlip blenny actino… perci… blenni… ophi… atlant… direct obser… 20   
##  5 marin… bermuda chub  actino… perci… kyphos… kyph… sectat… telemetry     11   
##  6 marin… cherubfish    actino… perci… pomaca… cent… argi    direct obser… <NA> 
##  7 marin… damselfish    actino… perci… pomace… chro… chromis direct obser… <NA> 
##  8 marin… twinspot dam… actino… perci… pomace… chry… biocel… direct obser… 18   
##  9 marin… wards damsel  actino… perci… pomace… poma… wardi   direct obser… <NA> 
## 10 marin… australian g… actino… perci… pomace… steg… apical… direct obser… <NA> 
## # … with 217 more rows, and 15 more variables: mean.mass.g <dbl>,
## #   log10.mass <dbl>, alternative.mass.reference <chr>, mean.hra.m2 <dbl>,
## #   log10.hra <dbl>, hra.reference <chr>, realm <chr>, thermoregulation <chr>,
## #   locomotion <chr>, trophic.guild <fct>, dimension <dbl>, preymass <dbl>,
## #   log10.preymass <dbl>, PPMR <dbl>, prey.size.reference <chr>
```

**8. Do herbivores or carnivores have, on average, a larger `mean.hra.m2`? Remove any NAs from the data.**  
## herbivores have a larger mean.hra.m2 on average

```r
 carnivore_hra <- carnivores[,13]
na.omit(carnivore_hra)
```

```
## # A tibble: 342 × 1
##    mean.hra.m2
##          <dbl>
##  1    282750  
##  2       282. 
##  3       116. 
##  4       126. 
##  5        87.1
##  6     39344. 
##  7      9056. 
##  8     44516. 
##  9     52773  
## 10     10407. 
## # … with 332 more rows
```

```r
colMeans(carnivore_hra)
```

```
## mean.hra.m2 
##    13039918
```

```r
herbivore_hra <- herbivores[,13]
na.omit(herbivore_hra)
```

```
## # A tibble: 227 × 1
##    mean.hra.m2
##          <dbl>
##  1       11.1 
##  2    32093.  
##  3    17900   
##  4        0.52
##  5    34423   
##  6        1.13
##  7       18.5 
##  8        2.58
##  9        0.54
## 10        2.25
## # … with 217 more rows
```

```r
colMeans(herbivore_hra)
```

```
## mean.hra.m2 
##    34137012
```


**9. Make a new dataframe `deer` that is limited to the mean mass, log10 mass, family, genus, and species of deer in the database. The family for deer is cervidae. Arrange the data in descending order by log10 mass. Which is the largest deer? What is its common name?**  
## the largest deer is the moose! scientific name: Alces alces


```r
deer <- select(homerange, "family", "genus", "species", "mean.mass.g", "log10.mass")
deer <- filter(deer, family == "cervidae")
deer
```

```
## # A tibble: 12 × 5
##    family   genus      species     mean.mass.g log10.mass
##    <chr>    <chr>      <chr>             <dbl>      <dbl>
##  1 cervidae alces      alces           307227.       5.49
##  2 cervidae axis       axis             62823.       4.80
##  3 cervidae capreolus  capreolus        24050.       4.38
##  4 cervidae cervus     elaphus         234758.       5.37
##  5 cervidae cervus     nippon           29450.       4.47
##  6 cervidae dama       dama             71450.       4.85
##  7 cervidae muntiacus  reevesi          13500.       4.13
##  8 cervidae odocoileus hemionus         53864.       4.73
##  9 cervidae odocoileus virginianus      87884.       4.94
## 10 cervidae ozotoceros bezoarticus      35000.       4.54
## 11 cervidae pudu       puda              7500.       3.88
## 12 cervidae rangifer   tarandus        102059.       5.01
```

```r
arrange(deer, desc(log10.mass))
```

```
## # A tibble: 12 × 5
##    family   genus      species     mean.mass.g log10.mass
##    <chr>    <chr>      <chr>             <dbl>      <dbl>
##  1 cervidae alces      alces           307227.       5.49
##  2 cervidae cervus     elaphus         234758.       5.37
##  3 cervidae rangifer   tarandus        102059.       5.01
##  4 cervidae odocoileus virginianus      87884.       4.94
##  5 cervidae dama       dama             71450.       4.85
##  6 cervidae axis       axis             62823.       4.80
##  7 cervidae odocoileus hemionus         53864.       4.73
##  8 cervidae ozotoceros bezoarticus      35000.       4.54
##  9 cervidae cervus     nippon           29450.       4.47
## 10 cervidae capreolus  capreolus        24050.       4.38
## 11 cervidae muntiacus  reevesi          13500.       4.13
## 12 cervidae pudu       puda              7500.       3.88
```

```r
deer.2 <- select(homerange, "family", "genus", "species","common.name", "mean.mass.g", "log10.mass")
deer.2 <- filter(deer.2, family == "cervidae")
arrange(deer.2, desc(log10.mass))
```

```
## # A tibble: 12 × 6
##    family   genus      species     common.name       mean.mass.g log10.mass
##    <chr>    <chr>      <chr>       <chr>                   <dbl>      <dbl>
##  1 cervidae alces      alces       moose                 307227.       5.49
##  2 cervidae cervus     elaphus     red deer              234758.       5.37
##  3 cervidae rangifer   tarandus    reindeer              102059.       5.01
##  4 cervidae odocoileus virginianus white-tailed deer      87884.       4.94
##  5 cervidae dama       dama        fallow deer            71450.       4.85
##  6 cervidae axis       axis        chital                 62823.       4.80
##  7 cervidae odocoileus hemionus    mule deer              53864.       4.73
##  8 cervidae ozotoceros bezoarticus pampas deer            35000.       4.54
##  9 cervidae cervus     nippon      sika deer              29450.       4.47
## 10 cervidae capreolus  capreolus   roe deer               24050.       4.38
## 11 cervidae muntiacus  reevesi     Reeves's muntjac       13500.       4.13
## 12 cervidae pudu       puda        pudu                    7500.       3.88
```

```r
deer.2
```

```
## # A tibble: 12 × 6
##    family   genus      species     common.name       mean.mass.g log10.mass
##    <chr>    <chr>      <chr>       <chr>                   <dbl>      <dbl>
##  1 cervidae alces      alces       moose                 307227.       5.49
##  2 cervidae axis       axis        chital                 62823.       4.80
##  3 cervidae capreolus  capreolus   roe deer               24050.       4.38
##  4 cervidae cervus     elaphus     red deer              234758.       5.37
##  5 cervidae cervus     nippon      sika deer              29450.       4.47
##  6 cervidae dama       dama        fallow deer            71450.       4.85
##  7 cervidae muntiacus  reevesi     Reeves's muntjac       13500.       4.13
##  8 cervidae odocoileus hemionus    mule deer              53864.       4.73
##  9 cervidae odocoileus virginianus white-tailed deer      87884.       4.94
## 10 cervidae ozotoceros bezoarticus pampas deer            35000.       4.54
## 11 cervidae pudu       puda        pudu                    7500.       3.88
## 12 cervidae rangifer   tarandus    reindeer              102059.       5.01
```


**10. As measured by the data, which snake species has the smallest homerange? Show all of your work, please. Look this species up online and tell me about it!** **Snake is found in taxon column**   
## the namaqua dwarf adder is the snake with the smallest range in this dataset. This small venomous snake resides in some parts of Africa and has evolved a high rate of reproduction due to its consdierable mortality rates (caused by the abundance of predators in its habitat of choice).

```r
snakes <- filter(homerange, taxon == "snakes")
snakes <- select(snakes, "common.name" , "genus", "mean.hra.m2")
arrange(snakes,mean.hra.m2)
```

```
## # A tibble: 41 × 3
##    common.name          genus       mean.hra.m2
##    <chr>                <chr>             <dbl>
##  1 namaqua dwarf adder  bitis              200 
##  2 eastern worm snake   carphopis          253 
##  3 butlers garter snake thamnophis         600 
##  4 western worm snake   carphopis          700 
##  5 snubnosed viper      vipera            2400 
##  6 chinese pit viper    gloydius          2614.
##  7 ringneck snake       diadophis         6476 
##  8 cottonmouth          agkistrodon      10655 
##  9 redbacked ratsnake   oocatochus       15400 
## 10 gopher snake         pituophis        17400 
## # … with 31 more rows
```


## Push your final code to GitHub!
Please be sure that you check the `keep md` file in the knit preferences.   
