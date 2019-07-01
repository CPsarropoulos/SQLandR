SQLandR
================

###### author: “Christos Psarropoulos”

loading the SQLLite db file into R

``` r
library(RSQLite)

filename <- "advert.db"
sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver,
                dbname = filename)
```

List the tables avalaible in the db

``` r
dbListTables(db)
```

    ## [1] "campaigns"

passing the “campaings” db table in a R dataframe named
**campaignsinfo** and listing the data

``` r
campaingnsinfo<-dbReadTable(db, "campaigns")
knitr::kable(campaingnsinfo)
```

| ID                               | PAYOUT | ACCOUNT\_MANAGER  |
| :------------------------------- | -----: | :---------------- |
| 2b7203e1d1b1c3dd4615bdd0ffe60736 |  10.00 | John Smith        |
| 49f0bad299687c62334182178bfd75d8 |   5.30 | Erica Archer      |
| adbf5a778175ee757c34d0eba4e932bc |   3.10 | Felix Butterworth |
| 1395ad342f7331f6ae81badb2eeb4717 |   8.10 | Felix Butterworth |
| db06e8e094e3b4b5943bbb71aabe9cdc |   0.90 | Erica Archer      |
| 39060730d62be825ebb599711124309a |   9.00 | Felix Butterworth |
| bff149a0b87f5b0e00d9dd364e9ddaa0 |   4.87 | Felix Butterworth |
| a8f5f167f44f4964e6c998dee827110c |   0.45 | John Smith        |
| 396ac2a33748bb784077f4112335faa0 |   3.76 | John Smith        |
| a4f0dc8026624beec687c6b74e5adc46 |   7.00 | Erica Archer      |
| b5b037a78522671b89a2c1b21d9b80c6 |   7.60 | Erica Archer      |

df constists of 11 observations of 3 variables(`ID`,`PAYOUT`,
`ACCOUNT_MANAGER`) we will change the name of the `ID` variable to
`CAMPAiNGID` since ID is used also as a variable name in the conversions
datasets

``` r
library(dplyr)
campaingnsinfo<-campaingnsinfo%>%rename(CAMPAIGNID=ID)
knitr::kable(head(campaingnsinfo, n=11))
```

| CAMPAIGNID                       | PAYOUT | ACCOUNT\_MANAGER  |
| :------------------------------- | -----: | :---------------- |
| 2b7203e1d1b1c3dd4615bdd0ffe60736 |  10.00 | John Smith        |
| 49f0bad299687c62334182178bfd75d8 |   5.30 | Erica Archer      |
| adbf5a778175ee757c34d0eba4e932bc |   3.10 | Felix Butterworth |
| 1395ad342f7331f6ae81badb2eeb4717 |   8.10 | Felix Butterworth |
| db06e8e094e3b4b5943bbb71aabe9cdc |   0.90 | Erica Archer      |
| 39060730d62be825ebb599711124309a |   9.00 | Felix Butterworth |
| bff149a0b87f5b0e00d9dd364e9ddaa0 |   4.87 | Felix Butterworth |
| a8f5f167f44f4964e6c998dee827110c |   0.45 | John Smith        |
| 396ac2a33748bb784077f4112335faa0 |   3.76 | John Smith        |
| a4f0dc8026624beec687c6b74e5adc46 |   7.00 | Erica Archer      |
| b5b037a78522671b89a2c1b21d9b80c6 |   7.60 | Erica Archer      |

Merging the indivudal csv files containing the conversion events from
01-03-2019 to 07-03-2019 to a dataframe naned **conversionevents**

``` r
#change the wd to the folder where conversion files stored
setwd("/Users/christospsaropoulos/Documents/Git/SQLandR/conversions")
getwd()
```

    ## [1] "/Users/christospsaropoulos/Documents/Git/SQLandR/conversions"

``` r
library(stringr)
library(readr)
library(purrr)
library(dplyr)

#create a list with the names of the csvs that contain the conversion data
files <- list.files( pattern = "csv", full.names = TRUE)
#merge the individual csv adding a column that contains the file name 
conversionevents<-files %>% 
  map(.f = ~read_csv(.x)) %>%
  set_names(nm = files) %>%     
  map_dfr(identity, .id = "Name")
#replace the Name column with Date
conversionevents<-conversionevents%>%mutate(Name=str_extract(Name,"[0-9]{4}[-][0-9]{2}[-][0-9]{2}+"))%>%rename(Date=Name)%>%mutate(Date=as.Date(Date))
conversionevents
```

    ## # A tibble: 121,878 x 3
    ##    Date       ID                                CAMPAIGNID                 
    ##    <date>     <chr>                             <chr>                      
    ##  1 2019-03-01 5f6e57a887303a5f568459820e87904b… 39060730d62be825ebb5997111…
    ##  2 2019-03-01 c4c6e95e34c3bfe9d8b9628dfcc8a244… 39060730d62be825ebb5997111…
    ##  3 2019-03-01 b645996f754e084f5da14be7197d3c1d… b5b037a78522671b89a2c1b21d…
    ##  4 2019-03-01 e3527595bb5f0c3deef9a2a7a6f7b88e… 39060730d62be825ebb5997111…
    ##  5 2019-03-01 46b3d889fa4fc90dd56b64f323575110… b5b037a78522671b89a2c1b21d…
    ##  6 2019-03-01 6bda4fea92cf9310500684a702a7bb06… db06e8e094e3b4b5943bbb71aa…
    ##  7 2019-03-01 4bbf1c577a6ae45c5fb33ede9b18c8a7… 39060730d62be825ebb5997111…
    ##  8 2019-03-01 9a1519e497fe9aabe041891f47247dfd… 396ac2a33748bb784077f41123…
    ##  9 2019-03-01 8e15e2b783986d0054b58fa1e1242e06… 1395ad342f7331f6ae81badb2e…
    ## 10 2019-03-01 99f8175f367caeb7138fe348a4fa98de… 2b7203e1d1b1c3dd4615bdd0ff…
    ## # … with 121,868 more rows

Removing duplicates from the **conversionevents** dataframe and saving
the unique values in a new dataframe named **conversionevents\_unique**.
A total of 2389 duplicate values have been removed.

``` r
conversionevents_unique<-distinct(conversionevents)
conversionevents_unique
```

    ## # A tibble: 119,489 x 3
    ##    Date       ID                                CAMPAIGNID                 
    ##    <date>     <chr>                             <chr>                      
    ##  1 2019-03-01 5f6e57a887303a5f568459820e87904b… 39060730d62be825ebb5997111…
    ##  2 2019-03-01 c4c6e95e34c3bfe9d8b9628dfcc8a244… 39060730d62be825ebb5997111…
    ##  3 2019-03-01 b645996f754e084f5da14be7197d3c1d… b5b037a78522671b89a2c1b21d…
    ##  4 2019-03-01 e3527595bb5f0c3deef9a2a7a6f7b88e… 39060730d62be825ebb5997111…
    ##  5 2019-03-01 46b3d889fa4fc90dd56b64f323575110… b5b037a78522671b89a2c1b21d…
    ##  6 2019-03-01 6bda4fea92cf9310500684a702a7bb06… db06e8e094e3b4b5943bbb71aa…
    ##  7 2019-03-01 4bbf1c577a6ae45c5fb33ede9b18c8a7… 39060730d62be825ebb5997111…
    ##  8 2019-03-01 9a1519e497fe9aabe041891f47247dfd… 396ac2a33748bb784077f41123…
    ##  9 2019-03-01 8e15e2b783986d0054b58fa1e1242e06… 1395ad342f7331f6ae81badb2e…
    ## 10 2019-03-01 99f8175f367caeb7138fe348a4fa98de… 2b7203e1d1b1c3dd4615bdd0ff…
    ## # … with 119,479 more rows

Loading the rejected conversions in a R dataframe named **rejections**.
As we can see we have 4779 rejected conversions

``` r
rejections <- read.table("rejections.txt", header=FALSE, fill = TRUE)
rejections<-rejections%>% mutate_all(as.character)%>%rename(ID=V1)
knitr::kable(head(rejections))
```

| ID                                       |
| :--------------------------------------- |
| 76e40637d5f6e3f1d06df3c3341745cdfc9041de |
| 2162a7f5ef30d438e50687502435c3d3ee4e2f34 |
| 944cf89cba90c4ea175bc346ab92c719d72c48c3 |
| 7c6367196670bd1f7a94d0448d6a160f82187de5 |
| 60e9d4db34d97d2e37bc4b233dbf715448294a82 |
| 821ae887ac6626b80eb8d369f956908baf2e4a15 |

``` r
summary(rejections)
```

    ##       ID           
    ##  Length:4779       
    ##  Class :character  
    ##  Mode  :character

excluding `ID`listed in **rejections** from
**conversionsevents\_unique**

``` r
conversionevents_unique<-conversionevents_unique %>% anti_join(rejections)
conversionevents_unique
```

    ## # A tibble: 114,710 x 3
    ##    Date       ID                                CAMPAIGNID                 
    ##    <date>     <chr>                             <chr>                      
    ##  1 2019-03-01 5f6e57a887303a5f568459820e87904b… 39060730d62be825ebb5997111…
    ##  2 2019-03-01 c4c6e95e34c3bfe9d8b9628dfcc8a244… 39060730d62be825ebb5997111…
    ##  3 2019-03-01 b645996f754e084f5da14be7197d3c1d… b5b037a78522671b89a2c1b21d…
    ##  4 2019-03-01 e3527595bb5f0c3deef9a2a7a6f7b88e… 39060730d62be825ebb5997111…
    ##  5 2019-03-01 6bda4fea92cf9310500684a702a7bb06… db06e8e094e3b4b5943bbb71aa…
    ##  6 2019-03-01 4bbf1c577a6ae45c5fb33ede9b18c8a7… 39060730d62be825ebb5997111…
    ##  7 2019-03-01 9a1519e497fe9aabe041891f47247dfd… 396ac2a33748bb784077f41123…
    ##  8 2019-03-01 8e15e2b783986d0054b58fa1e1242e06… 1395ad342f7331f6ae81badb2e…
    ##  9 2019-03-01 99f8175f367caeb7138fe348a4fa98de… 2b7203e1d1b1c3dd4615bdd0ff…
    ## 10 2019-03-01 3a9b8971c8a7294c666485cf6230aab2… b5b037a78522671b89a2c1b21d…
    ## # … with 114,700 more rows

Grouping conversions per
`GAMPAINGID`

``` r
conversionevents_unique %>% group_by(CAMPAIGNID) %>%summarise( campaignconversion = n())
```

    ## # A tibble: 11 x 2
    ##    CAMPAIGNID                       campaignconversion
    ##    <chr>                                         <int>
    ##  1 1395ad342f7331f6ae81badb2eeb4717               1912
    ##  2 2b7203e1d1b1c3dd4615bdd0ffe60736              17598
    ##  3 39060730d62be825ebb599711124309a              16873
    ##  4 396ac2a33748bb784077f4112335faa0              31109
    ##  5 49f0bad299687c62334182178bfd75d8               3670
    ##  6 a4f0dc8026624beec687c6b74e5adc46               3333
    ##  7 a8f5f167f44f4964e6c998dee827110c               2172
    ##  8 adbf5a778175ee757c34d0eba4e932bc               5351
    ##  9 b5b037a78522671b89a2c1b21d9b80c6              26866
    ## 10 bff149a0b87f5b0e00d9dd364e9ddaa0               2498
    ## 11 db06e8e094e3b4b5943bbb71aabe9cdc               3328

Joining the **conversionevents\_unique** dataframe with the
**campaigninfo** dataframe in a dataframe named
**master**

``` r
master<-full_join(conversionevents_unique,campaingnsinfo,by="CAMPAIGNID")
master
```

    ## # A tibble: 114,710 x 5
    ##    Date       ID                  CAMPAIGNID         PAYOUT ACCOUNT_MANAGER
    ##    <date>     <chr>               <chr>               <dbl> <chr>          
    ##  1 2019-03-01 5f6e57a887303a5f56… 39060730d62be825e…   9    Felix Butterwo…
    ##  2 2019-03-01 c4c6e95e34c3bfe9d8… 39060730d62be825e…   9    Felix Butterwo…
    ##  3 2019-03-01 b645996f754e084f5d… b5b037a78522671b8…   7.6  Erica Archer   
    ##  4 2019-03-01 e3527595bb5f0c3dee… 39060730d62be825e…   9    Felix Butterwo…
    ##  5 2019-03-01 6bda4fea92cf931050… db06e8e094e3b4b59…   0.9  Erica Archer   
    ##  6 2019-03-01 4bbf1c577a6ae45c5f… 39060730d62be825e…   9    Felix Butterwo…
    ##  7 2019-03-01 9a1519e497fe9aabe0… 396ac2a33748bb784…   3.76 John Smith     
    ##  8 2019-03-01 8e15e2b783986d0054… 1395ad342f7331f6a…   8.1  Felix Butterwo…
    ##  9 2019-03-01 99f8175f367caeb713… 2b7203e1d1b1c3dd4…  10    John Smith     
    ## 10 2019-03-01 3a9b8971c8a7294c66… b5b037a78522671b8…   7.6  Erica Archer   
    ## # … with 114,700 more rows

calculating spend per
campaign

``` r
master2<-master%>% group_by(CAMPAIGNID,PAYOUT,ACCOUNT_MANAGER) %>%summarise( CAMPAIGN_CONVERSION = n())%>% mutate(TOTALSPEND=(PAYOUT*CAMPAIGN_CONVERSION))
knitr::kable(master2,n=11)
```

| CAMPAIGNID                       | PAYOUT | ACCOUNT\_MANAGER  | CAMPAIGN\_CONVERSION | TOTALSPEND |
| :------------------------------- | -----: | :---------------- | -------------------: | ---------: |
| 1395ad342f7331f6ae81badb2eeb4717 |   8.10 | Felix Butterworth |                 1912 |   15487.20 |
| 2b7203e1d1b1c3dd4615bdd0ffe60736 |  10.00 | John Smith        |                17598 |  175980.00 |
| 39060730d62be825ebb599711124309a |   9.00 | Felix Butterworth |                16873 |  151857.00 |
| 396ac2a33748bb784077f4112335faa0 |   3.76 | John Smith        |                31109 |  116969.84 |
| 49f0bad299687c62334182178bfd75d8 |   5.30 | Erica Archer      |                 3670 |   19451.00 |
| a4f0dc8026624beec687c6b74e5adc46 |   7.00 | Erica Archer      |                 3333 |   23331.00 |
| a8f5f167f44f4964e6c998dee827110c |   0.45 | John Smith        |                 2172 |     977.40 |
| adbf5a778175ee757c34d0eba4e932bc |   3.10 | Felix Butterworth |                 5351 |   16588.10 |
| b5b037a78522671b89a2c1b21d9b80c6 |   7.60 | Erica Archer      |                26866 |  204181.60 |
| bff149a0b87f5b0e00d9dd364e9ddaa0 |   4.87 | Felix Butterworth |                 2498 |   12165.26 |
| db06e8e094e3b4b5943bbb71aabe9cdc |   0.90 | Erica Archer      |                 3328 |    2995.20 |

Selecting the column needed for Task1
Report

``` r
Task1<-master2%>%ungroup()%>%select(CAMPAIGNID,ACCOUNT_MANAGER,CAMPAIGN_CONVERSION,PAYOUT,TOTALSPEND)
knitr::kable(Task1, n=11)
```

| CAMPAIGNID                       | ACCOUNT\_MANAGER  | CAMPAIGN\_CONVERSION | PAYOUT | TOTALSPEND |
| :------------------------------- | :---------------- | -------------------: | -----: | ---------: |
| 1395ad342f7331f6ae81badb2eeb4717 | Felix Butterworth |                 1912 |   8.10 |   15487.20 |
| 2b7203e1d1b1c3dd4615bdd0ffe60736 | John Smith        |                17598 |  10.00 |  175980.00 |
| 39060730d62be825ebb599711124309a | Felix Butterworth |                16873 |   9.00 |  151857.00 |
| 396ac2a33748bb784077f4112335faa0 | John Smith        |                31109 |   3.76 |  116969.84 |
| 49f0bad299687c62334182178bfd75d8 | Erica Archer      |                 3670 |   5.30 |   19451.00 |
| a4f0dc8026624beec687c6b74e5adc46 | Erica Archer      |                 3333 |   7.00 |   23331.00 |
| a8f5f167f44f4964e6c998dee827110c | John Smith        |                 2172 |   0.45 |     977.40 |
| adbf5a778175ee757c34d0eba4e932bc | Felix Butterworth |                 5351 |   3.10 |   16588.10 |
| b5b037a78522671b89a2c1b21d9b80c6 | Erica Archer      |                26866 |   7.60 |  204181.60 |
| bff149a0b87f5b0e00d9dd364e9ddaa0 | Felix Butterworth |                 2498 |   4.87 |   12165.26 |
| db06e8e094e3b4b5943bbb71aabe9cdc | Erica Archer      |                 3328 |   0.90 |    2995.20 |

calculating daily spend per
campaign

``` r
master3<-master%>% group_by(CAMPAIGNID,Date,PAYOUT) %>%summarise( CAMPAIGN_CONVERSION = n())%>% mutate(TOTALSPEND=(PAYOUT*CAMPAIGN_CONVERSION))
master3
```

    ## # A tibble: 75 x 5
    ## # Groups:   CAMPAIGNID, Date [75]
    ##    CAMPAIGNID                Date       PAYOUT CAMPAIGN_CONVERS… TOTALSPEND
    ##    <chr>                     <date>      <dbl>             <int>      <dbl>
    ##  1 1395ad342f7331f6ae81badb… 2019-03-01    8.1               211      1709.
    ##  2 1395ad342f7331f6ae81badb… 2019-03-02    8.1               482      3904.
    ##  3 1395ad342f7331f6ae81badb… 2019-03-03    8.1               568      4601.
    ##  4 1395ad342f7331f6ae81badb… 2019-03-04    8.1               289      2341.
    ##  5 1395ad342f7331f6ae81badb… 2019-03-05    8.1               148      1199.
    ##  6 1395ad342f7331f6ae81badb… 2019-03-06    8.1                18       146.
    ##  7 1395ad342f7331f6ae81badb… 2019-03-07    8.1               196      1588.
    ##  8 2b7203e1d1b1c3dd4615bdd0… 2019-03-01   10                 970      9700 
    ##  9 2b7203e1d1b1c3dd4615bdd0… 2019-03-02   10                1182     11820 
    ## 10 2b7203e1d1b1c3dd4615bdd0… 2019-03-03   10                8998     89980 
    ## # … with 65 more rows

calculating daily percentage differences per campaign

``` r
library(scales)
dailydiff<-master3%>%ungroup()%>%select(Date,CAMPAIGNID,TOTALSPEND)%>%group_by(CAMPAIGNID,Date)%>%summarise(TOTALSPEND = sum(TOTALSPEND))%>% mutate(DIFF=TOTALSPEND-lag(TOTALSPEND, n=1),PERCENT_DIFF = percent(DIFF/lag(TOTALSPEND,n=1)))
knitr::kable(dailydiff)
```

| CAMPAIGNID                          | Date       | TOTALSPEND |       DIFF | PERCENT\_DIFF |
| :---------------------------------- | :--------- | ---------: | ---------: | :------------ |
| 1395ad342f7331f6ae81badb2eeb4717    | 2019-03-01 |    1709.10 |         NA | NA%           |
| 1395ad342f7331f6ae81badb2eeb4717    | 2019-03-02 |    3904.20 |    2195.10 | 130%          |
| 1395ad342f7331f6ae81badb2eeb4717    | 2019-03-03 |    4600.80 |     696.60 | 20%           |
| 1395ad342f7331f6ae81badb2eeb4717    | 2019-03-04 |    2340.90 |  \-2259.90 | \-50%         |
| 1395ad342f7331f6ae81badb2eeb4717    | 2019-03-05 |    1198.80 |  \-1142.10 | \-50%         |
| 1395ad342f7331f6ae81badb2eeb4717    | 2019-03-06 |     145.80 |  \-1053.00 | \-90%         |
| 1395ad342f7331f6ae81badb2eeb4717    | 2019-03-07 |    1587.60 |    1441.80 | 990%          |
| 2b7203e1d1b1c3dd4615bdd0ffe60736    | 2019-03-01 |    9700.00 |         NA | NA%           |
| 2b7203e1d1b1c3dd4615bdd0ffe60736    | 2019-03-02 |   11820.00 |    2120.00 | 22%           |
| 2b7203e1d1b1c3dd4615bdd0ffe60736    | 2019-03-03 |   89980.00 |   78160.00 | 661%          |
| 2b7203e1d1b1c3dd4615bdd0ffe60736    | 2019-03-04 |   48120.00 | \-41860.00 | \-47%         |
| 2b7203e1d1b1c3dd4615bdd0ffe60736    | 2019-03-05 |    1960.00 | \-46160.00 | \-96%         |
| 2b7203e1d1b1c3dd4615bdd0ffe60736    | 2019-03-06 |    6220.00 |    4260.00 | 217%          |
| 2b7203e1d1b1c3dd4615bdd0ffe60736    | 2019-03-07 |    8180.00 |    1960.00 | 32%           |
| 39060730d62be825ebb599711124309a    | 2019-03-01 |   34029.00 |         NA | NA%           |
| 39060730d62be825ebb599711124309a    | 2019-03-02 |   33192.00 |   \-837.00 | \-2.5%        |
| 39060730d62be825ebb599711124309a    | 2019-03-03 |   32940.00 |   \-252.00 | \-0.8%        |
| 39060730d62be825ebb599711124309a    | 2019-03-04 |   34533.00 |    1593.00 | 4.8%          |
| 39060730d62be825ebb599711124309a    | 2019-03-05 |    9801.00 | \-24732.00 | \-71.6%       |
| 39060730d62be825ebb599711124309a    | 2019-03-06 |    4293.00 |  \-5508.00 | \-56.2%       |
| 39060730d62be825ebb599711124309a    | 2019-03-07 |    3069.00 |  \-1224.00 | \-28.5%       |
| 396ac2a33748bb784077f4112335faa0    | 2019-03-01 |   16931.28 |         NA | NA%           |
| 396ac2a33748bb784077f4112335faa0    | 2019-03-02 |   15720.56 |  \-1210.72 | \-7.2%        |
| 396ac2a33748bb784077f4112335faa0    | 2019-03-03 |   14784.32 |   \-936.24 | \-6.0%        |
| 396ac2a33748bb784077f4112335faa0    | 2019-03-04 |   14318.08 |   \-466.24 | \-3.2%        |
| 396ac2a33748bb784077f4112335faa0    | 2019-03-05 |   14498.56 |     180.48 | 1.3%          |
| 396ac2a33748bb784077f4112335faa0    | 2019-03-06 |   19142.16 |    4643.60 | 32.0%         |
| 396ac2a33748bb784077f4112335faa0    | 2019-03-07 |   21574.88 |    2432.72 | 12.7%         |
| 49f0bad299687c62334182178bfd75d8    | 2019-03-01 |    1770.20 |         NA | NA%           |
| 49f0bad299687c62334182178bfd75d8    | 2019-03-02 |    2453.90 |     683.70 | 38.6%         |
| 49f0bad299687c62334182178bfd75d8    | 2019-03-03 |    2528.10 |      74.20 | 3.0%          |
| 49f0bad299687c62334182178bfd75d8    | 2019-03-04 |    2300.20 |   \-227.90 | \-9.0%        |
| 49f0bad299687c62334182178bfd75d8    | 2019-03-05 |    4022.70 |    1722.50 | 74.9%         |
| 49f0bad299687c62334182178bfd75d8    | 2019-03-06 |    3047.50 |   \-975.20 | \-24.2%       |
| 49f0bad299687c62334182178bfd75d8    | 2019-03-07 |    3328.40 |     280.90 | 9.2%          |
| a4f0dc8026624beec687c6b74e5adc46    | 2019-03-01 |    3227.00 |         NA | NA%           |
| a4f0dc8026624beec687c6b74e5adc46    | 2019-03-02 |    3206.00 |    \-21.00 | \-0.7%        |
| a4f0dc8026624beec687c6b74e5adc46    | 2019-03-03 |    2758.00 |   \-448.00 | \-14.0%       |
| a4f0dc8026624beec687c6b74e5adc46    | 2019-03-04 |    3255.00 |     497.00 | 18.0%         |
| a4f0dc8026624beec687c6b74e5adc46    | 2019-03-05 |    2681.00 |   \-574.00 | \-17.6%       |
| a4f0dc8026624beec687c6b74e5adc46    | 2019-03-06 |    4207.00 |    1526.00 | 56.9%         |
| a4f0dc8026624beec687c6b74e5adc46    | 2019-03-07 |    3997.00 |   \-210.00 | \-5.0%        |
| a8f5f167f44f4964e6c998dee827110c    | 2019-03-01 |     297.90 |         NA | NA%           |
| a8f5f167f44f4964e6c998dee827110c    | 2019-03-02 |     163.35 |   \-134.55 | \-45%         |
| a8f5f167f44f4964e6c998dee827110c    | 2019-03-03 |      43.20 |   \-120.15 | \-74%         |
| a8f5f167f44f4964e6c998dee827110c    | 2019-03-06 |     216.90 |     173.70 | 402%          |
| a8f5f167f44f4964e6c998dee827110c    | 2019-03-07 |     256.05 |      39.15 | 18%           |
| adbf5a778175ee757c34d0eba4e932bc    | 2019-03-01 |    2923.30 |         NA | NA%           |
| adbf5a778175ee757c34d0eba4e932bc    | 2019-03-02 |    2845.80 |    \-77.50 | \-2.7%        |
| adbf5a778175ee757c34d0eba4e932bc    | 2019-03-03 |    2662.90 |   \-182.90 | \-6.4%        |
| adbf5a778175ee757c34d0eba4e932bc    | 2019-03-04 |    2675.30 |      12.40 | 0.5%          |
| adbf5a778175ee757c34d0eba4e932bc    | 2019-03-05 |    2501.70 |   \-173.60 | \-6.5%        |
| adbf5a778175ee757c34d0eba4e932bc    | 2019-03-06 |    2371.50 |   \-130.20 | \-5.2%        |
| adbf5a778175ee757c34d0eba4e932bc    | 2019-03-07 |     607.60 |  \-1763.90 | \-74.4%       |
| b5b037a78522671b89a2c1b21d9b80c6    | 2019-03-01 |   38699.20 |         NA | NA%           |
| b5b037a78522671b89a2c1b21d9b80c6    | 2019-03-02 |   35894.80 |  \-2804.40 | \-7.2%        |
| b5b037a78522671b89a2c1b21d9b80c6    | 2019-03-03 |   32847.20 |  \-3047.60 | \-8.5%        |
| b5b037a78522671b89a2c1b21d9b80c6    | 2019-03-04 |   29252.40 |  \-3594.80 | \-10.9%       |
| b5b037a78522671b89a2c1b21d9b80c6    | 2019-03-05 |   26881.20 |  \-2371.20 | \-8.1%        |
| b5b037a78522671b89a2c1b21d9b80c6    | 2019-03-06 |   25604.40 |  \-1276.80 | \-4.7%        |
| b5b037a78522671b89a2c1b21d9b80c6    | 2019-03-07 |   15002.40 | \-10602.00 | \-41.4%       |
| bff149a0b87f5b0e00d9dd364e9ddaa0    | 2019-03-01 |    1611.97 |         NA | NA%           |
| bff149a0b87f5b0e00d9dd364e9ddaa0    | 2019-03-02 |    1777.55 |     165.58 | 10%           |
| bff149a0b87f5b0e00d9dd364e9ddaa0    | 2019-03-03 |    1777.55 |       0.00 | 0%            |
| bff149a0b87f5b0e00d9dd364e9ddaa0    | 2019-03-04 |    1879.82 |     102.27 | 6%            |
| bff149a0b87f5b0e00d9dd364e9ddaa0    | 2019-03-05 |     633.10 |  \-1246.72 | \-66%         |
| bff149a0b87f5b0e00d9dd364e9ddaa0    | 2019-03-06 |    2352.21 |    1719.11 | 272%          |
| bff149a0b87f5b0e00d9dd364e9ddaa0    | 2019-03-07 |    2133.06 |   \-219.15 | \-9%          |
| db06e8e094e3b4b5943bbb71aabe9cdc    | 2019-03-01 |     561.60 |         NA | NA%           |
| db06e8e094e3b4b5943bbb71aabe9cdc    | 2019-03-02 |     871.20 |     309.60 | 55%           |
| db06e8e094e3b4b5943bbb71aabe9cdc    | 2019-03-03 |     838.80 |    \-32.40 | \-4%          |
| db06e8e094e3b4b5943bbb71aabe9cdc    | 2019-03-04 |     110.70 |   \-728.10 | \-87%         |
| db06e8e094e3b4b5943bbb71aabe9cdc    | 2019-03-05 |     171.90 |      61.20 | 55%           |
| db06e8e094e3b4b5943bbb71aabe9cdc    | 2019-03-06 |     268.20 |      96.30 | 56%           |
| db06e8e094e3b4b5943bbb71aabe9cdc    | 2019-03-07 |     172.80 |    \-95.40 | \-36%         |
| Selecting the column needed for Tas | k2 Report  |            |            |               |

``` r
Task2<-dailydiff%>%ungroup()
knitr::kable(Task2)
```

| CAMPAIGNID                       | Date       | TOTALSPEND |       DIFF | PERCENT\_DIFF |
| :------------------------------- | :--------- | ---------: | ---------: | :------------ |
| 1395ad342f7331f6ae81badb2eeb4717 | 2019-03-01 |    1709.10 |         NA | NA%           |
| 1395ad342f7331f6ae81badb2eeb4717 | 2019-03-02 |    3904.20 |    2195.10 | 130%          |
| 1395ad342f7331f6ae81badb2eeb4717 | 2019-03-03 |    4600.80 |     696.60 | 20%           |
| 1395ad342f7331f6ae81badb2eeb4717 | 2019-03-04 |    2340.90 |  \-2259.90 | \-50%         |
| 1395ad342f7331f6ae81badb2eeb4717 | 2019-03-05 |    1198.80 |  \-1142.10 | \-50%         |
| 1395ad342f7331f6ae81badb2eeb4717 | 2019-03-06 |     145.80 |  \-1053.00 | \-90%         |
| 1395ad342f7331f6ae81badb2eeb4717 | 2019-03-07 |    1587.60 |    1441.80 | 990%          |
| 2b7203e1d1b1c3dd4615bdd0ffe60736 | 2019-03-01 |    9700.00 |         NA | NA%           |
| 2b7203e1d1b1c3dd4615bdd0ffe60736 | 2019-03-02 |   11820.00 |    2120.00 | 22%           |
| 2b7203e1d1b1c3dd4615bdd0ffe60736 | 2019-03-03 |   89980.00 |   78160.00 | 661%          |
| 2b7203e1d1b1c3dd4615bdd0ffe60736 | 2019-03-04 |   48120.00 | \-41860.00 | \-47%         |
| 2b7203e1d1b1c3dd4615bdd0ffe60736 | 2019-03-05 |    1960.00 | \-46160.00 | \-96%         |
| 2b7203e1d1b1c3dd4615bdd0ffe60736 | 2019-03-06 |    6220.00 |    4260.00 | 217%          |
| 2b7203e1d1b1c3dd4615bdd0ffe60736 | 2019-03-07 |    8180.00 |    1960.00 | 32%           |
| 39060730d62be825ebb599711124309a | 2019-03-01 |   34029.00 |         NA | NA%           |
| 39060730d62be825ebb599711124309a | 2019-03-02 |   33192.00 |   \-837.00 | \-2.5%        |
| 39060730d62be825ebb599711124309a | 2019-03-03 |   32940.00 |   \-252.00 | \-0.8%        |
| 39060730d62be825ebb599711124309a | 2019-03-04 |   34533.00 |    1593.00 | 4.8%          |
| 39060730d62be825ebb599711124309a | 2019-03-05 |    9801.00 | \-24732.00 | \-71.6%       |
| 39060730d62be825ebb599711124309a | 2019-03-06 |    4293.00 |  \-5508.00 | \-56.2%       |
| 39060730d62be825ebb599711124309a | 2019-03-07 |    3069.00 |  \-1224.00 | \-28.5%       |
| 396ac2a33748bb784077f4112335faa0 | 2019-03-01 |   16931.28 |         NA | NA%           |
| 396ac2a33748bb784077f4112335faa0 | 2019-03-02 |   15720.56 |  \-1210.72 | \-7.2%        |
| 396ac2a33748bb784077f4112335faa0 | 2019-03-03 |   14784.32 |   \-936.24 | \-6.0%        |
| 396ac2a33748bb784077f4112335faa0 | 2019-03-04 |   14318.08 |   \-466.24 | \-3.2%        |
| 396ac2a33748bb784077f4112335faa0 | 2019-03-05 |   14498.56 |     180.48 | 1.3%          |
| 396ac2a33748bb784077f4112335faa0 | 2019-03-06 |   19142.16 |    4643.60 | 32.0%         |
| 396ac2a33748bb784077f4112335faa0 | 2019-03-07 |   21574.88 |    2432.72 | 12.7%         |
| 49f0bad299687c62334182178bfd75d8 | 2019-03-01 |    1770.20 |         NA | NA%           |
| 49f0bad299687c62334182178bfd75d8 | 2019-03-02 |    2453.90 |     683.70 | 38.6%         |
| 49f0bad299687c62334182178bfd75d8 | 2019-03-03 |    2528.10 |      74.20 | 3.0%          |
| 49f0bad299687c62334182178bfd75d8 | 2019-03-04 |    2300.20 |   \-227.90 | \-9.0%        |
| 49f0bad299687c62334182178bfd75d8 | 2019-03-05 |    4022.70 |    1722.50 | 74.9%         |
| 49f0bad299687c62334182178bfd75d8 | 2019-03-06 |    3047.50 |   \-975.20 | \-24.2%       |
| 49f0bad299687c62334182178bfd75d8 | 2019-03-07 |    3328.40 |     280.90 | 9.2%          |
| a4f0dc8026624beec687c6b74e5adc46 | 2019-03-01 |    3227.00 |         NA | NA%           |
| a4f0dc8026624beec687c6b74e5adc46 | 2019-03-02 |    3206.00 |    \-21.00 | \-0.7%        |
| a4f0dc8026624beec687c6b74e5adc46 | 2019-03-03 |    2758.00 |   \-448.00 | \-14.0%       |
| a4f0dc8026624beec687c6b74e5adc46 | 2019-03-04 |    3255.00 |     497.00 | 18.0%         |
| a4f0dc8026624beec687c6b74e5adc46 | 2019-03-05 |    2681.00 |   \-574.00 | \-17.6%       |
| a4f0dc8026624beec687c6b74e5adc46 | 2019-03-06 |    4207.00 |    1526.00 | 56.9%         |
| a4f0dc8026624beec687c6b74e5adc46 | 2019-03-07 |    3997.00 |   \-210.00 | \-5.0%        |
| a8f5f167f44f4964e6c998dee827110c | 2019-03-01 |     297.90 |         NA | NA%           |
| a8f5f167f44f4964e6c998dee827110c | 2019-03-02 |     163.35 |   \-134.55 | \-45%         |
| a8f5f167f44f4964e6c998dee827110c | 2019-03-03 |      43.20 |   \-120.15 | \-74%         |
| a8f5f167f44f4964e6c998dee827110c | 2019-03-06 |     216.90 |     173.70 | 402%          |
| a8f5f167f44f4964e6c998dee827110c | 2019-03-07 |     256.05 |      39.15 | 18%           |
| adbf5a778175ee757c34d0eba4e932bc | 2019-03-01 |    2923.30 |         NA | NA%           |
| adbf5a778175ee757c34d0eba4e932bc | 2019-03-02 |    2845.80 |    \-77.50 | \-2.7%        |
| adbf5a778175ee757c34d0eba4e932bc | 2019-03-03 |    2662.90 |   \-182.90 | \-6.4%        |
| adbf5a778175ee757c34d0eba4e932bc | 2019-03-04 |    2675.30 |      12.40 | 0.5%          |
| adbf5a778175ee757c34d0eba4e932bc | 2019-03-05 |    2501.70 |   \-173.60 | \-6.5%        |
| adbf5a778175ee757c34d0eba4e932bc | 2019-03-06 |    2371.50 |   \-130.20 | \-5.2%        |
| adbf5a778175ee757c34d0eba4e932bc | 2019-03-07 |     607.60 |  \-1763.90 | \-74.4%       |
| b5b037a78522671b89a2c1b21d9b80c6 | 2019-03-01 |   38699.20 |         NA | NA%           |
| b5b037a78522671b89a2c1b21d9b80c6 | 2019-03-02 |   35894.80 |  \-2804.40 | \-7.2%        |
| b5b037a78522671b89a2c1b21d9b80c6 | 2019-03-03 |   32847.20 |  \-3047.60 | \-8.5%        |
| b5b037a78522671b89a2c1b21d9b80c6 | 2019-03-04 |   29252.40 |  \-3594.80 | \-10.9%       |
| b5b037a78522671b89a2c1b21d9b80c6 | 2019-03-05 |   26881.20 |  \-2371.20 | \-8.1%        |
| b5b037a78522671b89a2c1b21d9b80c6 | 2019-03-06 |   25604.40 |  \-1276.80 | \-4.7%        |
| b5b037a78522671b89a2c1b21d9b80c6 | 2019-03-07 |   15002.40 | \-10602.00 | \-41.4%       |
| bff149a0b87f5b0e00d9dd364e9ddaa0 | 2019-03-01 |    1611.97 |         NA | NA%           |
| bff149a0b87f5b0e00d9dd364e9ddaa0 | 2019-03-02 |    1777.55 |     165.58 | 10%           |
| bff149a0b87f5b0e00d9dd364e9ddaa0 | 2019-03-03 |    1777.55 |       0.00 | 0%            |
| bff149a0b87f5b0e00d9dd364e9ddaa0 | 2019-03-04 |    1879.82 |     102.27 | 6%            |
| bff149a0b87f5b0e00d9dd364e9ddaa0 | 2019-03-05 |     633.10 |  \-1246.72 | \-66%         |
| bff149a0b87f5b0e00d9dd364e9ddaa0 | 2019-03-06 |    2352.21 |    1719.11 | 272%          |
| bff149a0b87f5b0e00d9dd364e9ddaa0 | 2019-03-07 |    2133.06 |   \-219.15 | \-9%          |
| db06e8e094e3b4b5943bbb71aabe9cdc | 2019-03-01 |     561.60 |         NA | NA%           |
| db06e8e094e3b4b5943bbb71aabe9cdc | 2019-03-02 |     871.20 |     309.60 | 55%           |
| db06e8e094e3b4b5943bbb71aabe9cdc | 2019-03-03 |     838.80 |    \-32.40 | \-4%          |
| db06e8e094e3b4b5943bbb71aabe9cdc | 2019-03-04 |     110.70 |   \-728.10 | \-87%         |
| db06e8e094e3b4b5943bbb71aabe9cdc | 2019-03-05 |     171.90 |      61.20 | 55%           |
| db06e8e094e3b4b5943bbb71aabe9cdc | 2019-03-06 |     268.20 |      96.30 | 56%           |
| db06e8e094e3b4b5943bbb71aabe9cdc | 2019-03-07 |     172.80 |    \-95.40 | \-36%         |

Exporting Task1 and Task2 Report in an xlsx file

``` r
library(xlsx)

# create new workbook
wb <- createWorkbook()

#--------------------
# DEFINE CELL STYLES 
#--------------------
# title and subtitle styles
title_style <- CellStyle(wb) + 
               Font(wb, heightInPoints = 16,
                    color = "blue", 
                    isBold = TRUE, 
                    underline = 1)

subtitle_style <- CellStyle(wb) + 
                  Font(wb, heightInPoints = 14,
                       isItalic = TRUE,
                       isBold = FALSE)

# data table styles
rowname_style <- CellStyle(wb) +
                 Font(wb, isBold = TRUE)

colname_style <- CellStyle(wb) +
                 Font(wb, isBold = TRUE) +
                 Alignment(wrapText = TRUE, horizontal = "ALIGN_CENTER") +
                 Border(color = "black",
                        position = c("TOP", "BOTTOM"),
                        pen = c("BORDER_THIN", "BORDER_THICK"))



#-------------------------
# CREATE & EDIT WORKSHEET 
#-------------------------
# create worksheets
sheet1 <- createSheet(wb, sheetName = "Task1")
sheet2 <- createSheet(wb, sheetName = "Task2")

# helper function to add titles
xlsx.addTitle <- function(sheet, rowIndex, title, titleStyle) {
        rows <- createRow(sheet, rowIndex = rowIndex)
        sheetTitle <- createCell(rows, colIndex = 1)
        setCellValue(sheetTitle[[1,1]], title)
        setCellStyle(sheetTitle[[1,1]], titleStyle)
}

# add title and sub title to worksheet
xlsx.addTitle(sheet = sheet1, rowIndex = 1, 
              title = "Advert",
              titleStyle = title_style)

xlsx.addTitle(sheet = sheet2, rowIndex = 1, 
              title = "Advert",
              titleStyle = title_style)

xlsx.addTitle(sheet =sheet1, rowIndex = 2, 
              title = "Total Spend per Campaign & Account Manager",
              titleStyle = subtitle_style)

xlsx.addTitle(sheet =sheet2, rowIndex = 2, 
              title = "Total Spend per Campaign & Account Manager Declining Campaigns",titleStyle = subtitle_style)

# add data frame to worksheet
addDataFrame(Task1, sheet = sheet1, startRow = 3, startColumn = 1,
             colnamesStyle = colname_style, 
             rownamesStyle = rowname_style)

addDataFrame(Task2, sheet = sheet2, startRow = 3, startColumn = 1,
             colnamesStyle = colname_style, 
             rownamesStyle = rowname_style)

# change row name column width
setColumnWidth(sheet = sheet1, colIndex = 1, colWidth = 3)
setColumnWidth(sheet = sheet1, colIndex = 2, colWidth = 38)
setColumnWidth(sheet = sheet1, colIndex = 3, colWidth = 21)
setColumnWidth(sheet = sheet1, colIndex = 4, colWidth = 15)

# change row name column width
setColumnWidth(sheet = sheet2, colIndex = 1, colWidth = 3)
setColumnWidth(sheet = sheet2, colIndex = 2, colWidth = 38)
setColumnWidth(sheet = sheet2, colIndex = 3, colWidth = 25)
setColumnWidth(sheet = sheet2, colIndex = 4, colWidth = 30)


# save workbook
saveWorkbook(wb, file = "AdvertReport.xlsx")

# view the file
#library(r2excel)
#in case you do not have r2excel package in  your library please run the following: 
#install.packages("devtools")
#devtools::install_github("kassambara/r2excel")
#xlsx.openFile("BlueBananaReport.xlsx")
```
