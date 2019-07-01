---
title: "SQLandR"
output:
   html_document:
      self_contained: false
      keep_md: true
---




loading the SQLLite db file into R


```r
library(RSQLite)

filename <- "advert.db"
sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver,
                dbname = filename)
```

List the tables avalaible in the db


```r
dbListTables(db)
```

```
## [1] "campaigns"
```

passing the "campaings" db table in a R dataframe named **campaignsinfo** and listing the data


```r
campaingnsinfo<-dbReadTable(db, "campaigns")
knitr::kable(campaingnsinfo)
```



ID                                  PAYOUT  ACCOUNT_MANAGER   
---------------------------------  -------  ------------------
2b7203e1d1b1c3dd4615bdd0ffe60736     10.00  John Smith        
49f0bad299687c62334182178bfd75d8      5.30  Erica Archer      
adbf5a778175ee757c34d0eba4e932bc      3.10  Felix Butterworth 
1395ad342f7331f6ae81badb2eeb4717      8.10  Felix Butterworth 
db06e8e094e3b4b5943bbb71aabe9cdc      0.90  Erica Archer      
39060730d62be825ebb599711124309a      9.00  Felix Butterworth 
bff149a0b87f5b0e00d9dd364e9ddaa0      4.87  Felix Butterworth 
a8f5f167f44f4964e6c998dee827110c      0.45  John Smith        
396ac2a33748bb784077f4112335faa0      3.76  John Smith        
a4f0dc8026624beec687c6b74e5adc46      7.00  Erica Archer      
b5b037a78522671b89a2c1b21d9b80c6      7.60  Erica Archer      

df constists of 11 observations of 3 variables(`ID`,`PAYOUT`, `ACCOUNT_MANAGER`) we will change the name of the `ID` variable to `CAMPAiNGID` since ID is used also as a variable name in the conversions datasets


```r
library(dplyr)
campaingnsinfo<-campaingnsinfo%>%rename(CAMPAIGNID=ID)
knitr::kable(head(campaingnsinfo, n=11))
```



CAMPAIGNID                          PAYOUT  ACCOUNT_MANAGER   
---------------------------------  -------  ------------------
2b7203e1d1b1c3dd4615bdd0ffe60736     10.00  John Smith        
49f0bad299687c62334182178bfd75d8      5.30  Erica Archer      
adbf5a778175ee757c34d0eba4e932bc      3.10  Felix Butterworth 
1395ad342f7331f6ae81badb2eeb4717      8.10  Felix Butterworth 
db06e8e094e3b4b5943bbb71aabe9cdc      0.90  Erica Archer      
39060730d62be825ebb599711124309a      9.00  Felix Butterworth 
bff149a0b87f5b0e00d9dd364e9ddaa0      4.87  Felix Butterworth 
a8f5f167f44f4964e6c998dee827110c      0.45  John Smith        
396ac2a33748bb784077f4112335faa0      3.76  John Smith        
a4f0dc8026624beec687c6b74e5adc46      7.00  Erica Archer      
b5b037a78522671b89a2c1b21d9b80c6      7.60  Erica Archer      

Merging the indivudal csv files containing the conversion events from 01-03-2019 to 07-03-2019 to a dataframe naned **conversionevents**



```r
#change the wd to the folder where conversion files stored
setwd("/Users/christospsaropoulos/Documents/Git/SQLandR/conversions")
getwd()
```

```
## [1] "/Users/christospsaropoulos/Documents/Git/SQLandR/conversions"
```

```r
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
```

Removing duplicates from the **conversionevents** dataframe and saving the unique values in a new dataframe named **conversionevents_unique**. 
A total of 2389 duplicate values have been removed.


```r
conversionevents_unique<-distinct(conversionevents)
conversionevents_unique
```

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
```

Loading the rejected conversions in a R dataframe named **rejections**. As we can see we have 4779 rejected conversions


```r
rejections <- read.table("rejections.txt", header=FALSE, fill = TRUE)
rejections<-rejections%>% mutate_all(as.character)%>%rename(ID=V1)
head(rejections)
```

```
##                                         ID
## 1 76e40637d5f6e3f1d06df3c3341745cdfc9041de
## 2 2162a7f5ef30d438e50687502435c3d3ee4e2f34
## 3 944cf89cba90c4ea175bc346ab92c719d72c48c3
## 4 7c6367196670bd1f7a94d0448d6a160f82187de5
## 5 60e9d4db34d97d2e37bc4b233dbf715448294a82
## 6 821ae887ac6626b80eb8d369f956908baf2e4a15
```

```r
summary(rejections)
```

```
##       ID           
##  Length:4779       
##  Class :character  
##  Mode  :character
```


excluding `ID`listed in **rejections** from **conversionsevents_unique**


```r
conversionevents_unique<-conversionevents_unique %>% anti_join(rejections)
conversionevents_unique
```

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
```

Grouping conversions per `GAMPAINGID`


```r
conversionevents_unique %>% group_by(CAMPAIGNID) %>%summarise( campaignconversion = n())
```

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
```

Joining the **conversionevents_unique** dataframe with the **campaigninfo** dataframe in a dataframe named **master**


```r
master<-full_join(conversionevents_unique,campaingnsinfo,by="CAMPAIGNID")
master
```

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
```

calculating spend per campaign


```r
master2<-master%>% group_by(CAMPAIGNID,PAYOUT,ACCOUNT_MANAGER) %>%summarise( CAMPAIGN_CONVERSION = n())%>% mutate(TOTALSPEND=(PAYOUT*CAMPAIGN_CONVERSION))
knitr::kable(master2,n=11)
```



CAMPAIGNID                          PAYOUT  ACCOUNT_MANAGER      CAMPAIGN_CONVERSION   TOTALSPEND
---------------------------------  -------  ------------------  --------------------  -----------
1395ad342f7331f6ae81badb2eeb4717      8.10  Felix Butterworth                   1912     15487.20
2b7203e1d1b1c3dd4615bdd0ffe60736     10.00  John Smith                         17598    175980.00
39060730d62be825ebb599711124309a      9.00  Felix Butterworth                  16873    151857.00
396ac2a33748bb784077f4112335faa0      3.76  John Smith                         31109    116969.84
49f0bad299687c62334182178bfd75d8      5.30  Erica Archer                        3670     19451.00
a4f0dc8026624beec687c6b74e5adc46      7.00  Erica Archer                        3333     23331.00
a8f5f167f44f4964e6c998dee827110c      0.45  John Smith                          2172       977.40
adbf5a778175ee757c34d0eba4e932bc      3.10  Felix Butterworth                   5351     16588.10
b5b037a78522671b89a2c1b21d9b80c6      7.60  Erica Archer                       26866    204181.60
bff149a0b87f5b0e00d9dd364e9ddaa0      4.87  Felix Butterworth                   2498     12165.26
db06e8e094e3b4b5943bbb71aabe9cdc      0.90  Erica Archer                        3328      2995.20

Selecting the column needed for Task1 Report


```r
Task1<-master2%>%ungroup()%>%select(CAMPAIGNID,ACCOUNT_MANAGER,CAMPAIGN_CONVERSION,PAYOUT,TOTALSPEND)
knitr::kable(Task1, n=11)
```



CAMPAIGNID                         ACCOUNT_MANAGER      CAMPAIGN_CONVERSION   PAYOUT   TOTALSPEND
---------------------------------  ------------------  --------------------  -------  -----------
1395ad342f7331f6ae81badb2eeb4717   Felix Butterworth                   1912     8.10     15487.20
2b7203e1d1b1c3dd4615bdd0ffe60736   John Smith                         17598    10.00    175980.00
39060730d62be825ebb599711124309a   Felix Butterworth                  16873     9.00    151857.00
396ac2a33748bb784077f4112335faa0   John Smith                         31109     3.76    116969.84
49f0bad299687c62334182178bfd75d8   Erica Archer                        3670     5.30     19451.00
a4f0dc8026624beec687c6b74e5adc46   Erica Archer                        3333     7.00     23331.00
a8f5f167f44f4964e6c998dee827110c   John Smith                          2172     0.45       977.40
adbf5a778175ee757c34d0eba4e932bc   Felix Butterworth                   5351     3.10     16588.10
b5b037a78522671b89a2c1b21d9b80c6   Erica Archer                       26866     7.60    204181.60
bff149a0b87f5b0e00d9dd364e9ddaa0   Felix Butterworth                   2498     4.87     12165.26
db06e8e094e3b4b5943bbb71aabe9cdc   Erica Archer                        3328     0.90      2995.20

calculating daily spend per campaign


```r
master3<-master%>% group_by(CAMPAIGNID,Date,PAYOUT) %>%summarise( CAMPAIGN_CONVERSION = n())%>% mutate(TOTALSPEND=(PAYOUT*CAMPAIGN_CONVERSION))
master3
```

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
```
calculating daily percentage differences per campaign

```r
library(scales)
dailydiff<-master3%>%ungroup()%>%select(Date,CAMPAIGNID,TOTALSPEND)%>%group_by(CAMPAIGNID,Date)%>%summarise(TOTALSPEND = sum(TOTALSPEND))%>% mutate(DIFF=TOTALSPEND-lag(TOTALSPEND, n=1),PERCENT_DIFF = percent(DIFF/lag(TOTALSPEND,n=1)))
dailydiff
```

```
## # A tibble: 75 x 5
## # Groups:   CAMPAIGNID [11]
##    CAMPAIGNID                     Date       TOTALSPEND   DIFF PERCENT_DIFF
##    <chr>                          <date>          <dbl>  <dbl> <chr>       
##  1 1395ad342f7331f6ae81badb2eeb4… 2019-03-01      1709.    NA  NA%         
##  2 1395ad342f7331f6ae81badb2eeb4… 2019-03-02      3904.  2195. 130%        
##  3 1395ad342f7331f6ae81badb2eeb4… 2019-03-03      4601.   697. 20%         
##  4 1395ad342f7331f6ae81badb2eeb4… 2019-03-04      2341. -2260. -50%        
##  5 1395ad342f7331f6ae81badb2eeb4… 2019-03-05      1199. -1142. -50%        
##  6 1395ad342f7331f6ae81badb2eeb4… 2019-03-06       146. -1053  -90%        
##  7 1395ad342f7331f6ae81badb2eeb4… 2019-03-07      1588.  1442. 990%        
##  8 2b7203e1d1b1c3dd4615bdd0ffe60… 2019-03-01      9700     NA  NA%         
##  9 2b7203e1d1b1c3dd4615bdd0ffe60… 2019-03-02     11820   2120  22%         
## 10 2b7203e1d1b1c3dd4615bdd0ffe60… 2019-03-03     89980  78160  661%        
## # … with 65 more rows
```
Selecting the column needed for Task2 Report

```r
Task2<-dailydiff%>%ungroup()
Task2
```

```
## # A tibble: 75 x 5
##    CAMPAIGNID                     Date       TOTALSPEND   DIFF PERCENT_DIFF
##    <chr>                          <date>          <dbl>  <dbl> <chr>       
##  1 1395ad342f7331f6ae81badb2eeb4… 2019-03-01      1709.    NA  NA%         
##  2 1395ad342f7331f6ae81badb2eeb4… 2019-03-02      3904.  2195. 130%        
##  3 1395ad342f7331f6ae81badb2eeb4… 2019-03-03      4601.   697. 20%         
##  4 1395ad342f7331f6ae81badb2eeb4… 2019-03-04      2341. -2260. -50%        
##  5 1395ad342f7331f6ae81badb2eeb4… 2019-03-05      1199. -1142. -50%        
##  6 1395ad342f7331f6ae81badb2eeb4… 2019-03-06       146. -1053  -90%        
##  7 1395ad342f7331f6ae81badb2eeb4… 2019-03-07      1588.  1442. 990%        
##  8 2b7203e1d1b1c3dd4615bdd0ffe60… 2019-03-01      9700     NA  NA%         
##  9 2b7203e1d1b1c3dd4615bdd0ffe60… 2019-03-02     11820   2120  22%         
## 10 2b7203e1d1b1c3dd4615bdd0ffe60… 2019-03-03     89980  78160  661%        
## # … with 65 more rows
```

Exporting Task1 and Task2 Report in an xlsx file



```r
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




