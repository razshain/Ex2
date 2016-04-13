#יישום שיטות לניתוח נתונים – עבודה 2 חלק א
## מגישות רז שיין 201054103 , ליבנת גרשוני 301792792 ##
### קובץ נתונים מה-Web בשם : DfTRoadSafety_Accidents_2014.csv ###
```{r} 
install.packages("rmarkdown")
library(descr, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library( ROCR , warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(foreach)
library(RCurl)
library(mlbench)
library(pbkrtest)
library(caret)
library(rmarkdown)
setwd("C:\\Users\\pc\\Dropbox\\Raz&Livnat\\lastYear\\b\\ofrit")

x<- read.csv('Accidents_2014.csv',header = TRUE)
head(x)
``` 

הקובץ מכיל 32 עמודות וכ-1012 רשומות. הנתונים עוסקים בתאונות דרכים באנגליה לשנת 2014, הנתונים מבוססים על פציעות מתאונות שדווחו למשטרה ונשלפו מטופס תאונות STATS19 .
הנתונים כוללים: תאריך, שעה ,יום בשבוע,  מ"ז, מיקום, מספר כוחות המשטרה בזירה, חומרת התאונה, מספר הנפגעים, מס הכביש, סוג הכביש, הגבלת המהירות בכביש, מצב תאורה, מצב מזג אויר, מצב הדרך, האם קצין משטרה נכח במקום התאונה.

ניתוח הנתונים:


ניתן לראות שהתפלגות היום בשבוע לתאונה דיי אחידה, כאשר הקו הצהוב (4) מסמל את החציון של הנתונים:

```{r} 
hist(x$Day_of_Week,col="green")
abline(v=12,lwd=2,col="red")
abline(v=median(x$Day_of_Week),lwd=4,col="yellow")
``` 
![alt text](https://github.com/razshain/Ex2/blob/master/p1.png "p1")

לאחר מכן התמקדנו בכמות הרכבים לעומת כמות הנפגעים כיוון שהגיוני שככל שיש יותר רכבים בתאונה יש יותר נפגעים. ניתן לראות שכאשר יש מעט נפגעים מדובר במעט כלי רכב, אך לא בהכרח הפוך (כנראה שכאשר היו 93 נפגעים התאונה הייתה בין 2 אוטובוסים).
ציר ה-X הוא מס' הנפגעים וציר ה-Y מס' הרכבים.

```{r} 

boxplot(Number_of_Vehicles~Number_of_Casualties,data=x,col="red")

``` 
![alt text](https://github.com/razshain/Ex2/blob/master/p2.png "p2")

כמו כן רצינו לדעת היכן מתבצעות רוב התאונות, לשם כך בדקנו את העמודה Urban_or_Rural_Area כאשר 1- Urban ו- 2 – Rural. ניתן לראות כי הרוב המוחלט של התאונות היה באזור עירוני.

```{r} 
barplot(table(x$Urban_or_Rural_Area),col="pink",main="Number of Accidents per Area")
``` 

![alt text](https://github.com/razshain/Ex2/blob/master/p3.png "p3")

מצורפת תמונה של המפה עם מיקומי התאונות עליה, כמו כן ניתן לראות שתאונות הצבועות באדום הינן תאונות עם כמות נפגעים גדולה (לדוגמא 89) ותאונות הצבועות בירוק אלו תאונות עם כמות נפגעים קטנה (לדוגמא 1): 

```{r} 
require(ggmap)
map<-get_map(location='england', zoom=7, maptype = "terrain",
             source='google',color='color')
require("ggplot2")
require("RColorBrewer")
ggmap(map) + geom_point(
  aes(x=Longitude, y=Latitude, show_guide = TRUE, 
      colour=Number_of_Casualties), data=x, 
  alpha=.6, na.rm = T)  + 
  scale_color_gradient(low="green", high="red")
``` 

ביצענו על הנתונים בדיקת קורולציה בין קריטריונים שונים ומצאנו קורולציה גבוהה מאוד (מעל 0.996) בין העמודות הבאות: 
Location_Easting_OSGR – Longitude
Location_Northing_OSGR – Latitude
Police_Force - Local_Authority_.District.

על מנת לבצע קורולציה ב-R היה עלינו להוריד את העמודות בעלות ערכים לא נומריים (כמו תאריך,שעה, מס' כביש ועוד)
לאחר מכן בדקנו שונות 0 לכל העמודות, לא היו עמודות בעלות שונות 0 ולכן נציג את העמודות עם השונויות הנמוכות :
Pedestrian_Crossing.Human_Control : 0.003933137
Longitude: 0.011092645
Urban_or_Rural_Area : 0.01929792

```{r} 
x$Local_Authority_.Highway.<-NULL
x$LSOA_of_Accident_Location<-NULL
x$Accident_Index<-NULL
x$Date<-NULL
x$Time<-NULL
## build a correlation matrix based on the first 100000 rows ##
corr.matrix = cor(x, use = "pairwise.complete.obs")
corr.matrix[is.na(corr.matrix)] = 0
write.csv(corr.matrix,'corrMatrix.csv')

corr.list = foreach(i = 1:nrow(corr.matrix)) %do% {
  rownames(corr.matrix[corr.matrix[,i] > 0.996,])
}
## remove empty sets ##
corr.list = corr.list[sapply(corr.list, function(x) length(x) > 0 )]
## remove duplicated sets ##
corr.list = unique(corr.list)
corr.list

varX<-apply(x, 2, var)
write.csv(varX,'variance.csv')
``` 

לסיכום: ניתן לראות שרוב המידע זהה בסופו של דבר, מספר הנפגעים בדרך כלל נמוך עם מס רכבים קטן, רוב תאונות מתרחשות באזורים עירוניים. כמו כן לא בצענו צמצום ממדים אך ניתן לבצע זאת על עמודות בעלות קורלציה גבוהה.


