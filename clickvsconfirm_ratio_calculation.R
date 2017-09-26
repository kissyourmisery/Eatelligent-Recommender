res1 <- read.csv("/Users/Crystal/Desktop/Restaurant1_sessionid.csv", stringsAsFactors = FALSE)
library(sqldf)
all_items <- sqldf("SELECT * FROM res1 WHERE EVENT_Type = 'click_item' OR EVENT_Type = 'add_item'")
all_items <- sqldf("SELECT Event_Type, item_id, session_id FROM all_items")

for (i in 1:(nrow(all_items)-1)){
  if (all_items[i,]$item_id == all_items[(i+1),]$item_id && all_items[i,]$Event_Type == all_items[(i+1),]$Event_Type) {
    all_items[i,]$Error <- "duplicate"
  }
  else {
    all_items[i,]$Error <- "correct"
  }
}

all_items_correct <- sqldf("SELECT * FROM all_items WHERE Error='correct'")



#all_items_correct_noNA <- sqldf("SELECT * FROM all_items_correct WHERE session_id != 'NA'")
all_items_correct_noNA <- all_items_correct
all_clicks <- sqldf("SELECT * FROM all_items_correct_noNA WHERE Event_Type = 'click_item'")
all_confirms <- sqldf("SELECT * FROM all_items_correct_noNA WHERE Event_Type = 'add_item'")
all_clicks <- sqldf("SELECT COUNT(*), * FROM all_clicks GROUP BY item_id")
all_confirms <- sqldf("SELECT COUNT(*), * FROM all_confirms GROUP BY item_id")
altogether <- merge(all_clicks, all_confirms, by='item_id')
names(altogether)[2] <- "Number_of_clicks"
names(altogether)[6] <- "Number_of_confirms"
altogether <- altogether[,c(1,2,6,4)]
names(altogether)[4] <- "session_id"

altogether$ratio <- (altogether$Number_of_clicks)/(altogether$Number_of_confirms)
altogether <- altogether[order(altogether$ratio, decreasing = TRUE),]
altogether <- altogether[,c(1,2,3,5)]
View(altogether)

library(readxl)
dish <- read_excel("/Users/Crystal/Desktop/Dish_Items.xlsx")

names(dish)[1] <- "item_id"
combined_res1 <- merge(dish, altogether, by='item_id')
combined_res1 <- combined_res1[,c(1,2,12,13,14)]
combined_res1 <- combined_res1[order(combined_res1$ratio, decreasing=TRUE),]
View(combined_res1)