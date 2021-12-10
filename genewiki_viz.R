library(dplyr)
library(ggplot2)
library(data.table)
library(openxlsx)
library(tibble)
#Gene_wiki_views
df_gwv <- read.table(file = "results/gene_wiki_views.tsv", 
                   sep = "\t", header=TRUE)
df_gwv <- df_gwv %>% select(-X)
View(df_gwv)
df_gwv$title <- as.factor(df_gwv$title)
#summary(as.factor(df_gwv$title))
length(unique(df_gwv$title)) #123
class(df_gwv$title)#factor

# Gene Wiki Views (M)
sum(df_gwv$views) #4950048

#selecting random subset of data for plotting purpose
selected_df_gwv <- df_gwv[1:456,]
ggplot(selected_df_gwv, aes(x=title, fill=title))+geom_bar()+theme_bw()

ggplot(selected_df_gwv, aes(x=title, y= views, col=title))+geom_boxplot()+theme_bw()+labs(title= "Title vs. Page Views", x= "Genes" , y="Page Views")+theme(plot.title = element_text(hjust = 0.5, face="bold"))

ggplot(selected_df_gwv, aes(x=views, fill=title))+geom_histogram(bins=100)

ggplot(selected_df_gwv, aes(x=views, col=title))+geom_freqpoly(bins=60)+theme_bw()

#GENE_WIKI_REVIEW
df_gwr <-as.data.frame(fread("data/GeneWikiReviewlist.tsv"))
View(df_gwr)
#Making a copy
df_gwr_modified <- df_gwr
df_gwr_modified$Batch_upd <- df_gwr_modified$Batch
df_gwr_modified <- df_gwr_modified[,c(1,18,2:17)]
View(df_gwr_modified)

length(factor(df_gwr_modified$GW_title))
length(unique(df_gwr_modified$GW_title))#129

#Status (A & X)
dim(df_gwr_modified %>% dplyr::distinct(status)) # 96*1
gwr_status_count <- (df_gwr_modified %>% dplyr::count(status))
dim(gwr_status_count) # Rows-96, Col-2 (96*2)
#unique status [94];"Submitted elsewhere"; empty 
write.xlsx(gwr_status_count, "./numbers/gwr_status_count.xlsx")

#Gene Wiki Pages (B)
dim(df_gwr_modified %>% dplyr::distinct(`Gene Wiki Page`)) # 124*1
gwr_genewikipages_count <- (df_gwr_modified %>% dplyr::count(`Gene Wiki Page`))
dim(gwr_genewikipages_count) # Rows-124, Col-2
# 123 unique; empty
write.xlsx(gwr_genewikipages_count, "./numbers/gwr_genewikipages_count.xlsx")

#changes to be made [Ginger]
#The first 6 existed before I joined the project, so you can consider those batch 0
df_gwr_modified$Batch_upd[1:6] <- 0

#Row 96 It's Dr. Peipei Ping, so it must be a cardiac gene-- batch 5
df_gwr_modified$Batch_upd[96] <- 5

#For row 107, it was officially part of Batch 9, but this guy is also in Peipei's lab so we can count it as batch 5
df_gwr_modified$Batch_upd[107] <- 5

#removing * and combining
df_gwr_modified$Batch_updated <- stringr::str_replace(df_gwr_modified$Batch_upd, '\\*', '')
df_gwr_modified <- df_gwr_modified[,c(1,2,19,3:18)]
df_gwr_modified$Batch_updated <- as.numeric(df_gwr_modified$Batch_updated)
#View(df_gwr_modified)
ggplot(df_gwr_modified, aes(x=Batch_updated, fill=Batch_updated))+geom_bar()

table(is.na(df_gwr_modified$Batch_updated)) #FALSE

unique(df_gwr_modified$Batch_updated)
unique(df_gwr_modified$Batch_upd)

#aggregate Genes from the Cardiac Gene Wiki Series (batches 4,5) vs disease-agnostic (batch 1-3,6,8,9), vs rare disease series (batch 7, 10) vs alzheimer disease (batch 11)
colnames(df_gwr_modified)[7] <- "title"
colnames(df_gwr_modified)

#aggregate Genes from the Cardiac Gene Wiki Series (batches 4,5) vs disease-agnostic (batch 1-3,6,8,9), vs rare disease series (batch 7, 10) vs alzheimer disease (batch 11)
# using Switch statement in R


df_gwr_modified <- df_gwr_modified %>% dplyr::mutate(case_category = case_when(Batch_updated < 1 ~ "earlier",
                                                                        Batch_updated == 1 ~ "disease-agnostic",
                                                                        Batch_updated == 2 ~ "disease-agnostic",
                                                                        Batch_updated == 3 ~ "disease-agnostic",
                                                                        Batch_updated == 8 ~ "disease-agnostic",
                                                                        Batch_updated == 9 ~ "disease-agnostic",
                                                                        Batch_updated == 4 ~ "cardiac",
                                                                        Batch_updated == 5 ~ "cardiac",
                                                                        Batch_updated == 7 ~ "rare",
                                                                        Batch_updated == 10 ~ "rare",
                                                                        Batch_updated == 11 ~ "AD",
                                                                        TRUE ~ "Low",))
df_gwr_modified <- df_gwr_modified[,c(1,2,3,20,4:19)]
View(df_gwr_modified)

length(intersect( df_gwv$title, df_gwr_modified$title)) #74
merge_gwv_gwr <- (left_join(df_gwv, df_gwr_modified, by= "title"))

unique(merge_gwv_gwr$Batch_updated)

library(dplyr)
temp3 <- merge_gwv_gwr[complete.cases(merge_gwv_gwr$case_category),]
#View(temp3)
ggplot(temp3, aes(x=case_category, y= views, col=case_category))+geom_boxplot()+theme_bw()+labs(title= "Title vs. Page Views", x= "Genes" , y="Page Views")+theme(plot.title = element_text(hjust = 0.5, face="bold"))

ggplot(temp3, aes(x=case_category, fill=case_category))+geom_bar()+theme_bw()




