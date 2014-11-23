run_analysis <- function(){
        test_set_file_loc    <- "test/X_test.txt"
        train_set_file_loc   <- "train/X_train.txt"
        subject_test_loc     <- "test/subject_test.txt"
        subject_train_loc    <- "train/subject_train.txt"
        feature_file_loc     <- "features.txt"
        test_labels_loc      <- "test/y_test.txt"
        train_labels_loc     <- "train/y_train.txt"
        activity_label_loc   <- "activity_labels.txt"
        
        test_data             <- read.table(test_set_file_loc)
        train_data            <- read.table(train_set_file_loc)
        total_data            <- rbind(test_data, train_data)
        sub_test              <- read.table(subject_test_loc)
        sub_train             <- read.table(subject_train_loc)
        subject_all           <- rbind(sub_test, sub_train)
        names(subject_all)[1] <- "Subject"
        features              <- read.table(feature_file_loc)
        mean_sd_vec           <- c(grep("mean\\(\\)", features[,2]), grep("std\\(\\)", features[,2]))
        feature_mean_sd       <- features[mean_sd_vec,]
        feature_mean_sd       <- feature_mean_sd[order(feature_mean_sd$V1),]
        Extracted_data        <- total_data[,sort(mean_sd_vec)]
        test_labels           <- read.table(test_labels_loc)
        train_labels          <- read.table(train_labels_loc)
        labels                <- rbind(test_labels, train_labels)
        activity_labels       <- read.table(activity_label_loc)
        labels$id             <- 1:nrow(labels)
        act_label_map         <- merge(labels,activity_labels,by="V1")
        act_label_map         <- act_label_map[order(act_label_map$id),]
        act_label_map$id      <- NULL
        Labeled_data          <- cbind(Extracted_data, act_label_map[,2])
        
        colnames(Labeled_data)     <- feature_mean_sd[,2]
        colnames(Labeled_data)[67] <- "Activity"
        tidy_data                  <- cbind(Labeled_data, subject_all)
        ##write.table(tidy_data, file = "tidy_data.txt", append = FALSE, row.names = FALSE)
        tidy_data_2 <- aggregate(tidy_data, list(Activity = tidy_data$Activity, Subject = tidy_data$Subject), mean)
        for (i in 1:2){
                tidy_data_2[,ncol(tidy_data_2)] <- NULL
        }
        write.table(tidy_data_2, file = "tidy_data_2.txt", append = FALSE, row.names = FALSE)
		tidy_data_2
}
