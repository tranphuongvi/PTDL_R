# Đường dẫn đến file CSV trên máy tính của bạn
duong_dan_file <- "C:/Users/Admin/Downloads/New_Google-Playstore_01-1.csv"

# Sử dụng hàm read.csv để đọc file CSV
du_lieu <- read.csv(duong_dan_file)

# Kiểm tra giá trị thiếu cho mỗi cột
sapply(du_lieu, function(x) sum(is.na(x)))

# Xem một số thông tin cơ bản về dữ liệu
print("Thông tin cơ bản về dữ liệu:")
print(summary(du_lieu))

# Kiểm tra và xử lý giá trị thiếu
print("Số lượng giá trị thiếu trong mỗi cột:")
print(colSums(is.na(du_lieu)))

# Loại bỏ các dòng chứa giá trị thiếu
du_lieu <- na.omit(du_lieu)
print(du_lieu)

# In trung bình và trung vị của một cột cụ thể
print("Trung bình cột 'Installs':")
print(mean(du_lieu$'Installs'))

# Ví dụ: Tính trung bình và độ lệch chuẩn của một cột
trung_binh <- mean(du_lieu$Installs)
do_lech_chuan <- sd(du_lieu$Installs)

# In kết quả
cat("Trung bình cột 'Installs':", trung_binh, "\n")
cat("Độ lệch chuẩn cột 'Installs':", do_lech_chuan, "\n")

print("Trung vị cột 'Rating':")
print(median(du_lieu$Rating))

# Tính phương sai
phuong_sai <- var(du_lieu$"Minimum.Installs")
print(phuong_sai)


# Vẽ biểu đồ 
# Đọc dữ liệu từ file CSV (thay thế "du_lieu.csv" bằng tên thực của file của bạn)
du_lieu <- read.csv("C:/Users/Admin/Downloads/New_Google-Playstore_01-1.csv")

# Vẽ biểu đồ hộp so sánh đánh giá theo thể loại
ggplot(du_lieu, aes(x = Category, y = Rating, fill = Category)) +
  geom_boxplot() +
  labs(title = "Biểu đồ Hộp So sánh Rating theo Category", x = "Thể loại", y = "Rating") +
  theme_minimal()
#top 10 thể loại được ưa chuộng
# Sắp xếp theo Rating giảm dần và lấy 10 hàng đầu
top_10_categories <- du_lieu %>%
  arrange(desc(Rating)) %>%
  head(10)

# In ra top 10 categories
print(top_10_categories)

#top 10 thể loại dcj ưa chuộng giá rẻ
# Sắp xếp bảng dữ liệu theo Rating và Price giảm dần
du_lieu_sapxep <- du_lieu %>% 
  arrange(desc(Rating), desc(Price))

# Chọn top 10 Category có Rating cao và Price cao
top_10_categories <- du_lieu_sapxep %>% 
  slice_head(n = 10)

# Hiển thị kết quả
print(top_10_categories[, c("Category", "Rating", "Price")])

#biến định tính và định tính
# Lấy tên và loại dữ liệu của từng cột
for (column_name in names(du_lieu)) {
  unique_values <- unique(du_lieu[[column_name]])
  
  if (is.factor(du_lieu[[column_name]])) {
    cat(paste("Biến định tính:", column_name, "\n"))
    cat("Giá trị:", levels(du_lieu[[column_name]]), "\n\n")
  } else {
    cat(paste("Biến định lượng:", column_name, "\n"))
    cat("Giá trị duy nhất:", unique_values, "\n\n")
  }
}

# Vẽ biểu đồ tần suất xuất hiện top 5 giá trị trong biến "Category"
# Tính tần suất của từng giá trị trong biến "Category"
tansuat_category <- table(du_lieu$Category)

# Chuyển thành DataFrame để sắp xếp
df_tansuat <- as.data.frame(tansuat_category)
colnames(df_tansuat) <- c("Category", "Frequency")

# Sắp xếp theo tần suất giảm dần và chọn top 5
top5_tansuat <- df_tansuat[order(df_tansuat$Frequency, decreasing = TRUE), ][1:5, ]

# Vẽ biểu đồ bar chart
ggplot(top5_tansuat, aes(x = reorder(Category, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 5 Category xuất hiện nhiều nhất", x = "Category", y = "Tần suất") +
  theme_minimal()



# Thống kê suy diễn
#Phân tích Tần suất xuất hiện của các Thể loại ứng dụng:

# Chọn top 5 ứng dụng
top5_apps <- head(du_lieu[order(du_lieu$Rating.Count, decreasing = TRUE), ], 5)

# Vẽ biểu đồ cột
library(ggplot2)

ggplot(top5_apps, aes(x = reorder(App.Name, -Rating.Count), y = Rating.Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Top 5 ứng dụng với số lượt đánh giá cao nhất",
       x = "Tên ứng dụng",
       y = "Số lượt đánh giá") +
  theme_minimal()



# Vẽ biểu đồ scatter plot và tính hệ số tương quan Pearson
ggplot(data = du_lieu, aes(x = Installs, y = Rating)) +
  geom_point() +
  labs(title = "Biểu đồ Scatter plot giữa Lượt cài đặt và Đánh giá", x = "Lượt cài đặt", y = "Đánh giá") +
  theme_minimal()

# Tính hệ số tương quan Pearson
cor_coefficient <- cor(du_lieu$Installs, du_lieu$Rating)
print(paste("Hệ số tương quan Pearson:", cor_coefficient))

# Xây dựng khoảng tin cậy cho lượt cài đặt
confidence_interval_installs <- t.test(du_lieu$Installs)$conf.int
print(paste("Khoảng tin cậy cho Lượt cài đặt:", confidence_interval_installs))

# Xây dựng khoảng tin cậy cho đánh giá
confidence_interval_rating <- t.test(du_lieu$Rating)$conf.int
print(paste("Khoảng tin cậy cho Đánh giá:", confidence_interval_rating))

# So sánh giá trị tối thiểu và tối đa giữa các thể loại ứng dụng
summary_stats <- summary(du_lieu[c("Maximum.Installs", "Minimum.Installs")])
print(summary_stats)

#Hypothesis

# Kiểm tra mẫu có phân phối chuẩn không (điều kiện tiên đề)
shapiro.test(du_lieu$Rating)

# Kiểm định t-test
t_test_result <- t.test(du_lieu$Rating[du_lieu$Category == "Adventure"], mu = 3)
print(t_test_result)

# In giả thuyết
if (t_test_result$p.value < 0.05) {
  print("Bác bỏ giả thuyết không: Trung bình Rating của phần tử Adventure khác 3.")
} else {
  print("Chấp nhận giả thuyết không: Trung bình Rating của phần tử Adventure bằng 3.")
}



