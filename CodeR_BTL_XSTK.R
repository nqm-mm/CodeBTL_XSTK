data <- read.csv("<file path>")

#===============================================================================
#=======================    DATA PREPROCESSING   ===============================
#===============================================================================

head(data, 10) #Đọc dữ liệu và xem 10 dòng đầu tiên của dữ liệu

apply(is.na(data), 2, which)#Kiểm tra trong tệp tin có dữ liệu khuyết hay không

#Kiểm tra định dạng của các biến
is.numeric(data$layer_height )
is.numeric(data$wall_thickness)
is.numeric(data$infill_density )
is.numeric(data$infill_pattern)
is.numeric(data$nozzle_temperature )
is.numeric(data$bed_temperature )
is.numeric(data$print_speed )
is.numeric(data$material)
is.numeric(data$fan_speed )
is.numeric(data$roughness )
is.numeric(data$tension_strenght )
is.numeric(data$elongation)

#===============================================================================
#=======================   DESCRIPTIVE STATISTICS   ============================
#===============================================================================

summary(data) #Tóm tắt thông tin mô tả của toàn bộ dữ liệu trong data

table(data$infill_pattern) #Tạo bảng tần suất — đếm số lượng mỗi giá trị khác nhau trong cột infill_pattern

table(data$material) #Tạo bảng tần suất — đếm số lượng mỗi giá trị khác nhau trong cột material


par(mfrow = c(2, 4)) #Đây là lệnh để chia khung vẽ (plotting area) thành 2 hàng, 4 cột 

plot(data$layer_height, data$tension_strength,
     xlab = "layer height", ylab = "tension strength", 
     main = "layer height vs tension strength")   # Dùng để vẽ biểu đồ phân tán 

plot(data$wall_thickness, data$tension_strength,
     xlab = "wall thickness", ylab = "tension Strength", 
     main = "Wall thickness vs tension Strength") # Dùng để vẽ biểu đồ phân tán 

plot(data$infill_density, data$tension_strength,
     xlab = "infill density", ylab = "tension strength", 
     main = "infill density vs tension strength") # Dùng để vẽ biểu đồ phân tán 

plot(mayin_data$nozzle_temperature, mayin_data$tension_strength,
     xlab = "nozzle temperature", ylab = "tension strength", 
     main = "nozzle temp vs tension strength") # Dùng để vẽ biểu đồ phân tán

plot(data$bed_temperature, data$tension_strength,
     xlab = "bed temperature", ylab = "tension strength", 
     main = "bed temp vs tension strength") # Dùng để vẽ biểu đồ phân tán

plot(data$print_speed, data$tension_strength,
     xlab = "print speed", ylab = "tension strength", 
     main = "print speed vs tension strength") # Dùng để vẽ biểu đồ phân tán

plot(data$fan_speed, data$tension_strength,
     xlab = "fan speed", ylab = "tension strength", 
     main = "fan speed vs tension strength") # Dùng để vẽ biểu đồ phân tán


hist(data$roughness,xlab = "roughness",
     main = "Histogram of roughness",col=heat.colors(5),labels = T,ylim=c(0,15)) # Dùng để vẽ biểu đồ tần suất (histogram) cho biến roughness (độ nhám bề mặt) trong dữ liệu data

boxplot(roughness~infill_pattern,data = data,col=c("red","blue"),main="roughness and material") # Dùng để vẽ biểu đồ hộp (boxplot) so sánh độ nhám bề mặt (roughness) giữa các loại vật liệu (material).

boxplot(roughness~material,data = data,col=c("pink","green"),main="roughness and infill_pattern") # Dùng để vẽ biểu đồ hộp (boxplot) so sánh độ nhám bề mặt (roughness) giữa các kiểu đổ vật liệu (infill pattern) trong in 3D


par(mfrow = c(2, 4)) #Đây là lệnh để chia khung vẽ (plotting area) thành 2 hàng, 4 cột 

plot(data$layer_height, data$roughness,
     xlab = "layer height", ylab = "roughness", 
     main = "layer height vs roughness")   # Dùng để vẽ biểu đồ phân tán 

plot(data$wall_thickness, data$roughness,
     xlab = "wall thickness", ylab = "roughnessh", 
     main = "Wall thickness vs roughness") # Dùng để vẽ biểu đồ phân tán 

plot(data$infill_density, data$roughness,
     xlab = "infill density", ylab = "roughness", 
     main = "infill density vs roughness") # Dùng để vẽ biểu đồ phân tán 

plot(data$nozzle_temperature, data$roughness,
     xlab = "nozzle temperature", ylab = "roughness", 
     main = "nozzle temp vs roughness") # Dùng để vẽ biểu đồ phân tán

plot(data$bed_temperature, data$roughness,
     xlab = "bed temperature", ylab = "roughness", 
     main = "bed temp vs roughness") # Dùng để vẽ biểu đồ phân tán

plot(data$print_speed, data$roughness,
     xlab = "print speed", ylab = "roughness", 
     main = "print speed vs roughness") # Dùng để vẽ biểu đồ phân tán

plot(data$fan_speed, data$roughness,
     xlab = "fan speed", ylab = "roughness", 
     main = "fan speed vs roughness") # Dùng để vẽ biểu đồ phân tán
#===============================================================================
#=======================   INFERENTIAL STATISTICS   ============================
#===============================================================================

#Tạo data không chứa tension_strength và elongation 
roughness_data <- data[,c("layer_height","wall_thickness","infill_density",
                          "infill_pattern","nozzle_temperature","bed_temperature",
                          "print_speed","material","fan_speed","roughness")]

#Loại bỏ các biến định tính khỏi roughness_data
roughness_data_DL <- roughness_data[,c("layer_height","wall_thickness",
                                       "infill_density","nozzle_temperature","bed_temperature",
                                       "print_speed","fan_speed","roughness")]

#Tạo ma trận tương quan (correlation matrix) dựa trên roughness_data_DL
correlation_matrix = cor(roughness_data_DL)
library(corrplot) #Cài đặt thư viện Corrplot 
corrplot(correlation_matrix,method = 'number') #Vẽ biểu đồ ma trận tương quan 

#Xây dựng mô hình hồi quy tuyến tính đa biến
model_roughness <- lm(roughness~layer_height + wall_thickness +
                        infill_density + infill_pattern + nozzle_temperature
                      + bed_temperature + print_speed + material,
                      data = roughness_data)
summary(model_roughness) #Hiển thị chi tiết kết quả tính toán mô hình

#Xây dựng lại mô hình hồi quy tuyến tính đa biến, loại bỏ các biến không có ý nghĩa
model2_roughness <- lm(roughness~layer_height + nozzle_temperature +
                         bed_temperature + print_speed + material,
                       data = roughness_data)
summary(model2_roughness)

#Kiểm tra các điều kiện giả định của mô hình hồi quy tuyến tính đa biến
par(mfrow = c(2,2))
plot(model2_roughness)

#===============================================================================
#============================== ONE-WAY ANOVA   ================================
#===============================================================================

#Chuyển biến layer_height thành biến định tính
data$layer_height <- as.factor(data$layer_height)

anova_model <- aov(roughness~layer_height,data)
summary(anova_model)


#Kiểm tra các điều kiện giả định 2 của mô hình ANOVA

#Chia 5 nhóm layer_height thành các dataset riêng biệt 
data_anova1 <- subset(data, layer_height == "0.02")
data_anova2 <- subset(data, layer_height == "0.06")
data_anova3 <- subset(data, layer_height == "0.1")
data_anova4 <- subset(data, layer_height == "0.15")
data_anova5 <- subset(data, layer_height == "0.2")

#Kiểm tra phân phối chuẩn nhóm 1
qqnorm(data_anova1$roughness)
qqline(data_anova1$roughness)
shapiro.test(data_anova1$roughness)

#Kiểm tra phân phối chuẩn nhóm 2
qqnorm(data_anova2$roughness)
qqline(data_anova2$roughness)
shapiro.test(data_anova2$roughness)

#Kiểm tra phân phối chuẩn nhóm 3
qqnorm(data_anova3$roughness)
qqline(data_anova3$roughness)
shapiro.test(data_anova3$roughness)

#Kiểm tra phân phối chuẩn nhóm 4
qqnorm(data_anova4$roughness)
qqline(data_anova4$roughness)
shapiro.test(data_anova4$roughness)

#Kiểm tra phân phối chuẩn nhóm 5
qqnorm(data_anova5$roughness)
qqline(data_anova5$roughness)
shapiro.test(data_anova5$roughness)


#Kiểm tra các điều kiện giả định 3 của mô hình ANOVA
library(car)
leveneTest(roughness ~ layer_height,data)

#===============================================================================
#==============================  TWO-WAY ANOVA  ================================
#===============================================================================

# Mô hình ANOVA 2 yếu tố
two_way_anova_model <- aov(roughness ~ material *layer_height ,
                           data = roughness_data)

# Xem kết quả
summary(two_way_anova_model)

#Vẽ biểu đồ tương tác trực quan
interaction.plot(roughness_data$layer_height,roughness_data$material,
                 data$roughness,
                 xlab = "Chiều cao lớp in (layer_height)",
                 ylab = "Độ nhám bề mặt (roughness)",
                 trace.label = "Vật liệu (material)",
                 main = "Biểu đồ tương tác giữa vật liệu và chiều cao lớp in")

