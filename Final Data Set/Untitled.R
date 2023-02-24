#we're going to remove all the cars with engine sizes = to 0...
subset(Vehicle_2, engineSize == 0)

Vehicle_2 <- Vehicle[-c(32:35, 38, 40:44, 46:56, 62:66,
                        73:74, 76:78, 83:84, 109, 120:124, 4244:2445,
                        4247, 4287, 4291, 4293:4294, 4571, 12366:12367, 12377:12380, 12382,
                        12403, 12406:12407, 12452, 12459, 12472, 12486:12489, 12492:12493,
                        12502:12503, 12505, 12508, 12510, 12520, 12523, 12528:12531, 12537,
                        12546, 12549, 12555, 12559, 12561, 12572, 12579, 12589, 12592, 12600,
                        12601:12602, 4245, 12593, 12604, 12609, 12610, 12623, 12696:12698, 12720,
                        12722, 12725, 12753, 13659, 13727, 14758, 16178, 16193, 20263, 20290, 20297,
                        21876, 21880, 21883, 21885, 27088, 27177, 27184, 28350, 28408, 28841, 40304, 40305,
                        40307, 40336, 40340, 41361, 41396, 41425, 41501, 41869, 42033, 42758, 57667, 57869, 57887,
                        59148, 59305, 59580, 60117, 60118, 60126:60145, 60162:60169, 60176, 60238:60240, 60276:60285,
                        64304, 64314, 66895, 67002, 67026, 67729, 69128, 69285, 69287, 70373, 70859, 71784, 72135, 72420,
                        72607, 72616, 72718, 73256, 73373, 73683, 74009, 74127, 74252, 74297, 74353, 74577, 74787, 75321, 77381, 
                        75383, 75468, 75554, 75591, 75761, 75806, 75854, 75929, 75959, 76303, 76882, 76907, 77106, 77204, 77206, 77208, 
                        77215, 77217, 77242, 77441, 77462, 77766, 78431, 78488, 60237, 75381),]


summary(Vehicle_2)
#Open Caret Package to look for outliers in data using z-scores

preProcess_z <- preProcess(Vehicle_2[1:11],method = c("center","scale"))

Vehicle_z <- predict(preProcess_z, Vehicle_2[1:11])

?caret
View(Vehicle_z)
preProcess_mm <- preProcess(Vehicle_2[1:11],method = c("range"))
vehicle_mm <- predict(preProcess_mm,Vehicle_2[1:11])

attach(Vehicle)
plot(UKprice, mileage)

summary(Vehicle)
nrow(Vehicle)

Vehicle$Year <- as.factor(Vehicle$Year)

Vehicle_2 <- select(Vehicle, -4)
summary(Vehicle_2)
Vehicle_2$USDPrice <- as.numeric(Vehicle_2$USDPrice)
#Open Caret Package to look for outliers in data using z-scores

preProcess_z <- preProcess(Vehicle[1:10],method = c("center","scale"))

Vehicle_z <- predict(preProcess_z, Vehicle[1:10])

View(Vehicle_z)
preProcess_mm <- preProcess(Vehicle[1:10],method = c("range"))
vehicle_mm <- predict(preProcess_mm,Vehicle[1:10])
View(vehicle_mm)
attach(Vehicle)
plot(UKprice, mileage)

