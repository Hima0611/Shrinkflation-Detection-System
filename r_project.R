
# ==========================================================
# PROJECT: SHRINKFLATION DETECTION USING R
# ==========================================================

# 1. CREATE SAMPLE PRODUCT DATASET

products <- data.frame(
  Product = c("Biscuits", "Chips", "Chocolate",
              "Detergent", "Shampoo", "Coffee",
              "Cookies", "Juice"),
  
  Old_Quantity = c(200, 100, 100, 1000, 500, 250, 300, 1000),
  New_Quantity = c(180, 90, 100, 900, 450, 250, 250, 900),
  
  Old_Price = c(50, 20, 40, 120, 90, 150, 80, 60),
  New_Price = c(50, 20, 45, 120, 90, 160, 80, 60)
)

cat("===== SHRINKFLATION ANALYSIS =====\n")
print(products)

# 2. CALCULATE QUANTITY CHANGE

products$Quantity_Reduction_Percent <- round(
  ((products$Old_Quantity - products$New_Quantity) /
     products$Old_Quantity) * 100, 2
)

# 3. CALCULATE PRICE CHANGE

products$Price_Change_Percent <- round(
  ((products$New_Price - products$Old_Price) /
     products$Old_Price) * 100, 2
)

# 4. CALCULATE PRICE PER UNIT

products$Old_Unit_Price <- round(
  products$Old_Price / products$Old_Quantity, 4
)

products$New_Unit_Price <- round(
  products$New_Price / products$New_Quantity, 4
)

products$Unit_Price_Increase_Percent <- round(
  ((products$New_Unit_Price - products$Old_Unit_Price) /
     products$Old_Unit_Price) * 100, 2
)

# 5. DETECT POSSIBLE SHRINKFLATION

products$Status <- ifelse(
  products$New_Quantity < products$Old_Quantity,
  "Possible Shrinkflation",
  ifelse(
    products$New_Price > products$Old_Price,
    "Price Increase Only",
    ifelse(
      products$New_Price < products$Old_Price,
      "Price Decrease Only",
      "No Change Detected"
    )
  )
)

# 6. DISPLAY FINAL PRODUCT REPORT

cat("\n===== FINAL PRODUCT REPORT =====\n")

print(products[, c(
  "Product",
  "Old_Quantity",
  "New_Quantity",
  "Old_Price",
  "New_Price",
  "Quantity_Reduction_Percent",
  "Price_Change_Percent",
  "Unit_Price_Increase_Percent",
  "Status"
)])

# 7. DISPLAY SUMMARY STATISTICS

cat("\n===== SUMMARY STATISTICS =====\n")

cat("Total Products Analyzed:", nrow(products), "\n")

cat("Products with Possible Shrinkflation:",
    sum(products$Status == "Possible Shrinkflation"), "\n")

cat("Products with Price Increase Only:",
    sum(products$Status == "Price Increase Only"), "\n")

cat("Products with No Change Detected:",
    sum(products$Status == "No Change Detected"), "\n")

cat("Average Quantity Reduction:",
    round(mean(products$Quantity_Reduction_Percent), 2), "%\n")

# 8. FIND PRODUCT WITH HIGHEST UNIT-PRICE INCREASE

highest_index <- which.max(
  products$Unit_Price_Increase_Percent
)

cat("\n===== HIGHEST UNIT-PRICE INCREASE =====\n")

cat("Product:", products$Product[highest_index], "\n")

cat("Unit-Price Increase:",
    products$Unit_Price_Increase_Percent[highest_index], "%\n")

# 9. SAVE REPORT AS CSV

write.csv(
  products,
  "Shrinkflation_Analysis_Report.csv",
  row.names = FALSE
)

cat("\nCSV report saved successfully!\n")

# 10. CREATE GRAPHS

old_par <- par(no.readonly = TRUE)

par(
  mfrow = c(2, 2),
  mar = c(7, 4, 3, 1),
  cex.main = 0.9,
  cex.axis = 0.8
)


# Graph 1: Old vs New Quantity (Improved Spacing)

barplot(
  rbind(products$Old_Quantity, products$New_Quantity),
  beside = TRUE,
  names.arg = products$Product,
  col = c("skyblue", "orange"),
  main = "Old vs New Quantity",
  ylab = "Quantity",
  las = 2,
  cex.names = 0.7,
  width = 0.65,
  space = c(0.15, 1.2),
  border = "black"
)

legend(
  "topright",
  legend = c("Old Quantity", "New Quantity"),
  fill = c("skyblue", "orange"),
  cex = 0.65,
  bty = "n"
)



# Graph 2: Old vs New Price (Fixed Spacing)

barplot(
  rbind(products$Old_Price, products$New_Price),
  beside = TRUE,
  names.arg = products$Product,
  col = c("lightgreen", "tomato"),
  main = "Old vs New Price",
  ylab = "Price (Rs.)",
  ylim = c(0, 200),
  las = 2,
  cex.names = 0.7,
  width = 0.65,
  space = c(0.15, 1.2),
  border = "black"
)

legend(
  "topleft",
  legend = c("Old Price", "New Price"),
  fill = c("lightgreen", "tomato"),
  cex = 0.65,
  bty = "n"
)

# Graph 3: Quantity Reduction Percentage

barplot(
  products$Quantity_Reduction_Percent,
  names.arg = products$Product,
  col = "orange",
  main = "Quantity Reduction",
  ylab = "Reduction (%)",
  las = 2,
  cex.names = 0.7
)

# Graph 4: Unit-Price Increase Percentage

barplot(
  products$Unit_Price_Increase_Percent,
  names.arg = products$Product,
  col = "lightblue",
  main = "Unit-Price Increase",
  ylab = "Increase (%)",
  las = 2,
  cex.names = 0.7
)

par(old_par)

cat("\n===== PROJECT COMPLETED SUCCESSFULLY! =====\n")
