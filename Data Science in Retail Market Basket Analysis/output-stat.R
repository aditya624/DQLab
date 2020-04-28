library(arules)
transaksi <- read.transactions(file="https://academy.dqlab.id/dataset/data_transaksi.txt", format="single", sep="\t", cols=c(1,2), skip=1)

data_item <- itemFrequency(transaksi, type="absolute")

#Melakukan sorting pada data_item
data_item <- sort(data_item, decreasing = TRUE)

#Mengambil 3 item pertama
data_item <- data_item[1:3]

#Konversi data_item menjadi data frame dengan kolom Nama_Produk dan Jumlah
data_item <- data.frame("Nama Produk"=names(data_item), "Jumlah"=data_item, row.names=NULL)

#Menulis File Statistik Top 3
write.csv(data_item, file="top3_item_retail.txt", eol = "\r\n")

#Grafik
itemFrequencyPlot(transaksi)

#Menghasilkan association rules dan disimpan sebagai variable mba
mba <- apriori(transaksi)

#Melihat isi dari rules dengan menggunakan fungsi inspect
inspect(mba)

#Filter rhs dengan item "Sirup" dan tampilkan
inspect(subset(mba, rhs %in% "Sirup"))

#Filter lhs dengan item "Sirup" dan tampilkan
inspect(subset(mba, lhs %in% "Gula"))

#Filter LHS dan RHS
inspect(subset(mba, lhs %in% "Pet Food" & rhs %in% "Sirup"))

#Menghasilkan Rules dengan Parameter Support dan Confidence
mba <- apriori(transaksi,parameter = list(supp = 0.1, confidence = 0.5))

#Melihat isi dari rules dengan menggunakan fungsi inspect
inspect(mba)

#Berikut adalah contoh untuk filter dimana lhs atau rhs keduanya memiliki item Teh Celup.
inspect(subset(mba, lhs %in% "Teh Celup" | rhs %in% "Teh Celup"))

#Filter berdasarkan Lift
inspect(subset(mba, (lhs %in% "Teh Celup" | rhs %in% "Teh Celup") & lift>1))

#Rekomendasi - Filter dengan %ain%
inspect(subset(mba, (lhs %ain% c("Pet Food", "Gula" ))))

#Plot graph
library(arulesViz)
plot(subset(mba, lift>1.1), method="graph")
