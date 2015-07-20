#
# 在執行之前:
# 1.下載台灣區域界線，在 Taiwan_Map 目錄解壓縮
#	a. 縣(市)行政區域界線 http://data.gov.tw/node/7442
#	b. 鄉(鎮、市、區)行政區域界線 http://data.gov.tw/node/7441
#	c. 村里界圖(WGS84經緯度) http://data.gov.tw/node/7438
#	d. 將中文的檔名改掉
#		縣市界(經緯度)1031225_big5 改成 Level1_1031225_big5
#		鄉鎮界(經緯度)1031225_big5 改成 Level2_1031225_big5
#
# 2.下載選舉資料庫 http://data.gov.tw/node/13119
#	解壓縮後，將目錄改名為 Taiwan_voteData
#
# 3.執行此 script
#
# 4.執行 map$ggplot(1, COLOR) 來產生圖形
#   詳細的參數說明請參考 parse_mapdata.R
#

major_2014 <- list(name="2014縣市長", enable_level=c(T,T,T), dir=c("Taiwan_voteData/2014年地方公職人員選舉/直轄市市長", "Taiwan_voteData/2014年地方公職人員選舉/縣市市長"))
councillor_2014 <- list(name="2014縣市議員", enable_level=c(F,T,T), dir=c("Taiwan_voteData/2014年地方公職人員選舉/直轄市區域議員", "Taiwan_voteData/2014年地方公職人員選舉/縣市區域議員"))
village_2014 <- list(name="2014村里長", enable_level=c(F,F,T), dir=c("Taiwan_voteData/2014年地方公職人員選舉/直轄市村里長", "Taiwan_voteData/2014年地方公職人員選舉/縣市村里長"))
menu <- list(major_2014, councillor_2014, village_2014)

source('Taiwan_Map/parse_mapdata.R', encoding = 'UTF-8')
source('Taiwan_voteData/parse_votedata.R', encoding = 'UTF-8')

for (i in seq_len(length(menu))) {
	message(sprintf("%3d: %s", i, menu[[i]]$name))
}
select <- as.numeric(readline(prompt = "請輸入選擇: "))
if (!is.na(select) && select <= length(menu) && select >= 1) {
	message(sprintf("你選擇了 %s", menu[[select]]$name))
	COLOR <- region_to_color(menu[[select]])
# 	g <- map$ggplot(1, COLOR, "pdf")
# 	g <- map$ggplot(2, COLOR, "pdf")
# 	g <- map$ggplot(3, COLOR, "pdf")
# 	g <- map$ggplot(3, COLOR, "pdf", county_select = "新北市|臺北市")
# 	g <- map$ggplot(3, COLOR, "pdf", county_select = "臺南市")
}
