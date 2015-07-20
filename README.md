
## 執行步驟

1. 下載台灣區域界線，在 Taiwan_Map 目錄解壓縮

	* 縣(市)行政區域界線 http://data.gov.tw/node/7442
	* 鄉(鎮、市、區)行政區域界線 http://data.gov.tw/node/7441
	* 村里界圖(WGS84經緯度) http://data.gov.tw/node/7438
	* 將中文的檔名改掉
		* 縣市界(經緯度)1031225_big5 改成 Level1_1031225_big5
		* 鄉鎮界(經緯度)1031225_big5 改成 Level2_1031225_big5

2. 下載選舉資料庫 http://data.gov.tw/node/13119

	解壓縮後，將目錄改名為 Taiwan_voteData，在GitHub上只保留2014年的資料作為範例

3. 安裝R (或加上R Studio)

	安裝教學可參考 http://www.dotblogs.com.tw/michael80321/archive/2014/12/15/147656.aspx

4. 修改`Taiwan_VoteMap.R`

	在第一行加上`setwd("path_to_your_source")`，例如`setwd("d:/Project/Map")`

5. 在R Studio中執行`Taiwan_VoteMap.R`

	* 看一下訊息，可以知道那些library需要安裝
	* 如果library有安裝完整，應該會看到下列訊息
	```
	1: 2014縣市長
	2: 2014縣市議員
	3: 2014村里長
	請輸入選擇:
	```
	* 這時輸入1讀取2014縣市長選舉資料

6. 執行`map$ggplot`來產生圖形

	* `g <- map$ggplot(1, COLOR)`
		產生縣市等級的地圖，著色資料來自COLOR
	* `g <- map$ggplot(2, COLOR, "pdf")`
		產生鄉鎮等級的地圖，輸出到pdf檔
	* `g <- map$ggplot(3, COLOR, "png", "Taiwan_Map")`
		產生村里等級的地圖，輸出到png檔，檔名為Taiwan_Map(可以忽略)
	* `g <- map$ggplot(3, COLOR, "pdf", county_select = "新北市|臺北市")`
		產生村里等級的地圖，範圍限定為新北市和臺北市 (注意: 是「臺」不是「台」)
	* `g <- map$ggplot(3, COLOR, "pdf", town_select = "板橋區")`
		產生村里等級的地圖，範圍限定為板橋區
	* 詳細的參數說明請參考`parse_mapdata.R`


## 架構說明

1. 程式架構分為地圖端和顏色端

	* `parse_mapdata.R`是地圖端，負責讀取地圖，並處理地區的名稱，但本身不包含顏色資料
	* `parse_votedata.R`是顏色端，只負責產生`COLOR`(`named character vector`)

2. 所有區域都有唯一的代碼，代碼可在`Taiwan_Map/region_name.xml`中找到

3. 正確產生的顏色資料，必須包含所有區域代碼及顏色的對應，例如:

	* `COLOR["63"] == "#6D6D00"` 這是台北市的顏色
	* `COLOR["6300100"] == "#757500"` 這是松山區的顏色
	* `COLOR["6300100-002"] == "#787800"` 這是松山區莊敬里的顏色
	* 在`map$ggplot`畫圖時，會找每一塊多邊形對應的代碼，再到`COLOR`中查出顏色
