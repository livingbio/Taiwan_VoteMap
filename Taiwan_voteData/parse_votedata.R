require(csvread)
require(plyr)
require(PBSmapping)
require(stringr)

#
# 本程式的目的，是從每個選區中挑出一個政黨及該政黨的得票率
# 用政黨決定顏色，用得票率決定顏色的深淺
#
# 1.如何挑選政黨
#   a.選擇得票率最高的侯選人所在的政黨
#   b.選擇特定的政黨
#
# 2.如何決定顏色
#   目前是根據傳統的顏色印象，即國民黨=藍色、民進黨=綠色
#   但其他政黨並沒有特別標註，無黨籍暫定黃色、其他政黨都給灰色
#
# 3.傳回值
#   傳回值是一個name vector，可直接用區域代碼查詢顏色
#   例如COLOR["63"] 代表台北市的顏色、COLOR["6702900"] 代表關廟區的顏色
#   區域代碼請參考 region_name.xml
#

# 參數 select_func 用來挑選政黨
#   例如有得票率資料如下:
#         1    16    999 <-- 這裡代表政黨代碼
#   63  0.5   0.4    0.1 <-- 這裡代表某個區域各政黨得票率
#   64  0.3   0.7    0.0
#   
#   預設的 select_func 是 max.col 會找出最大值所在的欄
#   上面的例子中，會傳回 c(1, 2)，因為第一列最大值在第一欄，第二列在第二欄
#   所以會挑出最高得票率的侯選人所在的政黨
#	如果要挑出特定的政黨，可以用下列函式
DPP <- function(x) rep(which(colnames(x)==16), nrow(x))
KMT <- function(x) rep(which(colnames(x)==1), nrow(x))
NOPARTY <- function(x) rep(which(colnames(x)==999), nrow(x))

# 參數 color_func 是將政黨及得票率轉換為顏色
# 為了方便選擇同一顏色的深淺，使用 HSV color space
# 可先到 http://colorizer.org/ 先測試想要的顏色
party_to_hsv <- function (party, support)
{
	# 中國共產黨外圍組織: 勞動黨,新黨,中華統一促進黨
	CPC <- c(15, 74, 113)
	# 第三勢力: 綠黨,人民民主陣線,人民最大黨,中華民主向日葵憲政改革聯盟,樹黨
	THIRD <- c(79, 199, 153, 254, 259)

	p <- hsv(1, 0, 1-support)	# 其他黨派是灰色
	p[party == 1] <- hsv(0.6, 1, 1-support[party == 1])	    # 國民黨: 藍色
	p[party == 16] <- hsv(0.25, 1, 1-support[party == 16])	# 民進黨: 是綠色
	p[party == 999] <- hsv(1/6, 1, 1-support[party == 999])	# 無黨: 土黃色
	p[party == 90] <- hsv(1/12, 1, 1-support[party == 90])	# 親民黨: 橘色
	p[party == 95] <- hsv(0.45, 1, 1-support[party == 95])	# 台聯: 青色
	p[party %in% CPC] <- hsv(0, 1, 1-support[party %in% CPC])	# 共產黨: 紅色
	p[party %in% THIRD] <- hsv(5/6, 1, 1-support[party %in% THIRD])	# 第三勢力: 紫色
	p
}

region_to_color <- function(dataset_name, color_func = party_to_hsv, select_func = max.col)
{
	factor_to_int <- function(fac)
	{
		levels(fac) <- sub("'", "", levels(fac))
		as.integer(as.character(fac))
	}

	elcand <- function(pathname)
	{
		f <- NULL
		for (i in seq_len(length(pathname))) {
			f <- rbind(f, csvread(paste0(pathname[i],"/elcand.csv"), coltypes = rep("string",16), header=F))
		}
		f$COL7 <- iconv(f$COL7, "UTF-8", "UTF-8")
		f$COL12 <- iconv(f$COL12, "UTF-8", "UTF-8")
		f$COL13 <- iconv(f$COL13, "UTF-8", "UTF-8")
		f <- do.call(cbind, lapply(f, function(s) gsub("'", "", gsub("\"", "", s))))
		f <- as.data.frame(f)
		for (i in 1:5) f[,i] <- factor_to_int(f[,i])
		names(f) <- c("省市別", "縣市別", "選區別", "鄉鎮市區", "村里別", "號次", "名字", "政黨代號", "性別", "出生日期", "年齡", "出生地", "學歷", "現任", "當選註記", "副手")
		f$政黨代號 <- as.integer(as.character(f$政黨代號))
		f$號次 <- as.integer(as.character(f$號次))
		f$鄉鎮市區 <- f$鄉鎮市區 / 10
		f
	}

	elctks <- function(pathname)
	{
		g <- NULL
		for (i in seq_len(length(pathname))) {
			g <- rbind(g, read.csv(paste0(pathname[i],"/elctks.csv"), header=F))
		}
		for (i in 1:5) g[,i] <- factor_to_int(g[,i])
		names(g) <- c("省市別", "縣市別", "選區別", "鄉鎮市區", "村里別", "投開票所", "候選人號次", "得票數", "得票率", "當選註記")
		g$鄉鎮市區 <- g$鄉鎮市區 / 10
		g$LV1 <- ifelse(g$省市別 > 10, g$省市別, g$省市別 * 1000 + g$縣市別)
		g$LV2 <- ifelse(g$LV1 > 100, g$LV1 * 100 + g$鄉鎮市區, g$LV1 * 100000 + g$鄉鎮市區 * 100)
		g$LV3 <- g$LV2 * 1000 + g$村里別
		g
	}

	elprof <- function(pathname)
	{
		h <- NULL
		for (i in seq_len(length(pathname))) {
			h <- rbind(h, read.csv(paste0(pathname[i],"/elprof.csv"), header=F))
		}
		h <- h[,1:10]
		for (i in 1:5) h[,i] <- factor_to_int(h[,i])
		names(h) <- c("省市別", "縣市別", "選區別", "鄉鎮市區", "村里別", "投開票所", "有效票", "無效票", "投票數", "選舉人數")
		h$鄉鎮市區 <- h$鄉鎮市區 / 10
		h
	}
	
	find_party <- function(pathname, g, f)
	{
		ff <- f$政黨代號
		if (grepl("市市長", pathname)) {
			g_ID <- sprintf("%02d%02d%02d", g$省市別, g$縣市別, g$候選人號次)
			names(ff) <- sprintf("%02d%02d%02d", f$省市別, f$縣市別, f$號次)
		}
		else if (grepl("區域議員", pathname)) {
			g_ID <- sprintf("%02d%02d%02d%02d", g$省市別, g$縣市別, g$選區別, g$候選人號次)
			names(ff) <- sprintf("%02d%02d%02d%02d", f$省市別, f$縣市別, f$選區別, f$號次)
		}
		else if (grepl("村里長", pathname)) {
			g_ID <- sprintf("%02d%02d%02d%03d%02d", g$省市別, g$縣市別, g$鄉鎮市區, g$村里別, g$候選人號次)
			names(ff) <- sprintf("%02d%02d%02d%03d%02d", f$省市別, f$縣市別, f$鄉鎮市區, f$村里別, f$號次)
		}
		else if (grepl("總統", pathname)) {
			g_ID <- sprintf("%02d", g$候選人號次)
			names(ff) <- sprintf("%02d", f$號次)
		}
		else if (grepl("立委", pathname)) {
			g_ID <- sprintf("%02d%02d%02d%02d", g$省市別, g$縣市別, g$選區別, g$候選人號次)
			names(ff) <- sprintf("%02d%02d%02d%02d", f$省市別, f$縣市別, f$選區別, f$號次)
		}
		g$政黨代號 <- ff[g_ID]
		g
	}

	# 選擇得票率最高的候選人，這會使同一個縣市的顏色不同
	# supp: 得票率、當選註記
	# stat: 選舉人數、投票人數
	# cand: 候選人、政黨
	generate_color <- function(g, h, f, func)
	{
		level_column <- c("LV1","LV2","LV3")
		COLOR <- character()
		gg <- list()
		gg[[3]] <- g[g$投開票所 == 0 & g$村里別 != 0 & g$鄉鎮市區 != 0 & g$省市別 != 0,]
		gg[[2]] <- g[g$村里別 == 0 & g$鄉鎮市區 != 0 & g$省市別 != 0,]
		gg[[1]] <- g[g$鄉鎮市區 == 0 & g$省市別 != 0,]
		
		for (lv in 1:3)
		{
			if (grepl("區域議員", dataset_name$dir[1])) {
				tt <- tapply(gg[[lv]]$得票數, list(gg[[lv]][[level_column[lv]]],gg[[lv]]$政黨代號), sum)
				tt[is.na(tt)] <- 0
				tt <- apply(tt, 2, function(x) x/rowSums(tt)*100)
			} else {
				tt <- tapply(gg[[lv]]$得票率, list(gg[[lv]][[level_column[lv]]],gg[[lv]]$政黨代號), max)
				tt[is.na(tt)] <- 0
			}
			winner <- data.frame(id=as.numeric(row.names(tt)), vote=tt[(func(tt)-1)*nrow(tt)+seq_len(nrow(tt))], party=colnames(tt)[func(tt)])
			color <- party_to_hsv(winner$party, winner$vote / 100)
			if (lv == 1) {
				names(color) <- ifelse(winner$id > 100, sprintf("%05d", winner$id), winner$id)
			} else if (lv == 2) {
				names(color) <- sprintf("%07d", winner$id)
			} else {
				names(color) <- sprintf("%07d-%03d", as.integer(winner$id / 1000), winner$id %% 1000)
			}
			COLOR <- c(COLOR, color)
		}
		COLOR
	}


	f <- elcand(dataset_name$dir)
	g <- elctks(dataset_name$dir)
	h <- elprof(dataset_name$dir)
	g <- find_party(dataset_name$dir[1], g, f)
	COLOR <- generate_color(g, h, f, select_func)
	COLOR
}
