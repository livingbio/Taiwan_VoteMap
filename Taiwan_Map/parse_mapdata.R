require(sp)
require(rgdal)
require(XML)
require(stringr)
require(ggplot2)
require(plyr)
require(Cairo)

#
# 地圖和資料是獨立的
# 資料代表區域和顏色的對應，例如: 高雄市->綠色、新北市->藍色等
# 地圖則是區域和多邊形的對應
#
# generate_map() 會傳回 plot_map() 函式
# plot_map(
#	level,					1 or 2 or 3，代表不同層級的地圖
#	COLOR,					COLOR 必須有區域代碼與顏色的對應
#	type = "none",			輸出格式，"pdf"或"png"會輸出為檔案，其他則會輸出在 R plot window
#	filename = "Votemap",	輸出檔名
#	county_select = "",		選擇要畫的區域，必須是縣市層級
#	town_select = "",		選擇要畫的區域，必須是鄉鎮市層級
#	village_select = ""		選擇要畫的區域，必須是村里層級
# )
#
# 範例:
#	a. 產生 2014_vote_major_lv2.png
#		plot_map(2, COLOR, "2014_vote_major", "png")
#   b. 產生 2014_vote_major_lv3.pdf
#		plot_map(3, COLOR, "2014_vote_major", "pdf")
#   c. 指定要產生的地區(可用 | 分隔)
#		plot_map(3, COLOR, town_select = "神岡區|大雅區|沙鹿區|清水區")
#		plot_map(3, COLOR, county_select = "臺北市")
#		plot_map(3, COLOR, village_select = "山崙里")
#

generate_map <- function()
{
	r <- xmlRoot(xmlTreeParse("Taiwan_Map/region_name.xml", useInternalNodes = TRUE))
	region_id <- str_replace_all(xpathSApply(r, "//Code", xmlValue), "\\t", "")
	names(region_id) <- str_replace_all(xpathSApply(r, "//Content", xmlValue), "\\t", "")
	region_name <- names(region_id)
	names(region_name) <- region_id

	sh <- list()
	sh[[1]] <- readOGR("Taiwan_Map", "Level1_1031225_big5")
	sh[[2]] <- readOGR("Taiwan_Map", "Level2_1031225_big5")
	sh[[3]] <- readOGR("Taiwan_Map", "Village_NLSC_TWD97_1040624")

	#
	# 為了相容不同來源的地圖，需要將column name一致化
	# Level 1 縣級地圖，必須有 COUNTY_ID
	# Level 2 鎮級地圖，必須有 TOWN_ID
	# Level 3 村級地圖，必須有 VILLAGE_ID
	#
	names(sh[[1]]@data) <- str_replace(names(sh[[1]]@data), "County_ID", "COUNTY_ID")
	names(sh[[2]]@data) <- str_replace(names(sh[[2]]@data), "Town_ID", "TOWN_ID")
	names(sh[[3]]@data) <- str_replace(names(sh[[3]]@data), "VILLAGE_ID", "VILLAGE_ID")
	column_name <- c("COUNTY_ID", "TOWN_ID", "VILLAGE_ID")

	levels(sh[[1]]@data$C_Name) <- iconv(levels(sh[[1]]@data$C_Name), "utf8", "utf8")
	levels(sh[[2]]@data$C_Name) <- iconv(levels(sh[[2]]@data$C_Name), "utf8", "utf8")
	levels(sh[[2]]@data$T_Name) <- iconv(levels(sh[[2]]@data$T_Name), "utf8", "utf8")
	levels(sh[[3]]@data$C_Name) <- iconv(levels(sh[[3]]@data$C_Name), "utf8", "utf8")
	levels(sh[[3]]@data$T_Name) <- iconv(levels(sh[[3]]@data$T_Name), "utf8", "utf8")
	levels(sh[[3]]@data$V_Name) <- iconv(levels(sh[[3]]@data$V_Name), "utf8", "utf8")

	# 因為 VILLAGE_NAME 會出現亂碼，所以用 region_name 來校正
	v_name <- as.character(sh[[3]]@data$V_Name)
	v_id <- as.character(sh[[3]]@data$VILLAGE_ID)
	error_v_names <- grep("\\?", v_name)
	v_name[error_v_names] <- as.character(region_name[v_id[error_v_names]])
	sh[[3]]@data$V_Name <- factor(v_name)

	plot_orig <- function(level, COLOR, type = "none", filename = "Votemap",
						  county_select = "", town_select = "", village_select = "")
	{
		type <- tolower(type)
		filename <- paste0(filename, "_", level, "_", county_select, town_select, village_select, ".", type)
		filename <- str_replace_all(filename,"\\|","_")
		if (county_select == "" && town_select == "" && village_select == "") {
			lty <- 0
		} else {
			lty <- 1
		}
		lwd <- ifelse(type=="png", 1, 0.01)
#		xlim <- c(118.192967, 122.034616)
#		ylim <- c(21.814761, 26.38528)

		select <- rep(TRUE, nrow(sh[[level]]))
		if (county_select != "")
			select <- grep(county_select, sh[[level]]@data$C_Name)
		else if (town_select != "")
			select <- grep(town_select, sh[[level]]@data$T_Name)
		else if (village_select != "")
			select <- grep(village_select, sh[[level]]@data$V_Name)

		shp <- sh[[level]][select,]
		width <- bbox(shp)["x","max"] - bbox(shp)["x","min"]
		height <- bbox(shp)["y","max"] - bbox(shp)["y","min"]
		if (type == "pdf") {
			pdf(filename, width = width*level, height = height*level)
		}
		else if (type == "none") {
			# display in R
		}
		else {
			png(filename, width = width*level, height = height*level, res = 600, units = "in", bg = "transparent")
		}

		xlim <- bbox(shp)["x",]
		ylim <- bbox(shp)["y",]
		color <- COLOR[as.character(sh[[level]]@data[[column_name[level]]][select])]
		g <- plot(shp, col=color, lty=lty, lwd=lwd, xlim = xlim, ylim = ylim)

		if (county_select == "" && town_select == "" && village_select == "") {
			par(new=TRUE)
			plot(sh[[1]], bg="#ff000000", lty = 1, lwd = lwd, xlim = xlim, ylim = ylim)
		}

		if (type == "pdf" || type == "png") dev.off()
	}

	plot_gg <- function(level, COLOR, type = "none", filename = "Votemap",
						county_select = "", town_select = "", village_select = "")
	{
		windowsFonts(chFont=windowsFont("細明體"))
		par(family="chFont")

		shp <- sh[[level]][sh[[level]]@plotOrder,]
		select_c <- rep(TRUE, nrow(shp))
		select_t <- select_c
		select_v <- select_c
		if (county_select != "")
			select_c <- grepl(county_select, shp@data$C_Name)
		if (town_select != "")
			select_t <- grepl(town_select, shp@data$T_Name)
		if (village_select != "")
			select_v <- grepl(village_select, shp@data$V_Name)
		select <- select_c & select_t & select_v
		shp <- shp[select,]
		width <- bbox(shp)["x","max"] - bbox(shp)["x","min"]
		height <- bbox(shp)["y","max"] - bbox(shp)["y","min"]
		xlim <- bbox(shp)["x",]
		ylim <- bbox(shp)["y",]
		if (county_select == "" && town_select == "" && village_select == "") {
			xlim <- c(118,123)
			ylim <- c(21.5,25.5)
		}
	
		color <- COLOR[as.character(shp@data[[column_name[level]]])]
		names(color) <- NULL
		shp <- spChFIDs(shp, sprintf("%06d",seq_len(nrow(shp))))
		shp.df <- fortify(shp)

		ggobj <- ggplot() +
			geom_polygon(aes(long, lat, group=group, fill=id), data = shp.df) +
			coord_equal(xlim = xlim, ylim = ylim) +
			scale_fill_manual(values = color) +
			theme(legend.position="none")

		gnames <- data.frame()
		for(i in seq_len(nrow(shp))) gnames <- rbind(gnames,rowMeans(bbox(shp[i,])))
		colnames(gnames) <- c("long", "lat")
		if (level == 2)
			gnames$name <- shp@data$T_Name
		else
			gnames$name <- shp@data$V_Name

		if (county_select == "" && town_select == "" && village_select == "") {
			if (level == 2) {
				ggobj <- ggobj + geom_polygon(data = fortify(sh[[1]]), aes(long, lat, group=group), size=0.02, fill="#FF000000", color = "black")
				ggobj <- ggobj + geom_text(aes(long, lat, label=name), gnames, size=0.2, color = "white", family="chFont")
				width <- width * 2
				height <- height * 2
			}
			else if (level == 3) {
				ggobj <- ggobj + geom_polygon(data = fortify(sh[[2]]), aes(long, lat, group=group), size=0.02, fill="#FF000000", color = "black")
				ggobj <- ggobj + geom_text(aes(long, lat, label=name), gnames, size=0.1, color = "white", family="chFont")
				width <- width * 6
				height <- height * 6
			}
		} else {
			if (level == 2 && county_select != "") {
				tselect <- grepl(county_select, sh[[2]]@data$C_Name)
				ggobj <- ggobj + geom_polygon(data = fortify(sh[[2]][tselect,]), aes(long, lat, group=group), size=0.02, fill="#FF000000", color = "black")
				ggobj <- ggobj + geom_text(aes(long, lat, label=name), gnames, size=1, color = "white", family="chFont")
			} else if (level == 3 && (county_select != "" || town_select != "")) {
				if (county_select != "")
					tselect <- grepl(county_select, sh[[2]]@data$C_Name)
				else
					tselect <- grepl(town_select, sh[[2]]@data$T_Name)
				ggobj <- ggobj + geom_polygon(data = fortify(sh[[2]][tselect,]), aes(long, lat, group=group, fill=id), size=0.02, fill="#FF000000", color = "black")
				font_size <- ifelse(town_select != "", 3, 0.2)
				ggobj <- ggobj + geom_text(aes(long, lat, label=name), gnames, size=font_size, color = "white", family="chFont")
			}
			width <- 3 * level
			height <- 3 * level
		}

		type <- tolower(type)
		if (type == "pdf" || type == "png") {
			filename <- paste0(filename, "_", level, "_", county_select, town_select, village_select, ".", type)
			filename <- str_replace_all(filename,"\\|","_")
			ggsave(filename, ggobj, device=cairo_pdf, width = width, height = height, dpi = 600)
# 			default width = par("din")[1]
# 			default height = par("din")[2]
# 			default unit = "in" (other choices are "cm" or "mm")
# 			default dpi = 300
		}
		ggobj
	}

	list(plot=plot_orig, ggplot=plot_gg)
}

map <- generate_map()

