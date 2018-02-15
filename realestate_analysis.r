
# データを読み込む
est<-read.csv("Tokyo_RealEstate_20162_20172.csv",fileEncoding="Shift-JIS",,skip=1,header=FALSE,stringsAsFactors = FALSE)

# カラム名を英語に変換(macのRstudioでは日本語が上手く表示されないため)
colnames(est)<-c("No","type","region_type","city_code","prefecture","city","district","station","station_distance","total_price","price_per_1_tsubo","floor_plan","area","price_per_unit_area","land_shape","frontage","total_floor_area","building_year","architecture","use","next_use","front_load_direction","front_load_type","front_width","city_planning","building_to_land_ratio","foor_area_to_land_ratio","deal_time","note"

# 23区だけにする
est23<-est[grep("区$",est$city),]

#種類(type)はfactorに変換
est23$種類<-as.factor(est23$type)
levels(est23$type)

# 地域(region_type)は欠損値が多すぎること、市区町村コード(city_code)と都道府県名(prefecture)は全てのデータで同じであることから削除
est23$region_type<-NULL
est23$city_code<-NULL
est23$prefecture<-NULL

#市区町村名(city)、地区名(district)、最寄駅名称(station)をfactor化
est23$city<-as.factor(est23$city)
est23$district<-as.factor(est23$district)
est23$station<-as.factor(est23$station)

# 坪単価のnaの割合を調べる
sum(is.na(est23$price_per_1_tsubo))/length(est23$price_per_1_tsubo)
# 8割越えなので外す
est23$price_per_1_tsubo<-NULL

# 面積(area)を数値型に変換
# ”以上”があるせいで数値変換うまくいかない
grep("以上$",est23$area)
# "以上"を削除
est23<-est23[-grep("以上$",est23$area),]
# 文字列から数値へ
est23$area<-as.integer(est23$area)

# 取引価格(total_price)の分布を確認。hist_total_price参照
hist(est23$total_price)
# 右裾に外れ値あり。除外するか、それとも対数変換するか。一旦対数変換してみる。
est23$log_total_price<-log(est23$total_price)
# 対数変換後の分布は正規分布にかなり近い。hist_log_total_price参照。
hist(est23$log_total_price)

