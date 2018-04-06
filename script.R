# データの読み込み
est<-read.csv("Tokyo_RealEstate_20162_20172.csv",fileEncoding="Shift-JIS",,skip=1,header=FALSE,stringsAsFactors = FALSE,na.strings=(c("NA", "")))

# 元データでは特徴量の名前が日本語だが英語にする
colnames(est)<-c("No","type","region_type","city_code","prefecture","city","district","station","station_distance","total_price","price_per_1_tsubo","floor_plan","area","price_per_unit_area","land_shape","frontage","total_floor_area","building_year","architecture","use","next_use","front_load_direction","front_load_type","front_load_width","city_planning","building_to_land_ratio","floor_area_to_land_ratio","deal_time","note")
# 参考
## No->No
## 種類->type
## 地域->region_type
## 市区町村コード->city_code
## 都道府県名->prefecture
## 市区町村名->city
## 地区名->district
## 最寄駅:名称 station
## 最寄駅:距離（分）->station_distance
## 取引価格（総額）->total_price
## 坪単価->price_per_1_tsubo
## 間取り->floor_plan
## 面積->area
## 取引価格面積単価->price_per_unit_area
## 土地の形状	->land_shape
## 間口->frontage
## 延床面積->total_floor_area
## 建築年->building_year
## 建物の構造->architecture
## 建物の用途->use
## 今後の利用目的->next_use
## 前面道路：方位->front_load_direction
## 前面道路：種類->front_load_type
## 前面道路：幅員（ｍ）->front_load_width
## 都市計画：city_planning
## 建ぺい率->building_to_land_ratio
## 容積率->floor_area_to_land_ratio
## 取引時点->deal_time
## 備考->note

# 市区町村(city)をもとに23区だけを分析対象とする
est23<-est[grep("区$",est$city),]

# 最寄駅:距離（分）の前処理
## 「1H30~2H」「30分~60分」のような曖昧なデータは分析対象から外す。
est23<-est23[-grep("H|分",est23$station_distance),]
## 空白行は消す
est23<-est23[!is.na(est23$station_distance),]
## intに変換
est23$station_distance<-as.integer(est23$station_distance)

# 間取りの前処理
## 空白行は消す
est23<-est23[!is.na(est23$floor_plan),]

## 部屋数(number_of_floor)という特徴量を作成
est23$number_of_floor[grep("１",est23$floor_plan)]<-1
est23$number_of_floor[grep("２",est23$floor_plan)]<-2
est23$number_of_floor[grep("３",est23$floor_plan)]<-3
est23$number_of_floor[grep("４",est23$floor_plan)]<-4
est23$number_of_floor[grep("５",est23$floor_plan)]<-5
est23$number_of_floor<-as.integer(est23$number_of_floor)

## オープンフロア、スタジオ、メゾネットは今回は分析対象から外す
est23<-est23[-grep("オープンフロア|スタジオ|メゾネット",est23$floor_plan),]

## キッチンをレベル別に分ける
est23$kitchen_level[grep("Ｒ",est23$floor_plan)]<-"R"
est23$kitchen_level[grep("Ｋ",est23$floor_plan)]<-"K"
est23$kitchen_level[grep("ＤＫ",est23$floor_plan)]<-"DK"
est23$kitchen_level[grep("ＬＤＫ",est23$floor_plan)]<-"LDK"
est23$kitchen_level[is.na(est23$kitchen_level)]<-"N"

## サービスルームをカウント
est23$service_room<-0
est23$service_room[grep("Ｓ",est23$floor_plan)]<-1
est23$service_room<-as.integer(est23$service_room)

## 間取りが分解できたので、元の特徴量は削除
est23$floor_plan<-NULL

# 面積の前処理
## 「X以上」のような曖昧なデータは削除
est23<-est23[-grep("以上$",est23$area),]
## 文字列から数値へ
est23$area<-as.integer(est23$area)

# 建築年の前処理
## 和暦を西暦に変換後、今年(2018年)から何年前かを計算
## 和暦の建築年を西暦に加工するために、加工用の特徴量を追加
est23$AD_building_year<-est23$building_year
## ある和暦が始まる前年の西暦を、あとで計算する用の基準年(std_year)として用意
est23$std_year[grep("昭和",est23$AD_building_year)]<-1925
est23$std_year[grep("平成",est23$AD_building_year)]<-1988
est23$std_year<-as.integer(est23$std_year)
## 余分な文字（昭和、平成）を消す
est23$AD_building_year<-sub("昭和","",est23$AD_building_year)
est23$AD_building_year<-sub("平成","",est23$AD_building_year)
est23$AD_building_year<-sub("年","",est23$AD_building_year)
## 元年があるので、「元」を「１」に変換
est23$AD_building_year<-sub("元","1",est23$AD_building_year)
## 計算できるように数値型に変換
est23$AD_building_year<-as.integer(est23$AD_building_year)
## 1つNAが生成されるので、そのデータの元の値を見る
est23$building_year[is.na(est23$AD_building_year)]
## [1] "戦前"
## まさかの展開... 具体的な年が分からないので、削除。
est23<-est23[!is.na(est23$AD_building_year),]
## 基準年を使って西暦に変換
est23$AD_building_year<-est23$AD_building_year+est23$std_year
## 現在（2018年）を基準に築年数(ag)を計算
est23$age<-2018-est23$AD_building_year
## 築年数が計算できたので、余分な変数を消す
est23$building_year<-NULL
est23$AD_building_year<-NULL
est23$std_year<-NULL

# 今後の利用目的の前処理
## 空白行は消す
est23<-est23[!is.na(est23$next_use),]
## factorに変換
est23$next_use<-as.factor(est23$next_use)

# 都市計画の前処理
est23<-est23[!is.na(est23$city_planning),]

# 備考の前処理
unique(est23$note)
##[1] "未改装を購入"                     "改装済を購入"
##[3] NA                                 "調停・競売等、未改装を購入"
##[5] "関係者間取引、未改装を購入"       "調停・競売等"
##[7] "他の権利・負担付き、未改装を購入" "調停・競売等、改装済を購入"

## 「他の権利・負担付き」はどのような権利・負担なのか不明確な上に、取引価格に影響があると予測できるので、そのデータは削除
est23<-est23[-grep("他の権利・負担付き",est23$note),]
## 改装フラグ、調停競売フラグ、、関係者間取引フラグが作成できそうだが、データ数的に改装フラグしか使えなさそう。
## "改装済を購入"という文字があれば、改装フラグを立て、それ以外は改装フラグを立てない。
est23$renovation_flg<-0
est23$renovation_flg[grep("改装済を購入",est23$note)]<-1
## 備考は不要なので、削除
est23$note<-NULL


# 以下の特徴量は欠損が多すぎるので削除
est23$region_type<-NULL
est23$city_code<-NULL
est23$prefecture<-NULL
est23$price_per_1_tsubo<-NULL
est23$land_shape<-NULL
est23$frontage<-NULL
est23$total_floor_area<-NULL

# 今回の分析では地理的情報はモデルに組み込まない
est23$city<-NULL
est23$district<-NULL
est23$station<-NULL

# 取引価格を推定するという目的上、取引価格面積単価は説明変数として使ってはいけない。
est23$price_per_unit_area<-NULL

# 線形回帰を実施
m_lm<-lm(total_price~.,data=est23)
# 線形回帰後のp値からモデルに対して無影響だと考えられる特徴量は削除
est23$type<-NULL
est23$architecture<-NULL
est23$use<-NULL
est23$front_load_direction<-NULL
est23$front_load_type<-NULL
est23$front_load_width<-NULL
est23$building_to_land_ratio<-NULL
est23$floor_area_to_land_ratio<-NULL
est23$deal_time<-NULL

# 学習データとテストデータに分ける
set.seed(111)
train_sample<-sample(11510,8000)
est23_train<-est23[train_sample,]
est23_test<-est23[-train_sample,]
# 線形回帰



m_lm<-lm(total_price~.,data=est23)
