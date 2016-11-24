#
#                       _oo0oo_
#                      o8888888o
#                      88" . "88
#                      (| -_- |)
#                      0\  =  /0
#                    ___/`---'\___
#                  .' \\|     |# '.
#                 / \\|||  :  |||# \
#                / _||||| -:- |||||- \
#               |   | \\\  -  #/  |   |
#               | \_|  ''\---/''  |_/ |
#               \  .-\__  '-'  ___/-. /
#             ___'. .'  /--.--\  `. .'___
#          ."" '<  `.___\_<|>_/___.' >' "".
#         | | :  `- \`.;`\ _ /`;.`/ - ` : | |
#         \  \ `_.   \_ __\ /__ _/   .-` /  /
#     =====`-.____`.___ \_____/___.-`___.-'=====
#                       `=---='
#
#
#     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#               佛祖保佑         永无BUG
#
# 天津师范大学 心理与行为研究院 近红外组
# 夏骁凯 编写
# 有问题请联系 dddd1007@gmail.com

convert_stat <- function(x = NA){
  if(is.na(x)) stop("Please input your filename!")
  rawfile <- readr::read_delim(x, delim = ";", col_names = FALSE, skip = 8)
  row_num <- nrow(rawfile)
  temptable <- as.data.frame(matrix(0, nrow = row_num, ncol = 3))
  colnames(temptable) <- c("CH", "Beta", "T-statistics")

  for(i in  1 : row_num){
    a <- str_split(rawfile$X1[i], pattern = ":")
    b <- str_split(a[[1]][1], pattern = " ")
    c <- str_split(a[[1]][2], pattern = ", ")
    temptable[i,1] <- as.numeric(b[[1]][2])
    temptable[i,2] <- as.numeric(c[[1]][1])
    temptable[i,3] <- as.numeric(c[[1]][2])
  }

  temptable <- na.omit(temptable)

  return(temptable)
}
