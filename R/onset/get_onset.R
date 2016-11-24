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

get_onset <- function(filename){
  matfile <- R.matlab::readMat(filename)
  onset_mat <- unlist(matfile[1]$nirs.data[7])
  onsetnumber <- unique(onset_mat)
  onsetnumber <- onsetnumber[onsetnumber != 0]
  trail_num <- length(onsetnumber)
  result <- data.frame(matrix(0, ncol = 2, nrow = trail_num))
  colnames(result) <- c("onset", "scans")

  for(i in 1 : trail_num){
  onset_detect <- onsetnumber[i]
  result[i,1] <- onset_detect
  result[i,2] <- stringr::str_c(as.character(which(onset_mat == onset_detect)), collapse = " ")
  }

  myfilename <- stringr::str_sub(filename, start = 1, end = -5)
  myfilename <- stringr::str_c(myfilename,"_onset",".csv", sep = "")
  write.csv(result, myfilename)
}
