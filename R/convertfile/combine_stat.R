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

combine_stat <- function(method = "T") {
  if (method == "T") {
     getcol = 3
   }
  else {
     getcol = 2
  }

  getfilename <- dir()
  txtfilename <- getfilename[stringr::str_detect(getfilename, ".txt|.TXT")]
  result <- 0

  for (i in txtfilename){
    result <- cbind(result, convert_stat(i)[getcol])
  }

  result <- apply(result[-1], MARGIN = 1, mean)
  return(result)
}
