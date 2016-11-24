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

brain_heatmap <- function(ChNum = NA, ncol = NA, nrow = NA, data = NA) {
  if(is.na(ChNum)){
    stop("Please input the number of channal !")
  }
  if(is.na(ncol)){
    stop("Please input the number of column !")
  }
  if(is.na(nrow)){
    stop("Please input the number of row !")
  }
  if(is.na(data)){
    stop("Please input the statistical result of NIRS_SPM !")
  }
}
