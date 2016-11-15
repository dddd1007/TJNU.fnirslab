spm_convert <- function(filename){

  #读取数据
  datafile <- read_delim(filename, "\t", escape_double = FALSE, trim_ws = TRUE, 
                         skip = 35, col_names = FALSE)
  
  #生成确定行列的数据框
  rownum <- nrow(datafile)
  colnum <- (ncol(datafile)-4)/3
  oxyData <- as.data.frame(matrix(ncol = colnum, nrow = rownum))
  dxyData <- as.data.frame(matrix(ncol = colnum, nrow = rownum))
  tHbData <- as.data.frame(matrix(ncol = colnum, nrow = rownum))
  
  #循环分类取出氧合血红蛋白、脱氧血红蛋白、总血红蛋白变化
  for (i in 1 : colnum) {
    oxyData[,i] <- datafile[,(i*3)+2]
    dxyData[,i] <- datafile[,(i*3)+3]
    tHbData[,i] <- datafile[,(i*3)+4]
  }
  
  #取出其他部分的数据
  Vector_onset <- datafile[,3]        #onset点数据
  fs <- 9.2593                        #并不知道是什么鬼
  nch <- (ncol(datafile)-4)/3         #通道数
  T <- 0.1080
  
  #将数据转换成矩阵
  oxyData <- as.matrix(oxyData)
  dxyData <- as.matrix(dxyData)
  tHbData <- as.matrix(tHbData)
  
  #将数据整合为列表
  nirs_data <- list(oxyData = oxyData, dxyData = dxyData, tHbData = tHbData, 
                    fs = fs, nch = nch, T = T)
  
  #提取文件名
  myfilename <- str_sub(filename, start = 1, end = -5)
  myfilename <- str_c(myfilename, "mat", sep = ".")
  #生成Mat文件
  writeMat(myfilename, nirs_data = nirs_data)
}