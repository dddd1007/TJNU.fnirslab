spm_convert_all <- function(){
  getfilename <- dir()
  txtfilename <- getfilename[str_detect(getfilename, ".txt|.TXT")]
  spm_convert(txtfilename)
}
