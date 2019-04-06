getDSWS=function(test){#算法1:获取DSWS
  r=nrow(test);
  score=-10000000;#score取-10000000，足够小
  lborder=round(r/60);
  rborder=round(r/50);#获取DSWS所在区间intervals
  size=(rborder-lborder)/10;#获取十等分小区间的大小
  lborder1=lborder;
  rborder1=lborder1+size;#令[lborder1,rborder1]为当前处理区间
  for(i in seq(1,10,1)){
    len1=round(runif(1,l1,r1));#取随机数作为临时DSWS
    stp1=round(len1/10);#取窗口大小的1/10作为步长
    num=1;
    sco=vector(length=0);
    for(i in seq(1,r-len1+1,stp1)){
      t=test[i:(i+len1-1),];
      sco[num]=score(mmhc(t),t);#记录窗口评分
      num=num+1;
    }
    if(mean(sco)>score){
      len=len1;
    }
    lborder1=rborder1;
    rborder1=rborder1+sca;#进入下一个区间
  }
  return(len);
}
getBNLHD=function(test){#算法2:获取BNLHD
  r=nrow(test);
  score=-10000000;#score取-10000000，足够小
  lborder=round(r/60);
  rborder=round(r/50);#获取DSWS所在区间intervals
  size=(rborder-lborder)/10;#获取十等分小区间的大小
  lborder1=lborder;
  rborder1=lborder1+size;#令[lborder1,rborder1]为当前处理区间
  for(i in seq(1,10,1)){
    len1=round(runif(1,l1,r1));#取随机数作为临时DSWS
    stp1=round(len1/10);#取窗口大小的1/10作为步长
    num=1;
    sco=vector(length=0)
    for(i in seq(1,r-len1+1,stp1)){
      t=test[i:(i+len1-1),];
      sco[num]=score(mmhc(t),t);#记录窗口评分
      num=num+1;
    }
    if(mean(sco)>score){
      score=mean(sco);
    }
    lborder1=rborder1;
    rborder1=rborder1+sca;#进入下一个区间
  }
  return(score);
}
removeNOISE=function(test){#算法3:输入一个数据集，返回优化后的数据集
  DSWS=getDSWS(test);
  stp=round(DSWS/10);#根据窗口大小得到步长
  BNLHD=getBNLHD(test);
  diff=vector(length=0);
  r=nrow(test);#获取数据规模
  for(i in seq(1,r+1,1)){
    diff[i]=0
  }#初始化差分数组diff
  for(i in seq(1,r-DSWS+1,stp)){
    t=test[i:(i+len1-1),];
    if(score(mmhc(t),t)>=BNLHD){
      diff[i]=diff[i]+1;
      diff[i+len1]=diff[i+len1]-1;
    }
  }
  for(i in seq(2,r,1)){
    diff[i]=diff[i]+diff[i-1]
  }
  del=vector(length=0);#将要删除的数据存在del中，最后一并删除，提高效率
  num=0;
  for(i in seq(1,r,1)){
    if(diff[i]==0){
      del[num]=i;
      num=num+1;
    }
  }
  test=test[-del,]
  return(test);
}
