from py_post import *
def main(fname):
	p=post_open(fname)
	fout=open("MaxStress_YiZhi.txt","wt")
	p.moveto(1)
	ninc=p.increments()  #增量步数
	nele=p.elements()  #单元数
	grids=p.set(5).items   #经调试可知set 5为屋架,grids为桁架单元编号数组
	gridsnum=len(grids)   #桁架单元数量
	for i in range(0,gridsnum):
		temp=[];
		for j in range(0,ninc):
			p.moveto(j)
			StressTemp=p.element_scalar(grids[i],6)  #经调试scalar 6为11方向（xx向）的正应力
			temp.append(abs(StressTemp[0].value))
		print max(temp)
		fout.write(str(max(temp)))
		fout.write("\n")
	fout.close()
	return 1

if __name__=="__main__":
	main("gridmodel7_YiZhi.t16")
