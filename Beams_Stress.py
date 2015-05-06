#输出混凝土梁单元的应力至txt
from py_post import *
def main(fname):
	p=post_open(fname)
	fout=open("BeamStress_YiZhi.txt","wt")
	p.moveto(1)
	ninc=p.increments()  #增量步数
	Beams=p.set(1).items  #set1为梁
	BeamsNum=len(Beams)
	count=0
	#Stress=[[] for i in range(BeamsNum)]
	for i in range(0,ninc):
		p.moveto(i)
		StressTemp=p.element_scalar(Beams[218],3)
		print StressTemp

	
	fout.close()
	return 1

if __name__=="__main__":
	main("gridmodel7_YiZhi.t16")
