from py_post import *
def main(fname):
	p=post_open(fname)
	fout=open("shearforce_YiZhi.txt","wt")
	bottoms=[653,659,652,640,562,686,580,436,599,706,709,613,233,626,502,667,642,675,682,696,680,685,705,687,668,690,692,313,536,695,661,697,688,699,472,701,669,703,707,632,618,708,607,644]
	botnum=len(bottoms)
	real_bot=[]
	for i in range(0,botnum):
		real_bot.append(bottoms[i]-1)
	p.moveto(1)
	ninc=p.increments()
	for i in range(0,ninc):
		p.moveto(i)
		ShearForceTemp=0
		for j in range(0,botnum):
			ID_TEMP=real_bot[j]
			ShearForceTemp+=p.node_scalar(ID_TEMP,3)
		#fout.write(p.node_scalar_label(3))
		#fout.write("  ")
		fout.write(str(ShearForceTemp/1000))
		fout.write("\n")
	fout.close()
	return 1

if __name__=='__main__':
	main("gridmodel7_YiZhi.t16")
