from py_post import *
def main(fname):
	p=post_open(fname)
	p.moveto(400)
	grids=p.set(5).items   #经调试可知set 5为屋架,grids为桁架单元编号数组
	for i in range(0,p.element_scalars()):
		print p.element_scalar_label(i)
		print p.element_scalar(grids[i],i)
	return 1

if __name__=="__main__":
	main("gridmodel7_YiZhi.t16")
