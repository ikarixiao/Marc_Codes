from py_post import *
def main(fname):
	p=post_open(fname)
	p.moveto(1)
	ninc=p.increments()
	nele=p.elements()
	print p.element(2).type
	return 1

if __name__=="__main__":
	main("gridmodel7_YiZhi.t16")
