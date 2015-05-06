from py_post import *
def main(fname):
	p=post_open(fname)
	p.moveto(1)
	Nid=p.node_id(562)
	print Nid
	return 1

if __name__=='__main__':
	main("gridmodel7_YiZhi.t16")
