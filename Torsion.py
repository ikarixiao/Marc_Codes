#求结构的扭转角度，将结果输出至txt
from py_post import *
import math
def VecLen(vec):                    #求向量长度
	VectorLength=0
	for i in range(0,len(vec)):
		VectorLength=VectorLength+vec[i]*vec[i]
	return math.sqrt(VectorLength)
def VecsAngle(vec1,vec2):      #求向量夹角,夹角为角度格式
	if(len(vec1)!=len(vec2)):
		return
	VecDot=0
	for i in range(0,min(len(vec1),len(vec2))):
		VecDot=VecDot+vec1[i]*vec2[i]
	COS=VecDot/(VecLen(vec1)*VecLen(vec2))
	Rad=math.acos(math.fabs(COS))
	return Rad/(2*math.pi)*360
def VecPlus(a,b):                             #向量相加
	return [a[i]+b[i] for i in range(min(len(a),len(b)))]
def VecMinus(a,b):                           #向量相减
	return [a[i]-b[i] for i in range(min(len(a),len(b)))]

def main(fname):
	p=post_open(fname)
	fout=open("Torsion_YiZhi.txt","wt")
	#fout.write("一致激励下每个进程步的结构扭转程度（单位：角度）")
	#fout.write("\n")
	p.moveto(1)
	ninc=p.increments()  #增量步数
	Top_Node_Num=[9,297,677,524]
	Real_Top_Node_Num=[]
	Angles=[]
	for i in range(0,len(Top_Node_Num)):
		Real_Top_Node_Num.append(Top_Node_Num[i]-1)
	for i in range(1,ninc):
		p.moveto(i)
		for j in range(0,len(Top_Node_Num)):
			for k in range(j+1,len(Top_Node_Num)):
				Point1=[p.node(Real_Top_Node_Num[j]).x,p.node(Real_Top_Node_Num[j]).y,p.node(Real_Top_Node_Num[j]).z]
				Point2=[p.node(Real_Top_Node_Num[k]).x,p.node(Real_Top_Node_Num[k]).y,p.node(Real_Top_Node_Num[k]).z]
				Vec1=VecMinus(Point1,Point2)
				NewPoint1=VecPlus(Point1,list(p.node_displacement(Real_Top_Node_Num[j])))
				NewPoint2=VecPlus(Point2,list(p.node_displacement(Real_Top_Node_Num[k])))
				Vec2=VecMinus(NewPoint1,NewPoint2)
				Angles.append(VecsAngle(Vec1,Vec2))
		fout.write(str(max(Angles)))
		fout.write("\n")
	return 1

if __name__=="__main__":
	main("gridmodel7_YiZhi.t16")
