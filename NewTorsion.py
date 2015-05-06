#求结构的扭转角，将结果输出至txt
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
	if(COS>1):
		COS=1
	Rad=math.acos(math.fabs(COS))
	return Rad/(2*math.pi)*360
def VecPlus(a,b):                             #向量相加
	return [a[i]+b[i] for i in range(min(len(a),len(b)))]
def VecMinus(a,b):                           #向量相减
	return [a[i]-b[i] for i in range(min(len(a),len(b)))]
def Mean(a):           #求数组平均数
	sum=0
	for i in range(0,len(a)):
		sum=sum+a[i]
	return sum/len(a)

def main(fname):
	p=post_open(fname)
	H_8100mm_Out=open("Torsion_8100mm_YiZhi.txt","wt")
	H_6660mm_Out=open("Torsion_6660mm_YiZhi.txt","wt")
	H_4500mm_Out=open("Torsion_4500mm_YiZhi.txt","wt")
	H_2700mm_Out=open("Torsion_2700mm_YiZhi.txt","wt")
	H_0mm_Out=open("Torsion_0mm_YiZhi.txt","wt")
	p.moveto(1)
	ninc=p.increments()  #增量步数
	H_8100mm_Node_Num=[9,297,677,524]
	H_6660mm_Node_Num=[1096,1060,1188,1140]
	H_4500mm_Node_Num=[541,585,169,590]
	H_2700mm_Node_Num=[824,916,1172,904]
	H_0mm_Node_Num=[562,653,707,644]
	for i in range(0,len(H_8100mm_Node_Num)):
		H_8100mm_Node_Num[i]=H_8100mm_Node_Num[i]-1
		H_6660mm_Node_Num[i]=H_6660mm_Node_Num[i]-1
		H_4500mm_Node_Num[i]=H_4500mm_Node_Num[i]-1
		H_2700mm_Node_Num[i]=H_2700mm_Node_Num[i]-1
		H_0mm_Node_Num[i]=H_0mm_Node_Num[i]-1
	Angles_8100mm=[]
	Angles_6660mm=[]
	Angles_4500mm=[]
	Angles_2700mm=[]
	Angles_0mm=[]
	for i in range(1,ninc):
		p.moveto(i)
		for j in range(0,4):
			for k in range(j+1,4):
				#8100mm扭转角
				Point1=[p.node(H_8100mm_Node_Num[j]).x,p.node(H_8100mm_Node_Num[j]).y,p.node(H_8100mm_Node_Num[j]).z]
				Point2=[p.node(H_8100mm_Node_Num[k]).x,p.node(H_8100mm_Node_Num[k]).y,p.node(H_8100mm_Node_Num[k]).z]
				Vec1=VecMinus(Point1,Point2)
				NewPoint1=VecPlus(Point1,list(p.node_displacement(H_8100mm_Node_Num[j])))
				NewPoint2=VecPlus(Point2,list(p.node_displacement(H_8100mm_Node_Num[k])))
				Vec2=VecMinus(NewPoint1,NewPoint2)
				Angles_8100mm.append(VecsAngle(Vec1,Vec2))
				#6660mm扭转角
				Point1=[p.node(H_6660mm_Node_Num[j]).x,p.node(H_6660mm_Node_Num[j]).y,p.node(H_6660mm_Node_Num[j]).z]
				Point2=[p.node(H_6660mm_Node_Num[k]).x,p.node(H_6660mm_Node_Num[k]).y,p.node(H_6660mm_Node_Num[k]).z]
				Vec1=VecMinus(Point1,Point2)
				NewPoint1=VecPlus(Point1,list(p.node_displacement(H_6660mm_Node_Num[j])))
				NewPoint2=VecPlus(Point2,list(p.node_displacement(H_6660mm_Node_Num[k])))
				Vec2=VecMinus(NewPoint1,NewPoint2)
				Angles_6660mm.append(VecsAngle(Vec1,Vec2))
				#4500mm扭转角
				Point1=[p.node(H_4500mm_Node_Num[j]).x,p.node(H_4500mm_Node_Num[j]).y,p.node(H_4500mm_Node_Num[j]).z]
				Point2=[p.node(H_4500mm_Node_Num[k]).x,p.node(H_4500mm_Node_Num[k]).y,p.node(H_4500mm_Node_Num[k]).z]
				Vec1=VecMinus(Point1,Point2)
				NewPoint1=VecPlus(Point1,list(p.node_displacement(H_4500mm_Node_Num[j])))
				NewPoint2=VecPlus(Point2,list(p.node_displacement(H_4500mm_Node_Num[k])))
				Vec2=VecMinus(NewPoint1,NewPoint2)
				Angles_4500mm.append(VecsAngle(Vec1,Vec2))
				#2700mm扭转角
				Point1=[p.node(H_2700mm_Node_Num[j]).x,p.node(H_2700mm_Node_Num[j]).y,p.node(H_2700mm_Node_Num[j]).z]
				Point2=[p.node(H_2700mm_Node_Num[k]).x,p.node(H_2700mm_Node_Num[k]).y,p.node(H_2700mm_Node_Num[k]).z]
				Vec1=VecMinus(Point1,Point2)
				NewPoint1=VecPlus(Point1,list(p.node_displacement(H_2700mm_Node_Num[j])))
				NewPoint2=VecPlus(Point2,list(p.node_displacement(H_2700mm_Node_Num[k])))
				Vec2=VecMinus(NewPoint1,NewPoint2)
				Angles_2700mm.append(VecsAngle(Vec1,Vec2))
				#0mm扭转角
				Point1=[p.node(H_0mm_Node_Num[j]).x,p.node(H_0mm_Node_Num[j]).y,p.node(H_0mm_Node_Num[j]).z]
				Point2=[p.node(H_0mm_Node_Num[k]).x,p.node(H_0mm_Node_Num[k]).y,p.node(H_0mm_Node_Num[k]).z]
				Vec1=VecMinus(Point1,Point2)
				NewPoint1=VecPlus(Point1,list(p.node_displacement(H_0mm_Node_Num[j])))
				NewPoint2=VecPlus(Point2,list(p.node_displacement(H_0mm_Node_Num[k])))
				Vec2=VecMinus(NewPoint1,NewPoint2)
				Angles_0mm.append(VecsAngle(Vec1,Vec2))
		H_8100mm_Out.write(str(Mean(Angles_8100mm)))
		H_8100mm_Out.write("\n")
		H_6660mm_Out.write(str(Mean(Angles_6660mm)))
		H_6660mm_Out.write("\n")
		H_4500mm_Out.write(str(Mean(Angles_4500mm)))
		H_4500mm_Out.write("\n")
		H_2700mm_Out.write(str(Mean(Angles_2700mm)))
		H_2700mm_Out.write("\n")
		H_0mm_Out.write(str(Mean(Angles_0mm)))
		H_0mm_Out.write("\n")
	return 1

if __name__=="__main__":
	main("gridmodel7_YiZhi.t16")
