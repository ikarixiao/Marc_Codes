%Stress_xxx.txt�����˲�ͬ����������������ܵ�Ԫʱ����Ӧ������λΪMPa��
%��ȡӦ���洢�ھ����У����㳬�رȣ�ͳ�Ƴ��ص�Ԫ����
%���ߣ�L4in
%ʱ�䣺April 14th, 2015

Stress_YiZhi=importdata('Stress_YiZhi.txt');
Stress_Duodian_600=importdata('Stress_Duodian_600.txt');
Stress_Duodian_200=importdata('Stress_Duodian_200.txt');
Stress_Duodian_100=importdata('Stress_Duodian_100.txt');
Stress_Duodian_50=importdata('Stress_Duodian_50.txt');
Max_YiZhi=(max(abs(Stress_YiZhi)))';
Max_Duodian_600=(max(abs(Stress_Duodian_600)))';
Max_Duodian_200=(max(abs(Stress_Duodian_200)))';
Max_Duodian_100=(max(abs(Stress_Duodian_100)))';
Max_Duodian_50=(max(abs(Stress_Duodian_50)))';
%dlmwrite('MaxStress_Duodian_50.txt',Max_Duodian_50);
%Count_XXX������Ϊ���ּ��������³��ص�Ԫ����
Count_600=Count(Max_Duodian_600,Max_YiZhi);
Count_200=Count(Max_Duodian_200,Max_YiZhi);
Count_100=Count(Max_Duodian_100,Max_YiZhi);
Count_50=Count(Max_Duodian_50,Max_YiZhi);
%Ratio_XXX������Ϊ���ּ��������³��ޱ�����
Ratio_600=OverRatio(Max_Duodian_600,Max_YiZhi);
Ratio_200=OverRatio(Max_Duodian_200,Max_YiZhi);
Ratio_100=OverRatio(Max_Duodian_100,Max_YiZhi);
Ratio_50=OverRatio(Max_Duodian_50,Max_YiZhi);
format short;
MaxMin(Ratio_600)
MaxMin(Ratio_200)
MaxMin(Ratio_100)
MaxMin(Ratio_50)