%���������ܵ�Ԫ��Ӧ�����ֵ���������д��StressMax.txt��
Stress=importdata('Stress_YiZhi.txt');
size(Stress)
Stress=Stress/1e6;%��λ���㣬��Pa�����MPa
Max=(max(abs(Stress)))';%��Ӧ�������Ӧ������ֵ�����ֵ
dlmwrite('MaxStress_YiZhi.txt',Max);
