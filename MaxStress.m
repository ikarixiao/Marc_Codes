%求出各个桁架单元的应力最大值，并将结果写如StressMax.txt。
Stress=importdata('Stress_YiZhi.txt');
size(Stress)
Stress=Stress/1e6;%单位换算，将Pa换算成MPa
Max=(max(abs(Stress)))';%求应力矩阵的应力绝对值的最大值
dlmwrite('MaxStress_YiZhi.txt',Max);
