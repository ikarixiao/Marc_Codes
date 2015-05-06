%求不同高度扭转角的最大�?
Torsion_0mm=importdata('Torsion_0mm_YiZhi.txt');
Torsion_2700mm=importdata('Torsion_2700mm_YiZhi.txt');
Torsion_4500mm=importdata('Torsion_4500mm_YiZhi.txt');
Torsion_6660mm=importdata('Torsion_6660mm_YiZhi.txt');
Torsion_8100mm=importdata('Torsion_8100mm_YiZhi.txt');
TorsionMax=[max(Torsion_0mm),max(Torsion_2700mm),max(Torsion_4500mm),max(Torsion_6660mm),max(Torsion_8100mm)];
dlmwrite('TorsionMax_YiZhi.txt',TorsionMax')