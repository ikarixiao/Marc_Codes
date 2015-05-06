function Ratio=OverRatio(Duodian,YiZhi)
[a,~]=size(Duodian);
Ratio=zeros(a,1);
for i=1:a
    Ratio(i)=Duodian(i)/YiZhi(i);
end
end