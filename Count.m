function n=Count(Duodian,YiZhi)
[a,~]=size(Duodian);
n=0;
for i=1:a
    if Duodian(i)/YiZhi(i)>1.05
        n=n+1;
    end
end
end