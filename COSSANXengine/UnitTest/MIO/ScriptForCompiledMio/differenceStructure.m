for i = 1:length(Tinput)
    Toutput(i).diff1 = Tinput(i).Xrv1 - half(Tinput(i).Xrv2);
    Toutput(i).diff2 = Tinput(i).Xrv2 - Tinput(i).Xrv1;
end