function Toutput = differenceStructureEmptyOutput(Tinput)

for isample=1:length(Tinput)
    Toutput(isample).diff1 = Tinput(isample).Xrv1 - Tinput(isample).Xrv2;
    Toutput(isample).diff2 = Tinput(isample).Xrv2 - Tinput(isample).Xrv1;
end

% put a value to empty
Toutput(2).diff2 = [];

return;