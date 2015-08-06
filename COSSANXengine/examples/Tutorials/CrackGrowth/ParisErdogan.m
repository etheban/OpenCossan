function Tout = ParisErdogan( Tin )


% dummy Memory
Cdummy=num2cell(zeros(length(Tin),1));
Tout=struct('dadn',Cdummy);


for i=1:length(Tin)
   Tout(i).dadn = Tin.C*(Tin(i).sif)^ Tin(i).m;
end

end

