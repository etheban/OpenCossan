function [Toutput] = invalidcovfunction2(Tinput)
%function [Moutput] = expcovfunction(Minput)

sigma = -2;
b = 0.5;

Toutput(size(Tinput,1),1) = struct;
for i=1:length(Tinput),
    t1  = Tinput(i).t1;
    t2  = Tinput(i).t2;
    Toutput(i).fcov  = sigma;
end

% Moutput = zeros(size(Minput,1),1);
% for i=1:size(Minput,1),
%     t1 = Minput(i,1);
%     t2 = Minput(i,2);
%     Moutput(i)  = sigma^2*exp(-1/b*abs(t2-t1));
% end

return