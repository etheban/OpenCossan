function [Toutput] = invalidcovfunction1(Tinput)
%function [Moutput] = expcovfunction(Minput)

sigma = 1;
b = 0.5;

Toutput(size(Tinput,1),1) = struct;
for i=1:length(Tinput),
    t1  = Tinput(i).t1;
    Toutput(i).fcov  = sigma^2*exp(-1/b*abs(t1));
end

% Moutput = zeros(size(Minput,1),1);
% for i=1:size(Minput,1),
%     t1 = Minput(i,1);
%     t2 = Minput(i,2);
%     Moutput(i)  = sigma^2*exp(-1/b*abs(t2-t1));
% end

return