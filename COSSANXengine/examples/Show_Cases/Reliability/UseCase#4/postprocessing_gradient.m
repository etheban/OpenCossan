close all
clear all
clc

load Xgradient_Building
load Xpf_Building_parallel
Valpha = Xg.Valpha;

plot(1:length(Valpha),Valpha,'Marker','s','Markersize',4);
ylabel('Gradient [-]');
xlabel('RV no.')
set(gca,'YMinorGrid','on')
f1=gca; f2=get(gca,'XLabel'); f3=get(gca,'YLabel'); f4 = get(gca,'Title');
set([f1 f2 f3 f4],'FontSize',20);
%print('-dpng','BuildingBGradient.png')

figure
hold all
Nlines = 100;
Vset = 1:5;
Vg=Xo.getValues('Sname','Vg1');
for i=1:Nlines
    Vg_tmp= Vg((i-1)*length(Vset)+1:i*length(Vset));
    plot(Vset,Vg_tmp);
end
ylabel('Performance function [Pa]');
xlabel('c [-]')
f1=gca; f2=get(gca,'XLabel'); f3=get(gca,'YLabel'); f4 = get(gca,'Title');
set([f1 f2 f3 f4],'FontSize',20);
grid on
%print('-dpng','BuildingBLines.png')



