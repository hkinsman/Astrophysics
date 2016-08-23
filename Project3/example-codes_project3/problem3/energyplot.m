clear; clf;
out=load('out'); % load the data file out
t =out(:,1); % time 
xp=out(:,2); % the position of the primary star
yp=out(:,3);
xs=out(:,4); % the position of the secondary star
ys=out(:,5); 
Ep=out(:,6); % the energy of the primary star
Es=out(:,7); % the energy of the secondary star

%plot(xp,yp);
%hold on
%plot(xs-xp, ys-yp, 'r:')
%xlabel('x','Fontsize',20)
%ylabel('y','Fontsize',20)
%set(gca, 'Fontsize', 20)
%axis equal

plot(t,Ep,'o')
hold on
plot(t,Es,'r')
xlabel('t','Fontsize',20)
ylabel('E','Fontsize',20)
set(gca,'Fontsize',20)

