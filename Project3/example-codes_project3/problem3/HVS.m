% The disruption of a binary by a massive BH
% the main code
% define mb, mp, ms. These are dimensionless masses. 

clear             % clear all variables and functions from memory
D     = 0.1;        % penetration factor: Rp/Rtidal
h     = 1.d-3;    % time-step size
Ns    = 10000;    % sampling
mb    = 1e6;     % BH mass in units of the binary mass m
mp    = .75;     % primary star mass in units of m
ms    = .25;     % secondary star mass in units of m
[t, x] = initialc(D,mb,mp,ms); % Define the initial values of t and x. 
tmax   = -t;
tprint =  t;
dtp    =  (tmax-t)/Ns;
while t < tmax
     if  t >= tprint
        [Ep,Es] = energy(x,mb,mp,ms);
         v = [t x(1) x(2) x(5) x(6) Ep Es];
         save out v -ascii -append              
         tprint =  tprint + dtp;
     end
     x = RK4(h,t,x,mb,mp,ms);
     t = t+h;
end
