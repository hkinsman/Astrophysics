% this is the main code to estimate the binary evolution
% two functions: energy.m and RK4.m are used in this code.
% See section 24, Octave tutorial for Function m-files
% Define mp, ms, x(1)-x(8) and tmax

clear;clf          
h      = 1.d-2;  % time-step size
Ns     = 100  ;  % sampling
%-----------------
mp    =  .75 ;  % primary mass
ms    =  .25 ;  % secondary mass
x(1)  = ms*cos(pi);  % primary x
x(2)  = ms*sin(pi);  % primary y
x(3)  = -ms*sin(pi);  % primary vx
x(4)  = ms*cos(pi);  % primary vy
x(5)  =  mp*cos(0);  % secondary x
x(6)  =  mp*sin(0);  % secondary y
x(7)  =  -mp*sin(0);  % secondary vx
x(8)  = mp*cos(0);  % secondary vy
tmax  =  6*pi;  % final time
%-----------------
t      =  0  ;  % initial time
tprint =  t;
dtp    =  (tmax-t)/Ns;
while t < tmax
     if  t >= tprint
         E = energy(x,mp,ms);       % Estimat binary energy
         v = [t x(1) x(2) x(5) x(6) E];
         save out v -ascii -append  % save the v vector in the file out           
         tprint =  tprint + dtp;
     end
     x = RK4(h,t,x,mp,ms); % Runge-Kutta method 
     t = t+h;
end
