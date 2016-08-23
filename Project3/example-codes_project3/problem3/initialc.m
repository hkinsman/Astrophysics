function [t, x] =  initialc(D,mb,mp,ms)
% input valiables: D,mb,mp,ms. D=Rp/Rtidal is the penetration factor. 
% out variables: t and x. Note that x is a vector. 
% Define the initial values of t and x as functions of the input valiables. 
  phase=2*pi;
  f=-acos(-1+D/5);
t = (sqrt(2)/3)*(D^(3/2))*tan(f/2)*(3+(tan(f/2))^2); % initial time
Rt=mb^(1/3);
R=Rt*10;
Xx=R*cos(f);
Xy=R*sin(f);
rdot=(mb^(1/3)/(sqrt(2*D)))*sin(f);
fdot=(sqrt(2)/4)*(D^(-3/2))*(1+cos(f))^2;
xdotx=(rdot*cos(f))-(R*fdot*sin(f));
xdoty=(R*fdot*cos(f))+(sin(f)*rdot);
x(1) = Xx+(ms*cos(phase+pi)); % x : primary star
x(2) = Xy+(ms*sin(phase+pi)); % y
x(3) = xdotx+(-ms*sin(phase+pi)); % dx/dt
x(4) = xdoty+(ms*cos(phase+pi)); % dy/dt
x(5) = Xx+(mp*cos(phase)); % x : secondary star
x(6) = Xy+(mp*sin(phase)); % y
x(7) = xdotx+(-mp*sin(phase)); % dx/dt
x(8) = xdoty+(mp*cos(phase)); % dy/dt
