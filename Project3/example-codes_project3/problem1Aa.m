% example 1Aa: drawing a circle

% When you write code, it is a good practice to add comments 
% that describe the code. Comments allow others to understand 
% your code, and can refresh your memory when you return to it 
% later. Add comments to Octave code using the percent (%) symbol.
% Anytime Octave encounters a percent sign (%), it treats the rest 
% of the line as a comment.  section 26, Octave tutorial

clear        % clear variables and functions from memory
tic
numsteps=2000;
numparticles=100000;
dx=0.05;
binedges=0:dx:1;
for j=1:numparticles
    x(1)=0;
    y(1)=0;
    for i=2:numsteps  % Repeat statements a specific number of times,i.e. 100 times
             % see section 27.1, Octave tutorial
    theta = rand*2*pi; % theta= 2pi/100, 4pi/100, 6pi/100,...
                        % pi: a built-in octave variable, 3.1415....
                        % semicolon suppresses output from octave command
                        % see section 17, Octave tutorial 
    x(i)  = x(i-1)+0.01*cos(theta); % define i-th element of x vector. sec 5, tutorial 
    y(i)  = y(i-1)+0.01*sin(theta); % define i-th element of y vector     
    end
    xfinal(j)=x(numsteps);
    yfinal(j)=y(numsteps);
    rfinal(j)=sqrt(x(numsteps)^2+y(numsteps)^2);
end
%subplot(2,1,1)
%plot(xfinal,yfinal,"+")  % plots vector Y versus vector X. see section 17
%axis equal % sets the aspect ratio so that equal tick mark
           % increments on the x and y axis are equal in size.
%xlabel('X','FontSize',20) % X-axis label
%ylabel('y','FontSize',20) % Y-axis label
counts=histc(rfinal,binedges);
midpoints=binedges+dx/2;
subplot(2,1,2)
plot(midpoints,counts,'ro')
xlim([0 1])                
grid on                    
xlabel('X','FontSize',20)
ylabel('Counts','FontSize',20)
set(gca, 'FontSize', 15)

toc
