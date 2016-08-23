% problem1Ab.m
% First 1000 random numbers are generaged and stored in array X. 
% To produce a histogram, bin ediges are defined by the "binedges" array.
% The elements of the array are 0, 0.2, 0.4, 0.6 0.8 and 1, 
% these idicate the edges of the bins. 
% The "histc" function counts the number of the elements of X that fall 
% in the bins. The elements of "counts" give the number in each bin. 
% "midopoints" is the midpoints/cenetres of the bins. Note that dx is the 
% bin size. By using the tic and toc function, the time taken by the program 
% to execute can be measured. 

tic                 % Start a stopwatch timer.
clear               % clear variables and functions from memory
X  = rand(1,10000); % generate 10000 random numbers between 0 an 1. 
dx = 0.2;           % bin size;

binedges = 0:dx:1;  % define evenly spaced array, start : step : end
                    % see section 6, Octave tutorial
                    % bingedges = 0, 0.2, 0.4, 0.6, 0.8, 1
                    
counts = histc(X,binedges);% counts in each bin
                    % 0-0.2, 0.2-0.4, 0.4-0.6, 0.6-0.8, 0.8-1 and 1-1
                    % the last bin 1-1 is useless. 

midpoints = binedges+dx/2; % midpoints of the bins
                           % 0.1, 0.3, 0.5, 0.7, 0.9, 1.1
                           % the last point 1.1 is meaningless, but 
                           % when we plot the results by using plot below,
                           % the two vectors midpoints and counts should 
                           % have the same length, i.e. 6. so we include
                           % 1.1 in the midpoints. 
                           
plot(midpoints, counts,'ro')% plot counts vs midpoints
xlim([0 1])                 % plot-range from x=0 to 1. 
grid on                     % adds major grid lines to current axes.
xlabel('X','FontSize',20)
ylabel('Counts','FontSize',20)
set(gca, 'FontSize', 15)
toc         % Read the stopwatch timer.
            % tic and toc functions work together to measure elapsed time.
