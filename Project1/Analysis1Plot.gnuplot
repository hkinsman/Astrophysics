reset
#set term png #output terminal and file
set term postscript eps enhanced color
set output "Graph1.eps"
set tics out nomirror

set ylabel "Retrieved Absolute Magnitude"
set xlabel "Retrieved Distance (pc)"
plot "LKRDist.dat" using 6:8 notitle

#set offset graph 0.05,0.05,0.05,0.0

set style fill solid 0.5 #fillstyle
width=200
set boxwidth width
bin(x,width)=width*floor(x/width)+width/2.0
set xlabel "True Distance (pc)"
set ylabel "Distribution"
set output "Graph2.eps"
plot "LKTrueDist.dat" using (bin($2,width)):(1.0) smooth freq with boxes notitle
set xlabel "Retrieved Distance (pc)"
set ylabel "Distribution"
set output "Graph3.eps"
plot "LKRDist.dat" using (bin($6,width)):(1.0) smooth freq with boxes notitle

#n=12
#max=1.2
#min=0
n=40
max=3.0
min=-1.0
width=(max-min)/n
bin(x,width)=width*floor(x/width)+width/2.0
set xrange [min:max]
set yrange [0:]
set xlabel "Retrieved Absolute Magnitude"
set ylabel "Distribution"
set xtics min,(max-min)/5,max
set boxwidth width
set tics out nomirror
set output "Bin1.eps"
plot "Bin1000.dat" using (bin($8,width)):(1.0) smooth freq with boxes notitle

#n=20
#max=2.0
#min=0
n=40
max=3.0
min=-1.0
width=(max-min)/n
bin(x,width)=width*floor(x/width)+width/2.0
set xrange [min:max]
set yrange [0:]
set xlabel "Retrieved Absolute Magnitude"
set ylabel "Distribution"
set xtics min,(max-min)/5,max
set boxwidth width
set tics out nomirror
set output "Bin2.eps"
plot "Bin2000.dat" using (bin($8,width)):(1.0) smooth freq with boxes notitle

n=40
max=3.0
min=-1
width=(max-min)/n
bin(x,width)=width*floor(x/width)+width/2.0
set xrange [min:max]
set yrange [0:]
set xlabel "Retrieved Absolute Magnitude"
set ylabel "Distribution"
set xtics min,(max-min)/5,max
set boxwidth width
set tics out nomirror
set output "Bin3.eps"
plot "Bin3000.dat" using (bin($8,width)):(1.0) smooth freq with boxes notitle

n=40
max=3.0
min=-1.0
width=(max-min)/n
bin(x,width)=width*floor(x/width)+width/2.0
set xrange [min:max]
set yrange [0:]
set xlabel "Retrieved Absolute Magnitude"
set ylabel "Distribution"
set xtics min,(max-min)/5,max
set boxwidth width
set tics out nomirror
set output "Bin4.eps"
plot "Bin4000.dat" using (bin($8,width)):(1.0) smooth freq with boxes notitle

#n=40
#max=3.0
#min=-1.0
#width=(max-min)/n
#bin(x,width)=width*floor(x/width)+width/2.0
#set xrange [min:max]
#set yrange [0:]
#set xlabel "Retrieved Absolute Magnitude"
#set ylabel "Distribution"
#set xtics min,(max-min)/5,max
#set boxwidth width
#set tics out nomirror
#set output "Bin5.png"
#plot "Bin5000.dat" using (bin($8,width)):(1.0) smooth freq with boxes notitle

#n=40
#max=3.0
#min=-1.0
#width=(max-min)/n
#bin(x,width)=width*floor(x/width)+width/2.0
#set xrange [min:max]
#set yrange [0:]
#set xlabel "Retrieved Absolute Magnitude"
#set ylabel "Distribution"
#set xtics min,(max-min)/5,max
#set boxwidth width
#set tics out nomirror
#set output "Bin6.png"
#plot "Bin6000.dat" using (bin($8,width)):(1.0) smooth freq with boxes notitle


