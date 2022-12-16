C = fscanf(fopen('fractale_3.txt','r'), '%f',[4001,4001]);
X = [-2,2]
Y = [-2,2]
imagesc(X,Y,C)
title('Fractal de Mandelbrot')
ylabel('Im(z)')
xlabel('Re(z)')