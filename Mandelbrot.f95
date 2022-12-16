!*******************************************************************************
!
! Année :2020/2021
!
! Auteur: Laboureur Guillaume
!
!*******************************************************************************

program third

complex :: c = (-2.001,2.001), zzero = (0,0)
INTEGER :: i,j,k,m,o
complex, dimension(27) :: zi
complex, dimension(4001,4001) :: Matrice_c
INTEGER, dimension(4001,4001) :: Matrice_k



do k = 1,27
    zi(k) =  5
end do
do i = 1,4001
    do j = 1,4001
        Matrice_k(i,j) = 28
    end do
end do 

!----------------------------------------------
! Création de la matrice c contenant tous les c
!----------------------------------------------
do i = 1, 4001
    c = c - (0,0.001)
        do j = 1,4001
        c = c + (0.001,0) 
            
        Matrice_c(i,j) = c                      

    end do
    c = c - (0.001,0) *4001       
end do




!--------------------------------------------------------------
! Création de la matrice contenant les k maximum pour chaque c
!--------------------------------------------------------------

! Pour chaque valeur de c, on calcule les zk tant que |zk| <= 2 et on en déduit le k maximum
    
do i = 1, 4001
    do j= 1,4001

        ! Calcul des zi et mise des zi dans la matrice du même nom si |zi| <= 2
        !----------------------------------------------------------------------
        do k = 1, 26  
            zi(1) = zzero
            If (k /= 1) Then 
                do m = 1,(k-1)
                    If ( CABS(zi(m)) <= 2) Then 

                        zi(m+1) = zi(m)**2 + Matrice_c(i,j)

                    end If
                end do
            end If                             
        end do 

        ! Calcul de la matrice contenant le k maximum pour chaque c
        !-----------------------------------------------------------
        do k=1,27
            If (zi(k) == 5) Then
                
                If (Matrice_k(i,j)>k) Then
                    Matrice_k(i,j) = k-1
                end if

            end if 
        end do

        ! Réinitialisation de la matrice zi
        !----------------------------------
        do k = 1,26
            zi(k) = 5
        end do

    end do
end do 

open(unit=1,FILE='fractale_3.txt')
write(1,*)  Matrice_k
close(1)

! Programme Matlab:
!
! C = fscanf(fopen('fractale_3.txt','r'), '%f',[4001,4001]);
! X = [-2,2]
! Y = [-2,2]
! imagesc(X,Y,C)
! title('Fractal de Mandelbrot')
! ylabel('Im(z)')
! xlabel('Re(z)')


end program  third




















