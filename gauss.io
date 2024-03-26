PROGRAM GAUSS 

REAL A(100,100), B(100), L(100,100), U(100,100), SOMMA, DET
INTEGER N

PRINT*, 'DIGITA IL GRADO DEL SISTEMA '
READ*, N 

DO I=1,N
    DO K=1,N 
        PRINT*, 'A(',I,',',K,')='
        READ*, A(I,K)
    ENDDO 
ENDDO 
! matrice base 
! n ordine, i indice di riga, k indice di colonna 


DO I=1,N
    PRINT*, 'B(',I,')='
    READ*, B(I)
ENDDO
! vettore colonna 

DO I=1,N 
    PRINT*,(A(I,K),K=1,N),'|',B(I)
ENDDO 
! la matrice A orlata B ha ordine n+1*n

DO K=I,N-1
    DO I=K+1,N 
        A(I,K)=A(I,K)/A(K,K)
        
! i moltiplicatori sono M(I,K) 
        DO J=K+1,N
            A(I,J)=A(I,J)-A(I,K)*A(K,J)
        ENDDO 
    A(I,N+1)=A(I,N+1)-A(I,K)*A(K,N+1)
! così sono stati modificati gli elementi della matrice 
! così sono stati modificati i termini noti 
    ENDDO
ENDDO

DO I=1,N
    PRINT*, (A(I,J),J=1,N),'|',A(I,N+1)
ENDDO
! così è stata riscritta la matrice la matrice 
! tale matrice possiede i moltiplicatori sotto la diagonale principale 
! tale matrice possiede i "nuovi" elementi sopra la diagonale principale 
! l'ultima colonna di tale matrice possiede i termini noti 

DO I=1,N 
    DO K=1,N 
        L(I,K)=0
        U(I,K)=0
    ENDDO
ENDDO
! definisco le nuove matrici tali che A=L*U

DO I=1,N
    DO K=I,N
        U(I,K)=A(I,K)
    ENDDO
ENDDO 

DO K=1,N-1
    DO I=K+1,N
        L(I,K)=-A(I,K)
    ENDDO
    L(K,K)=1
ENDDO 
! ciò poichè L è una matrice tringolare inferiore con "1" sulla diagonale 

PRINT*, 'LA MATRICE U è: '
DO I=1,N
    PRINT*, (U(I,K),K=1,N)
ENDDO 

PRINT*, 'LA MATRICE L è: '
DO I=1,N
    PRINT*, (L(I,K),K=1,N) 
ENDDO 

DET=1
DO I=1,N
    DET=DET*A(I,I)
ENDDO 
PRINT*, 'DET(A)=',DET 

PRINT*, 'LE SOLUZIONI DEL SISTEMA SONO: '
DO K=N,1,-1
    SOMMA=0
        DO I=K+1,N 
            SOMMA=SOMMA+A(K,I)*A(I,I)
        ENDDO
    A(K,K)=(A(K,N+1)-SOMMA)/A(K,K)
ENDDO

PRINT *,'[',(A(I,I),I=1,N),']'
END 
