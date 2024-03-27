! IMPLEMENTAZIONE DEL METODO DI THOMAS

PROGRAM THOMAS
REAL A(10,10), M 
INTEGER N, COUNT
N=4

DO I=1,N 
    DO J=1,N 
        A(I,J)=0
    END DO 
END DO 

! INSERIMENTO ELEMENTI DELLA MATRICE TRIDIAGONALE
DO I=1,N 
    COUNT=I-1
        DO J=COUNT,I+1
            IF ((J.NE.0).AND.(J.LE.N+1)) THEN 
                PRINT*, 'A(',I,',',J,')='
                READ*, A(I,J)
            END IF 
        END DO 
END DO 

! STAMPA DELLA MATRICE 

DO I=1,N 
    PRINT*, (A(I,J),J=1,N)
END DO 

! METODO 

DO J=1,N-1
    M=A(J+1,J)/A(J,J)
    DO I=J,J+1
        A(J+1,I)=A(J+1,I)-M*A(J,I)
    END DO 
END DO 

DO I=1,N 
    PRINT*, (A(I,J),J=1,N)
END DO 

END  
