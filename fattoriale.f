PROGRAM FATTORIALE

INTEGER N, FATT
FATT=1
PRINT *, 'DIGITA UN NUMERO'
READ (*,*) N
DO I=1,N
FATT=FATT*I
PRINT *, I
PRINT *, 'IL FATTORIALE è ', FATT
END DO
END 


PROGRAM FATTORIALE

INTEGER N
COMPLEX FATT
FATT=1
PRINT *, 'DIGITA UN NUMERO'
READ (*,*) N
DO I=1,N
FATT=FATT*I
PRINT *, I
PRINT *, 'IL FATTORIALE è ', FATT
END DO
END 
