$PROB Reversible Metabolism
$INPUT ID TIME AMT RATE CMT DV MDV
$DATA reversible-metabolism-nonmem.csv IGNORE=@
$SUBR ADVAN13 TOL6
$MODEL
  COMP (PARENT)
  COMP (METABOLITE)
$PK
  Vc   = THETA(1)*EXP(ETA(1))
  Vm   = THETA(2)
  CLp  = THETA(3)
  CLm  = THETA(4)
  CLd1 = THETA(5)
  CLd2 = THETA(6)
  S1   = Vc
  S2   = Vm

$DES
  Ke1 = CLp/Vc
  Ke2 = CLm/Vm
  K12 = CLd1/Vc
  K21 = CLd2/Vm
  
  DADT(1) = -Ke1*A(1) - K12*A(1) + K21*A(2)*Vc/Vm 
  DADT(2) = -Ke2*A(2) + K12*A(1)*Vm/Vc - K21*A(2)

$ERROR
  Y    = F + F*ERR(1)

$THETA
  (0, 15)
  (0, 3)
  (0, 0.5)
  (0, 0.01)
  (0, 0.003)
  (0, 0.3)

$OMEGA 0 FIX
$SIGMA 0.1
$EST MAX=9999
$COV UNCOND PRINT=ERS

