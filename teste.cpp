aJpina2
DECLARE FUNCTION Ace rta S cxx ! DECLARE SUB ALTAGUA (g
D EC LA R E SU B AR EAMIN É
D EC LARE SU B AR ENTRADA Ę g D EC LAR E S U B AR SA I DA (
DE C LAR E S U B CA LCU LA8 9 EMž' ( Û D ECLAR E SU B CALCU LATORR E  ( D ECLARE   SU B   CALC U LAVAP   ( ) DE CLAPE SUB DADOSMOD E LO g
DECLARE   FUNcTIoN Den s á d ade  !	( L9 ! , L 6 ! ,  P7 !
D ECLARE SUB ENCHI MENTO (} OECLARE SUB ENCHOlt {Ş
DEC LARE FUNCTION Ental p1 a ! ¢ L 9 ! , L 3 ! DECLARE SUB LEDADOS §}
D ECLA R E SUB L ETOR R E {} D ECLAR E S U B L EVA P ( g
D ECLA R E S U B M ER KEL ENCH Ê ğ D ECLAR E S UB MER K EL PRO {§ DEC LARE SU B CODZIN g
D ECLA R E SUB NCNÌ ZN
0 ECLARE FUNCTION   P r e set m !   ¢E 2 0 ! D ECLA R E SU B PRES EST ¢)

D ECLAR E  FUNCTION  P r e s Pa rvap !   ( P4
D ECLAR E FUNCTI oN P r e svap s a t !  C P8 D EC LA R E S U B PROP A R (
UE C LA R E S U B S AI FIC HA (g
DEC LA RE SUB +emBs L L8 ! , L 7 ! , L 6 ! DEC LARE S UB TemBU ( L9 ! , L 7 ! , L 6 ! DECÌ.AR E SU B TEL PM $Ş

u9 ! ,  I.8 ! ,  L6 !

DEC LARE FUNC+ION Teo rAg ua ! ( P 7 ! , L 6 ! ) D EC LARE SUB TOL {}
DECLARE FUNCTION UR ! (P3 ! ,	7 !
D ECLARE S UB U SUA L ()
D EC L,ARE S UB VEN*I LADOR {) D EC LARE S UB E ES ENHA {}
UEC LARE SU B ca1 c u1 acs	(}
D EC LARE SUB  nos  t ra En cont rado  (}
' %F0RM Imprime
’ S FORN mo st ra
’ S PORM Ent ra00l  ’ $ FORM	be rt u ra
S FORM No st ra0
’ $ FORM  f rmH el pua1n
' S "ORM I rn1H el put 11 s SINCLUDE:	'selecao.bi @INCLUDE:		help.bi
' SINCLUDE :	' con s t an t . b1 '
' CHDRIV E "g : ”
’ CHOIR " g : ENGG PROGRAMAN PAD‹JOS ”

' R E DZM V8 E 3 ( 26,	66)	AS SING i . E , v9 E TO 26, 60Ï AS SINGLE , V9E 8 ( 2 l , 3?	s S	c L r . V9E § ( 18 , 60J AS SI NGLE
Ł7IN \‘A P Ț2 , d	AS SING L E
l3 i f/ F.NC L ó)	AS SI NGL E

DI II ,\A I ( 7 ,  2	AS SING L E , 4S SINGL E	’ LETORR E
UT Y S 2 5 , 2 TJ AS S Z NGL E DIM T 2 5,	2 7	AS STRING
D Nì  SHA R ED N CM  Ž Ș ,  Ț )
*i Jenum	= S

AA 3 6 , 4	AS S ING LE ,  AA 4  T 2 ,  4 3 )  A ? S I NG L E ,  PI Lß? Ę I›	7



INPUT #f4 lenum%, num%
SCk E EN . co nt rod Pan e1 ¢ T%} -	n urns



Pńg1r a 1

alpina2
S E LECT  CASE  I%
CAS E  9
Fundo 3 an e1 a = n urr%
CASE 10
FrenteJanela = num%
END 5ELECT
NEXT I%
CLOSE fi]enum%

He1 pReg i s te r    "aj  uda .EXT " ,   He1 pLoaded% I F He1 pLoa ded% = FALSE THEN
Ent ra001. cmdAj uda . Enab1 ed =  0
HelpSetOptions Fundolanela, Frentelanela, Fundolane4a, FrenteJanela, FundoJanela, 11, 6S
END	I F


’ CONST TRUE = — 1 CONST NUf4TOR = 2 5
CONST   PI   =   3 . 14J 5 9
CONST DE S VIOSUP =  2 CONST DESVIOINF = - 1. 5 CONST PRA FORA = — 1
DES	ENV = 0 LIMITECHUVA = 2l LIMITECHUVAINF =
LES ENHA
Ab e rt ura . SHou
L E DADOS
En t ra%  =  $
STATIC FUNCTION  Ace rta $  (xxv
X = INT (Xx	*   10   +   . 1)	/ 10
+emp 8 = sTR S (x}
Temp S   -    LTRIM$ (Temp S} Temp $ = RTRIM$ CTemp S}
X - INSTR(I, Temp$, ".")
I F    {X    =   0)	AND    ( L EN (Telf S	<  5 )    TH EN
Temp S =   Temp $   •
END   I F
IF (INSTR(1, Temp$, ”. ”) = 1) THEN
Temp% - "0"	rempB
ELSEIF (INSTR(1, Temp$ ,  " — . "	=   1J   THEN
Valor = LEN(Temp$)
Temp $ = " — 0.  "  •  NIID $ (Temp S	3	Va I o r	2
END  I F	'	'
Ac e rua $   -    Temp$

END
SUB

FUNCTION
ALTAGUA  (g
H0   -    0
S E LECT CASE  S(M , 17 CAS E 1
I3 - 1
DO

IE AA1(I3, 1) = S(M, 1) CHEN HO = AAI(I3, 2j
EXIT  DO E ND I F
Z F    H O   <>    O TI-|E N
EXIT DO
END  I F
1 3   -    1 3 +   1
LOOP
IF H0 = 0 THEN
PRINT  ” ERRO  NA D  ET  E REI INACAO   DA  ALTU RA  DE   ENTRADA   DE   AGUA"
PRINT " {CASEU  "
Pág n a 2


STOP
END  I F
CAS E  3
I3 = 1
DO

at pJ na2

I F   AA3    13 ,    1)	— S {N ,   1)	THEN
HO = AA3(13, 6 - E13) EXIT DO
END  I F
I F H0 <>  0  THEN EX IT    DO
END    I F
13    =   13    + 1
LOOP
z T  H0   =   0   TH EN
PRINT "ERRO  NA  DET  E RR I  NACAO  DA  A LTURA  DE  E NTRADA  DE  AGUA " PRINT " {CASE3 "
STOP
END IF CASE 4
I3 = l
I F ( ENCH = "A19" ) OR ( ENCH = "A 12 " }   TH EN
NC0 = 5
E L S E I F	ENCH   =    " SG "	OR   ( ENCH   =    " > 20 " }    TH EN
NC0 — 0

E L S E

NC0	10

END I F
DO
I F   AA4    13 ,   IQ	=   5 {M ,   1}	THE N
IF ENCH <> "RT" THEN
H0   =   AA4 {I 3 ,  NC0   +   NC   *   1	14 * (4   — E 13 E L SE
H0 —   AA4 ( 13 ,   NC0 •	NC	14	/   4	2   •   T4    ’	{8









LOOP

E ND I F
EXIT DO
END I F
I F H0 <> 0 TH EN EXIT DO
END   I  F
13 = I3	1

I F H0  =  0  THEN
PRI NT " ERRO NA PRINT " {CASE4 " STOP
END I F


DETERMINACAO   DA    A LTURA    DE    ENTRADA    DE    AGUA ’ ’


" ** Altura dos Pilaretes
I * S {Y ,   18)	=   2   THEN
I F  ( (  EN  C H    =  "  S G "  )	OR	E N CH	=	" >2    0 ”	A ND	N C    =	5     TH  E N

E L S E
NC Pi l    — 0
END	Z F
Z 3 = 1
DO
IF PILAR(I3, 1) - S(M, 1) THEN
PILARETE : PILAR(I), 3 ' NCPil	(4 - El3j	l}


PILAR(I3, 6 - E13))


END    I F
I K  Ent ra00T . Opt Ba cJ c (2	. va 1 ue  -  TRUE  THEN
PI LARETE  =  PI LARETE  +  1 . 3 5
EN 0 I F
EXIT DO

Pâg na 3




LOOP
END I F


END IF
I3 = I3	1

aJp4na2

’ ” * "  COR R ECAO DA ALTURA PARA O CASO DE TORRE TI PO — B E — "
I F S (M , 18a = 2 ANO Ent ra 001. Opt Ba c a (2 . Va1ue = TRUE THEN H 0 = H0 •	. 3
END I F

CAS E EL S E
PRINT " ERR0 NA D ETERMINA CAO DA A LTURA DE ENT RADA DE AGUA "
PRINT " (CAS E 5) “ STOP
END S EL ECT
END SUB

SU B AR EAMIN ( )
' CALCU LA A AR EA NINI LA POR C ELU LA
'VARIAvEIS DE ENTRADA: E1=VAZAO DE AGUA TOTAL
'	E T §=NUN!E RO D E CE LULAS
SELECT CAS E ENCH
CASE "A19"
AFÍN = E1 / EL § /  LI MI T ECHUVA
CMIN = l
CMAX —— S

CAS E


CAS E

" >20 " CMIN = l CMAX = 5
AMIN = E1 / El5 / LIMITECHUvA "SG"
CMIN : 1


CAS E


CAS E

CMAX = 5
AMIN = El / E15 / LIMITECHUvA
"RT"
CMIN - 1
CNAX - 3
AMIN = El / £1Ş / LIMITECHUvA
"A 12 "
CMIN = 2
CN!AX = S
ALI N = E1 / E15  /  L I MI TECHUVA

END


S U B

END S E LECT



SUB

















END

AR ENTRADA	)
IF E6 — 0 THEN
TemBs E8 , E 7,	E9 E6 = L 9
E LSEZ F E8 = 0 THEN TemBU E6, E 7,	E9 E8 = L8
END IF n9 = E6 L8 = E8 L6 - E9 L5 = E20 PROPAR E6 = L9 E8 = L8 E7 - L7
n9 - c6
Hl = u4 El = L3 R1 = L2 SUB


'PROPRIEDADES DO AR NA ENTRADA









TBS TB U UR
PR ES SAO  A^MOS FE RICA
ENTALPIA
’	TEOR D E AGUA DEN SI DAD E


SUB AR SA I DA ( )
’ D ET E RMI N A AS PRO PRI EDADE S DO AR NA SAI DA E CA LCU LA LAN BDA
Pág ' n a 4

alplna2
'CALCULO ITERATIVO DA DENSIDADE NEDIA DO AR
R3 = R1	'ADMITE-SE DENSIDADE SAIDA=DENS. ENTRADA 'DO
L = Vaz ° R3 / (G0 / 3.6J	'LAMBDA ESTIMADO COM BASE EM R3
H2 = H1 + E4 * 4.1868 / L	'ENTAIPIA DO AR NA SAIDA, CORRESPONDENTE
K9   =   H2
L 8 = E 8
TEM P 1

L9 = K8 L8 = K8 PRO PAR
R 2  =   L 2
R 4   =    ( RI    +   R 2      /   2
Tl = E3

TBS DO AR NA SAIDA ' TBU DO AR NA SAIDA

' D EN SI DAD E N!EDIA DO AR















END

SUB

Re vJ s ao : 2
De s c rJ cao : Ret1 ra da a conde cf on al	da ne c e s s1 da de da d fe ren ca en t re
den sl dades
PO r  :  C3 M	Data   :   16 . 12 . 94
’ I F (AB S (R 3 - R4}	>=	. 0001}   +HEN	—*"
R 3 —  R4
’	EXIT  DO
’ E ND I F
’ ml e r c asr sa da = — 1
' EXIT DO
,' LOO.P. . ..,. . . . .,,. ,. . , . „	, . .,,.   .,.. ,.	,
SUB

CALCULA89EM2 ()
'Alerta = 0
' REvel ho = RE
ANvelho = AN
A]ertaventilador = 0
AL = 3.14159 / 4 * (S(M, 7) ^ 2 - (S(M, 7) - 2 ' S(M, 8)) ^ 2)	'AREA LIVRE
s = (v5 / AL) * 2 ' R5 / 2 / 9.807 / D4 'SIGMA
UA = ROTVEN / 60 * 3.14159 ’ S(M, 7)	'VELOCIDADE PERIFERICA
F : V§ / AL / UA	'PHI
PSI = F	2 / S	'PSI=PHI^2/SIGMA PSI1 = INT((PSI0 - PSI) / ABS(DPSI)) + 1
'PSI1 = INT(ABS(PSIO - PSI) / ABS(DPSIj) + 1 PSA = PSIO + (PSI1 - 1) " DPSI
PS B = PSA + D PSI
FI2	= I NT	F — PHIOF   /   DPH13 + 1)
I F FI2	< 1 OR 2 “ FI2   + 2 > N?OL OR PSI I   < 1 OR PSI 1 + 1 > NLIN	TH EN RE1 - 0
E L S E

R E1    V PSI 1 , 2 " AN T V Ç PSI1, 2 " RE2   V PSI1 • 1 , AN 2       V   P 511    *    1 , RE 3     V P S 1 1. ,  ?   ”’
AN  3	V    PS 1 1 ,    ?     “” RE4	V ( PSI 1 + 1, AN4	V   PS T1   •   1 ,
RE 5	RE 1 — RE 2)	“  A BS  ( PSI	-   PS B}	/   DPSI	+   V ( PS 11	1,	2   “	1 2

R E 6	R E 3	R E 4	’  A BS  Ç P SI	P S B§  /  UF’3I	V PS I I *	1 ,   2	z 7
1)
A N 5	(ANI	AN 2	AB S Ç PS I	PS B}  /  DPS I)	V PS I T + 1 , 2	FI 2 AN 6	(AN 3 — AN4}	AB S C Ê PS I	PS B	/  DPS I }	V PS I I	+ I ,   2	P I ? •

END	IF
RE	= ( RE6	R E §	K	PHI0  •  ( Fi 2	1}	D PHI)	/ DPHI	R E 5
AN	— JAN 6	AN 5)	Ç F	PH10 + ( FI 2	1)	OPHI	/  D PHI *	AN §
Pãgina S


I F RE	0 THEN RE = 'i0

alplna2


IF	{RE1  =  0  OR  RE2  =  0  OR  RE3   =  0  OR  RE4  =  0)   THEN
if (w ‹> 2.25) And (W <> 2.8J rhen
'	RE = REVelho
'	AN = ANVelho
AlertaVentilador = -1
' Re vi sao : 1
' Des c r1 cao : B1oco que t rata e c ro po r pon to   fo ra do gráfl co
Por : CJM Data : 21.11.94
Copiado da versao da MIDWEST em 05.12.94
IF Pr1mvezFo ra = 0 THEN
PMI nE = 0 : Pmi nD = 0 : Fmi nB = 0: Fm1 nC = 0
FOR I = 1 TO NLIN
OF Pms nE = 0 AND V (I , 1)	<> 0 THEN P«ri nE = I
IF Pmi nD = 0 AND V(NLIN — I	+ 1,   NCOL -   1)	<> 0 WHEN Pm1 no =

NLIN	I   + 1


Z F PIti1 nE <> 0 AND Pm1 nD <> 0 THEN EXIT FOR
NEXT I
FOR 3 = 1 TO NCOL / 2
IF  Fmi nC = 0 AND V {1,	2 “ 3 • 1}	<> 0 THEN Fm1 nc = 3
IF	Fmi nB = 0 AND V(NLIN , 2 *	- 1)	<> 0 THEN Fmi nB = 3
IF Fminc <› 0 AND FminB <> 0 THEN EXIT FOR

NEKT  3
Pré mvezFo ra = - 1

ELS E
IF SegVezFora = 0 THEN
segVez Ko ra = - 1
RE = 60
ELSEIF Te rVezFo ra = 0 THEN
TerVez Fo ra = — 1 RE = 45
ELSEI F quavez Fo ra = 0 THEN
QuaVez Fo ra = - 1 RE = 40
ELS W F Qu a Vez Fo ra = 0 THEN
qu1vezFora = — 1
RE = 5 5

ELSE

msgs = ” Nao houve conv e rgenc1a no grafl co do vento 1ado r para

encontrar um ponto no grafico onde a potencia consumida esta 10 % abaixo da
potencia de placa do motor”
Nodel o1$ = "To r re   Nodel o   :   " + MODELO
Z F PSI1 <= Pmá n E AND FI2	<= Fm1nC THEN
ms g1$ = " A pe rda de pres s ao es tate ca total e mu to	a1ta ou a vazao de ar e muito baixa (srande Psi). Experimente diminuir a area livre do ventilador.
ELS EI F PSI I   <-	Pm1 nD AND FI2	>= Fm1 nC THEN
msgll = " O angulo da pas e muito alto (Grande Phi).
Experimente usar um ventilador com mais pas or aumente a rotacao do mesmo.
ELSEIF PSI1 ›= PminE AND FI2 ‹= FminB THEN

Expe r1 rrente
oesmo .

msgll = ” O angulo das pas e muito baixo (Pequeno Phi)
um ventilador com menor numero de pas or diminua a rotacao do
E LS EZ F   PSI1  >= Pm1 no    AND   FZ2	>=  Fm1 nB  THEN
msg1$  =  "  o  rend  vento  do  vent1l ado r   e  mu1 t o  bai  xo

( Pequeno  Ps1) .  Experl mente  aumenta r  a  area  1 1 vre	do venta 1 ador . "
END I F
ZK   A1 gunna   Vez   Den t ro    =    0   TH EL
IF	U1 t 1 rnON\/ < MOT /   1.  1 THEN
Novouode 1 o = - I
L = UltimoL w = ultimoW RE = U1 tJ moRE AN = U1 t1 JOAN
NF = UltimONU
Pâg; na 6

al pJ na2
V 5 =  UI t á m0V 5
D4 = UltimoD4 Rl = UltimoRS
msgs = msg$ * " Assim, usaremos o ultimo ponto que esta dentro do grafico. ” i msgl$
'Ir Terminando% = 0 THEN PRINT (msg%), 48, ModelolS
ELSE
No v ovode1o = —2
ms g S = ms gS + " O u1 t mo pon t o que e s t a dent ro do
grafico apresenta uma potencia consumida maior que o aceitavel.” + msgl$
' I  F se rmj n ando% -  0 +HEN PR I N+ (ms g $) , 48,	rod et o I S
END I F
E L S E
NovoModelo - —2
msg% = msg$ + msgl%
IF Terminando% = 0 THEN PRINT (msglJ, 48, YodeJolS



ELSE

END I F END I F
END I F

IF UltimoNV r MOT / 1. 1 OR Ul timoNV < v5 * D4 / 7S / RE / ET " 100 THEN
UJtimoW = W Ul timOL = L UltimoRE = RE UltímOAN = AN
UltimONV = V5 * D4 / 7S / RE / ET " 100
ultjmOV§ = V5 Ult4mOD4 = D4 U4timORS = RS
AlgumavezOentro	0


END SUB


5 U B CA L CULATOR RE (
’ VAR IAVE Z S QUE USA : E9=
’	H0=
'	ENCH=
E13=
F1—
'	R1=
R2=
R3=
’	R4=
'	TOT=
'	+T=
K8=
vaz—
N = E10
WAu tü 9° = *
Vaz = S(M, 3)	w
C ?LU LA
DADOS COD E LO
AR ENTRADA
r nvez Fo ra = 0 segvez Fo ra = 0 Te rve z Ko na — 0 qu ave z no ra — 0 Qu1 vez Fo ra = 0
ml guma ve zDent do = — I NovoMod eI o = 0
13  =  E18	1
















' VE LOCI DA D E M EDI A DO A R NA *tJPII E  L i›\,	)

'VAZAO VOLUMETRICA MEDIA DE AR POD

'DETERMINA DADOS CONSTRUTIVOS NODELO
' PROPRI EDAD ES DO AR NA ENT RADA

IF T(M, I3j = ”8EM2” OR T(N, 13)	”9EM2” *HEN
ArquivolB = arquivos
Pág1na 7


a rqu	vo S   =   T (M ,   13	+     LTRIM$ ( STR$ {N PÇ )    +  " . DAT"
IF A rqul vo1$ <> arqu voS THEN OPEN ”I " , # S , a rqu i vo S
IN PUT   4 5 ,   NLI  N , NCOL ,   PSI0 ,  OPS I ,  PH 10 ,  DPH I
REDIM V(N L I N , NCOL}  AS  SING L E FOR   I3    = 1 TO hl LI N
FOR  J  =  I   TO  NCOL
I NPUT    # 5 , V ( 13   ,   1 )
NEXT  3
NEXT 13 CLOS E #5
END I F
END  I F
ch 1 co =  0
OO




C E L U LA

ch   co  =  ch1 co  •   1
I F Ch1 co > 1 THEN Ca1 c u aNS
Va z = S (M ,  3)	*' N



'vAZAO VOLUMETRICA MEDIA DE AR POR

U1   =   Vaz	/ F 1

I e rtaAr Sal da -   0
I F N <> 0  THEN AR SAIDA

’ VE    LOCIDADE   M EDI     A    DO     AR	NA	ENTRADA    DA   TORRE



'PROPRIEDADES DO AR NA

SAI DA/ LAMBDA
END I F
I F FAI e rt aA rsal da —  - 1)	THEN
' PRINT (" E r ro	no a r de s a1 da ") , 48, PodeI o1S
EXIT S UB
END I F

I F S (M ,  2)	= 1 THEN
RS = R1

'DENSIDADE DO AR NO VEN*lLAOOR

E L S E
R 5   =   R 2

END IF
PR E S EST
V $ -	Vaz	" R 4 / R 5
VENTILADOR


' P E RDA    D E   P R E S SAO    E S +ATICA   NA TORRE

IF (T(M, 13) = ”VAP”) OR (T(M, I3) = ”VAL”) THEN
IF ABS((SD	04) / D4) <= .001 AND (Alertaventi lador = O) THEN
EXIT DO
E LS E I F No v ovo de 1 o =  —2 TH EN
'print (”Falta de convergencia no vent4lador”),48,xodelolS
EXIT SUB
END IF


TH EN

E L S E

IF	§ABS (1	( (TOT / 1. 1}   / NV§	<= . 003) UND A1 er t avent 1 ach r	U
EXIT DO
ELSEIF NovoMoclelo = -1 THEN
EXI* DO
ELSEIF NovoModelo = -Z THEN
' p r nt	( " Fa1 a de con ve rg en cf a no vent 1 ad o “ ’ ' ) , d 9,	Moll e1 o:! î
EXIT S UB
ELS EI * f Ch CO >   2 O§   THEN
’ PRINT ( " Nao con segu u conve rg1 r . Loo p t   po	n de t e ï» na rJo ‘
EXIT SUB
END I F

EN0 I F
LOOP
M E RK E L PRO ME Rlt E L E NCH
D9 — AB S K -  K 3
D - K * 100	/	L <0 ‘ L	A 5
TOL
FJ s ca0 = u7
I P L 1 e rt axe r ken P ro	— - 1}	TH EN
' PRINT	" E r r o no ire ‹ ke1	”	,   48,   code o1S
Pâg n a 8


EXIT SUB END IF
IF S(M, 1) <  100 THEN
I F E 5 - E8 >= 5 . § THEN
De s v1 o0 — DESVIOIN F — 2 De s vJ o1 = DESVJ:OSUP * 6 ELSE

alpina2




E LS E


oe s vt o0 = DES VIOINF — 1
De sv1 o1 = D ESVIOSUP	1
E LS E

EN0

END IF
I F

De s vi o0	DES VIOINF	1 * ( E ñ -  E8	/   5 . 5
Des v 1 o1	DESVIOSUP	1

IF ((Te rmi nando%  =   0J   AND   ( (u 7  <  De s v  o0}   OR   (U 7  >   De s v1 o1}	THEN ' PRINT   ( "De s vJ o  fo ra   da   faz  xa" ) , 48,	no de J o1S
EXIT SUB
ELSE
ESTE  =  - 1
Es t eEI5 3 a =  — 1
S E L	=	S E L    +	1
I F	PRA	FORA =  — 1§	HEN
I P ( E 3 —  E 5}  >=  6  THEN
capabilidade = .85

E L S E

END 1

Capabilidade = .9

u7 = U7 -  {E 5 * US — E8J "  ¢1 / Capab1 11 dade	1J
END I K
I F	S E L = 1}	AND ¢Te <mñ n ando% — 0}	HEN
OP E N " 0 " ,  # 2 , " S E L ECOES . TRIP "
mostra.Label9.visible = -1 mo st ra . Label NO. \/1 s ñ bJ e = — 1 mo s t ra . Label II . Vi s ñ b1 e - — 1
END   I  F
I *  (Te  rma nando%  =  0)   THEN
PRI   NT    Á2 ,  S E L ;   " , ’ ’ ;   MO0E LO ;    " , " ,  E1 ;   " , " ,  E 3 ;    ” , ” ;   E S ;   " , "

L8	E13 ;   “ , " ;   E16 ;   " , " ;   E20    ; " , " ;   U7 ;   " , " " ; FI S1 ca0 ; " , " ; EN
Nos t raEncont rado
ELSEIF (Terminando% = -1) THEN
I F	PRA FORA = — 1}	TH EN
I F	E 3   -   E S	>=  6    THEN
Capabi4 idade = .8S

 ;   NC ;   " , "

ELSE
Capabilidade = .9

END IFC
END	I F

(ES + U7 - E8) * (1	capabi44dade— lJ
, ¿ 1	.,




E LS E

PR I NT  #4 ,  DODE LO ;  "
PRI  N^	#4 ,  ” , " ; E T3 ;
PRINT #4 , NC ; " , " ; SAI FICHA


”’ E16’  ' '' ' '' ’ MOT

msg5 = s RB(7erminando/J
PRIN	ms g S
STOP E ND I F
END I F
END SUB


S U B    CA L C U LAVA P


Pay na   9


A1 e rt avent11ado r   = 0
I4 = E18 + 18

alp4na2


END

SUB

SD = V 5 ^ 3 * VAP (S {M , 14} , 2} + V S ^ 2 * VA P S (M , 14) , 3 }
SD = SD + V5 ” VAP {S (M , 14} , 4} • VA P S {M , 14} , S S UB
Calculaws ()
Ij = El8	1
I F   (T {M ,   I3)	= " 9Ef42 " )  OR (T (M , 13)	= " 8 EM2 "	TH E N
' r ev 3 . 0
01.07 . 96 po r . cj m
alterar o expoente da formula de convergenc4a para 1/3.5
'para convergi mais râpido
US = W ”	(MOT /I	. 1}	/   NV)   A (1	/	3 . 5)
E LS EI F  ¿T QM ,  13	=   " VA P"	OR   {T  (f4 ,   I s	=   " VAL "	TH EN
I F    $\/ 5   >-	VAP §S (M ,   13	+ 17}	,   7} }    TH EN
WS = N ”  {SD	/ D4)	^ ( 1 /  5

E LS E


US — 0
Nov oMode 1 o = - 2






END







SUB

END I F END I K
I F   (ABS {\./S   — wAn11 go}	<   . 001)   TF!EN
wS = (WS + WAntigo) / 2
ENO IF
uAn t1 go = w
U = (as • wAn t go}	/ 2


SUB














MO*

carregaMostra0 ()
REDIM SeJec0(100, 16J AS SINGLE, Selec1(100) AS STRING
Ir (Mostra0.lstLncontrado.LJstCount, 0) THEN Total = Hostra0.JsiEncontrado.LisiCounl FOR 13 = 1 TO Total
no s t ra0 . 1s t En cont rado . R EMOVEITEM 0
N EXT 13
E ND I F
no s t ra0 . Label 1. Capt ñ on - ace rta $ (EIQ Mo st ra0 . Label 2 . Capt on = ace rta S (E3) los t ra0 . Lab e13 . Capt on - Ac e r tal ¢E 'i no s t ra0 . Label 4 . Capt on = xc e rt a S ( E8 } 13 = 0
OPEN " I ” , #2 , " S EL ECOES . TO P " DO EHI L E NOT EOF ( 2
I N PUT #2 ,  S EL , CODE LO ,  E1 ,  E 3 ,  E 5 ,  E 7 ,  E 8 ,  E I3 ,  E16 ,  E2 0,	U 7 , N , 0 c ,   > , S PL , ZFñ s 1 c a , EN
I3 = 13   1
Selec0(13, 1) = SEL
Selec0(I3, 2J = El Selec0(I3, 3) = E3 Selec0(13, 4) = ES Selec0(I3, 3J = E7
selec0(13, 6) = E8 Selec0(I3, 7) = EIS Selec0(13, 8J = E16 SeJec0(I3, 9J = E20 Selec0(13, 10J - U7 Selec0(I3, 11) = M Selec0(13, 12) = NC
Selec0(13, 13) - &l0 se9ecOCI3, 14) = voi Selec0(13, lS) = SPL Selec0(I3, 16J - EN Selec1(I3J = YODELO
CLO5 E #2
13 = 1
DO

Pâ g1 na TO

at pi na2
I F   I3	S E L   THEN
EXIT DO
END I F
I F   (S e1 ec0 (I 3	1,	10)	>   se 1 ec0 (13 , 10a )	THEN
FOR 2 % = 1 TO 16
SOAP S e1 ec 0 (I 3,  u%) ,  se1  ec0 (I 3 + 1,	3 %}
N EXT  3 %
sWAP Se1ec1(I3J, Selecl(13 * 1)
I3 = 1
EL SE
13	13 + 1
END I F
LOOP
FOR  13  =  1  TO  SEL

ms gS = ms g 8 + ace rt a S (Se1 ec0 (I 3,  3 }  S e1 ec0 (I 3 , 10} ) ms g S = ms g$ + Ace rt a S (Se l ec0 (I 3,  4}    s e ec 0 {I 3 , 10) ) ms g S = msg S + ace rt aS (Set e c 0 (z 3, 6) ) +
I F (Se1 ec0 (I 3 , 10a < 0J THE N
ms g S = ms gS + xc e rt a S (S e1 ec0 C 13 , ION

E LS E

m s g S   -   ms g $ •   " • "   + Ace rt a$ (se1 ec0(13 ,   10a

END I F
E15   =   INSTR CS e1 e c 1 ¢13   ,    " — "
EI 5 = VAL (LEFTS (S e1 e c 1 L z 3 ,  ( EJ. 'i   -   1J
ms g S   -    ms g S   +   SPACES ( 3	•   STR $  EN	" x"  + STR A (se   l ec0 ¢1 3,   14) ) ms g S   =   m s g$  +   SPACES (6   -   LEN(STR$ (Set ec0C 13,	14$ } }
ms g S =   ms g S   +   STR$ (S e1 ec0 (Z 3,	15 } } Mo s t ra0 . 1 st E ncont rado . ADDITEM MS 98
N EXT	13
END SUB
SUB CENTRAL {g
' VAR IAV EI S D E ENTRADA :  E2 1= '	E 2 2—


'vARIAvEIS QUE USA: AMIN=AREA MINIMA TRANSVERSAL POR CELULA
'	E15=NUMERO DE CE LU LAS
’		c0=VAZAO DE AGU A POR CE LULA (m3/h R=INTENSIDAD E D E PR ECI PITACAO {m3/ (h . n2 )

S E L = 0
I F  E 9 = 0 TH EN
E9 - P r e s A t rr ( E20 J
L6	= E9
END	I F
EN	— T
FOR  EN   =   1   TO   6
I  F   ENC    EN )    =    T  TH EN S E L ECT	CA S E EN
CAS E  I
ENCH — “ A19 "
CAS E 2
ENCH   =	'N2 0 "
CAS E 3
ENCH   =	' SG "
CAS E 4
ENCH   =	’ RT  ”
C \ S E    5
ENCH   =   " A 12 "
END S EL ECT E15   =   E2 1 3 a% = 0
NOVO E15 % = 0
DO EHI L E E T 5 <=	E 2 2 AR EA II N


' S E 0,	cA LCU LA A PR E S SAO ATPC .






















Pâg na 11

at p na2
MODMIN
M = MMIN
*„„*”*,*’*vY,*,",
'M = 19
IF EN <› 6 THEN
G0 = El / El5
ELS EI F EN = 6   THEN
G0 = 2 * E T / E15
END I F
EsteE1.S.J..a.. % = .0...... .
DO EHI LE N <= NUMTOR
' DO PHI LE N = 19
EDTE =	0
ENCHOKO =	1
mostra.Label7.caption	STR%(EIS) ›	x" + STR%(M)
NCMIN
NC = CNIN
IF EN = 6 THEN ENCH = ”SG” NC = 2
CMAX = 2
CMIN = 2
E ND   I  F
I F   NC   =   0    THEN M   =    N   *     1 ENCHOKO — 0
END  I F
U7 = 0
' RE STR ICAO  DO  PROGRADA
I F    (G0    /	E10   “   S (x   ,   .3	*    3 . 6J	<=   5 . 5	AND	ENCHOK 0	1}
AND G0 /  S ¿M ,  3	> L I MIT ECHUVA IN F THEN
R — G0 / s (u , 3)

mo s t ra . Labe18 . capt on = STRS (NC}
ENCH IME NTO
ENCHOK
I F (A1	<> 0J AND ( E NCHOKI = " TRUE "	TH 2N
A1 ertaue r ke1P ro	- 0
CA LCU LATOR R E
' I F S LM , 1}  < !00	THEN
'	De s v1o0 -- D E SVIOINF	2
Des vi of  = DESVIOS UP + G

' EL S E

De sv1 o0 — DESVIOIN F	2
De s v1 o1 - D ES VI OS UP

' END I F

I F S (N, 1J < 100 THEu
I K E 5 - E8 >-	§ 5 TH EN
Des v o0 - DESVZ OINF	2 De s v1o1 = DES VIOSU P + 6

ELSE

De s v o0 = DE S VIOIN K	2 '	E §	E8	/
De s v1 o1   =   DES VIOSUP  -   6  *   ( ES	E 8	7


END I K
E LSE
I F E 5  —  E 8 > -  S .	TH EN
De s v1 o 0   =   D ESVIOIN	1
De s v   o	— n ESV I OSU P	1

E L S E

oe svJ o0   — D F SVIOIN F	I	’	E 5	8

Pâgina lZ





De s vJ o0J THEN

z1 pñ n a2
Desviol	DESVIOSUP + 1
END I F END I F
I F (Es t eE1 5 3 a	— 1J AND ANC	CMIN	AND §U 7 <
NOVo EUR	=	I EXIT DO
END I F
I F EN CH <> " RT" TH EN NC - NC * T

E L S E

NC = NC •	4


E L S E

END Z F

I F ENCH <> "RT " TH EN
NC = NC • 1

E L S E


NC = NC • 4


LOOP

END I F
END I F

I F    ( NO v OE 1 1%	— 1g	TH EN



tolerancia
TH EN






THEN

























LOOP

' Mudanca para O Caso em que a 100 com ripa tem alta
I F ( E s te E1S 3 a = — 1}	AND	ESTE = 0} AND (U7 < De s v o0)
EXIT DO END I K
I F ¿M = MAIN	AND (A1 e rt are r I‹e1 P ro	— - T} TH EN
MAIN  =  MAIN  *  1
END I F
M = M + 1
ELSE I F ( ENCHOK 0	— 1J AND (Es t eE15 3 a	- IQ AND ( ESTE	0)
EXIT DO
EL S EI F ( ENCHOKO =  — 1)	THEN
I F (M — NIJI N	TH EN MW I N =  MMI N	I
END I F

E ND I F
I F ¢U7 < De s vJ o0J  AND (E 15  < E2 2	THEN
EXIT DO
END I F





LOOP END IF
NEXT EN CLOSE #2

I F   ¢U7   <  De s v   o0)	AND ( E 1 S	E2 2	THEN
EXIT DO
END  I K
I	SE L  >   99   TH EN  EXIT  SUB
E13  = E 15	1
NOvO E1 S%   =	0
' S EL ECOES . TO P

IF (SEL = 0) THEN
msg% = "Nenhuma torre foi encontrada !" * CHRB(13J + CHRS(10) '     *nsg8 = msg6 + "raso necessite de ajuda, por favor" + CllRS(13) CHRí(l0J
msg% - msg$	"ent‹ e em contato com a ALPINA. ”
PRI No ms g S
E ND I F
E ND S UB
S UB DADOSMOD E LO
Page na 13

a1 p1 n a 2
DETERMINA OS VALOAES ESPECÍFICOS DAS VARIAVEIS PARA ESTE MODELO
I F   S ¿N ,   2)	=    1   THEN	’ DET E RMINA O NUMERO DE   LADOS   CON!   AR
E13 =  4

E L SE

E13	E2

END I F
FI = S {M , 8 - E13) ALTAGUA
E4 = E3 - E5
TT = T gM , 2 3 + E18
TV = T ( ¥ , 1 * E18J ROTMOT = S {M , 14 + E18) MOT -  s {N , 12	E18J
N P = S (M , 8 + E18) ROTVEN = S (M , 10	E18 I F TT = " DI RETA " TH EN
ET = 1
E LS EI F LEFT$ ATT ,  4)  =  " M EGA "  THEN
ET = . 9


' AREA  DE   ENTRADA   DE   AR
' DEVOLVE  H0=ALTURA  ENTRADA   AGUA
* DI  F E R E NCIA L   DE   TEMPE RATURA





'ET=EFICIENCIA DA TRANSMISSAO

ELSE
ET = . 96
END IF
13  -   EI8	+ I
CODE LO  =   RIGHTS (STR S( E1$ ) ,    L EN (STR $ ( E 15	— 1}	+
I  F     S  (M ,    18}	—   2    AND   En t: ra00I.OptBacta(2) . Va1 ue	—   TRUE   THEN
MODE LO    =    CODELO   *    " BE— "
END	I F
I F  RIGHTS (STR$ {5 (U,    1)	,   L EN {STR 8   S (M ,    1)   )	1}	" 230 ” TH EN DODE LO = DODE LO + " AP —240 "
E LS E


END	I F

DOD E LO   =   CODE LO  •   RIGHT $ {STR S (S (N ,    tg	,    L EN (STR$   S (N ,    Ig	I)




ENCH

IF	EN	— 6  WHEN
NOD E LO  = MOD E LO  +  " — PV"
EL S E
MODELO	MODELO	"/" + RIGHTS(STR$(NC), LEN(STRS(NCjJ	IJ +
END	IF

I F   S (M ,  1g	4  OR   S (N ,    I)	— 8  TH EN
f 1OD E LO	MOD ELO	+ "— II   "
E L S E
I F   EI8	=   1   THEN
COD E LO   =    COD E LO    +    “ — I "
ELS	E


END  I F
END IF

COD E LO	PODELO  *   " - I I "

















AND

I F   (M  <   NUMTOR§   TH EN
I F   (S {M — 1,	1J	=    S (M , 1)	OR   (S {M	1,	13	S (N , 1) )	THEN
I  F   S   N ,    2	=  1   TH EN
MOD E LO  =    MODE LO    •    ” - I NS "
ELS EI *   S ¿N ,   2	= 2   THEN
COD E LO = COD E LO + " — AS P "
END I F
END Z F
END I F
I F  (Rev%  =  0)  THEN
PODE LO = COD E LO • " - AE " END I F
Es p e ci aI $ = " "
I K É ET 3 < 4	OR PPAE% OR  Ê S Ê M ,  J.8	=	AND  Ê S Ç M ,  1}	>  2 0}	AND  Ç S Ç M ,  2	2
Cn c ra00l .optBac1 a (1g . Vai ue  -   0)	THEN
MODELO = PODE LO *  " — E ” I F	E13 < 4} THEN
Es pe c al S = " En t r ada de a r po r "	stR $ ( E13	+ " 1ado s "
END I F
I K P PA E% TH EN
E s pe c al S = Es pe c a1 $ + " E n cl men t o em PP —AE "
Pág na 14


END I F

at pt na2

I F (S {N , 18J = 1 AND (S(M, 1J > 20) AND (S(M, 2} = 2) AND
(Ent ra00l .optBacl a(1) . Va1ue  =  0J	THEN
EspecJ als	= Especl a1 $ + "Bac1 a de concr et o {pe1o c1i enter "
END I F END Z F
I F Espec1a1$ = " " THEN
Especl at $ =   "NADA"
END I F
ENO SUB
FUNCTION Densidade (L9, L6 P7)
’ CAL CULO DA DENSIDADE DO AR
' VARIAVEIS  DE  ENTRADA :  L9=TEMPERATURA  DE  BULBO  SECO  ( ' Cg
L6=PRESSAO   ATNOSFERICA	(mba r)
'	P7=PRESSAO PARCIAL DE VAPOR NO AR UNIDo (mbar) 'SAIDA: L2=DENSIDADE DO AR (kg/m3)
L 2 = . 348 3 ° L6 /	L9 + 2 73 . 15J — . 1316 * P7 /	L9 + 2 73 . 15)
Densi dade =  L2
END  FUNCTION
SUB  ENCHINENTO  {}
’ LE  AS    CARACTERISTICAS   DO   ENCHIf 4ENTO
VARZAVEI 5 DE ENTRADA : ENCH
’ VARIAVEIS   DE   SAIDA :   A1=   COEFICIENTE  DO   ENCHIMENTO   PARA   MERKEL '	A2=  COEFICIENTE  DO  ENCHII'4ENTO  PARA  MERKEL
'	A3=  COEFICIENTE  DO  ENCHIMENTO  PARA  MERKEL
'	A4=   COEFICIENTE   DO   ENCHINENTO   PARA   NERKEL A 'i= COEFICIENTE DO  ENCHZNENTO  PARA  NERKEL A6=  COEFICIENTE  DO   ENCHINENTO   PARA   PRESEST
'	A7= COEFICIENTE DO ENCHIMENTO PARA PRESEST
*AB=	COEFIC  I ENTE  DO  ENCHI PIENTO  PARA  PRESEST
’	A9=	COEFICIENTE  DO  ENCHIMENTO  PARA   PRESEST
*10=	COE	FICIENTE  DO   ENCHIMENTO   PARA   PRESEST
’	10-	CONTADOR   INICIAL   PARA   LOCALIZAR  LINHA   DO   ENCHI  /'4ENTO
'	NC=  NUMERO  DE  CAMADAS  DO  ENCHI BENTO
’		NM=  NUI'4ERO  NAXIMO  DE   DIS  POSICOES ,   NA   ALTURA ,   DO  ENCHISENTO IF NC <= CMAX THEN
SELECT CASE  ENCH
CASE "A19"
IF	NC  =  I   THEN

A1  =  . 92105 22
A2 = . 0701445 6#
A3 = - . 00451107 9#
A4 = . 000065 3 562# A5 = . 5025269
A6 = 81.31903
A7 = -57.931
A8 = 18.53425
A9 = -1.875487
A10 = l.Z2S
ELS EIF	NC  =  2   THEN
A1  =  1. 1671481671#
A2 =   . 093 3 2178 89#
A3 = - . 00 S 9338 5 36#
A4  =  . 00008 5408#
AS = . S S S 922 692 If
A6 = 91.268 6891742#
A7 = —64. 4486813 924#
A8 = 20 . 8425 9533 95#
A9 = —2 . 148 67 524 514
A10 = 1.225
ELS EI F NC = 3 THEN
A1 = l.419108l30Z# A2 = . 114180395 S# A3 = — . 0072194918# A4 = . 000103 6173#
AS = . 6012236296#
Página 15

' REM Al9 - 1 CAMADA








'   REI'4   A19	-     2    CAMADAS








’ REM A19 — 3 CAMADAS

at pJ na2
A6 - 96.1372825878# A7 = — 63 . 760138864 ?é A8 - 20. 036481899
A9 = — 1. 984I780449# A10 = 1.2 S
E L S EI   F   NC -   4 THEN
A1 = 1 . 65 6303 93 5 2#
A2 - .1371084013#
A3 - -.0086003672#
A4 - .0001224922#
A5 = .6331144182#
A6  =  101. 5 688 37161#
A7 = — G5 . 173973 74 24#
A8 = 20.2 708 °i4422#
A9 = — 1. 945 9S 01 S 2 6#
A10 =  1.25
ELS  EIF    NC  =  5  TH EN
A1	1. 9I4 787 2217# A2 - .l54S2280l# A3 - -.0096791963#
A4 =   . 00013 69428#
A 5 -	. 63422 788 3 5#
A6 = 110. 076269615 #
A7 — -67. 8934719739#
A8   =   20 . 5 7 7 5 81963 7#
A9 =   - 1. 9211404867S
A10 - 1.2375







’ REV A 19  -  4 CAMADAS








REV   A19	5 CAN!ADAS

E LS E
STOP

END I F
CAS E  "   20 " ,   "S?"
I F  NC  =  1  TH EN
A1 = . 7 65
A 2 —  . 0788
A 3 = — . 0049 A4 = . G0006 4S = . S 9
A6 - 91 . 17
A 7 — — 70 . 12 S A8  =  2 S . 02 S A9 = -2.93515
A10 = 1.025



REM   SG/+20	1 CAN!ADA

E LS EI F NC =   2   THEN
Al = 1.12S17	’	REM	SG/ 20	2	CA*tADA 5
A2  =   . 078 88
A 3 — — . 004 9 A4   =   . 00006 A 5 =  . 603 7 7
A6 =  94. 17 118 84#
A7 = —70 . 12 5
A8 — 2 S . 02 5
A9 = —2. 93 S US
A10 - 1. 025					
E L S EI   F   NC    =    3   TH EN
A1 = 1. 444 S 782 #	'	REV	SG/>20	3	C 4MADA S







A10 =  I . 025
E LS EI  F   NC   =   4   ^H EN
AT = 1. 731248 9#
A2  -	. 123 99
A 3 = — . 008 06





Pâ g  na   J.6



REM   SG/>2 0	4 CAN!ADAS


A4 = . 00012 AS = . 65889 A6 = 103 . 89
A7 = — 70.	896
A8 = 24. 80478
A9 = —2. 8 33 5 3
A10	= l . 025
ELS EI F NC =   § THEN
A1 =  2. 0179701#
A2 = 14691 A3 = - . 009748 A4 = . 00016
A 5 —  . 68
A6  =   110 . 64 7 93 4#
A7 = — 72 . 62 26
A8 = 25.3825989#
A9 = -2.89711
Al0 = 1.025

alpina2









REM SG/W20 - S CAMADAS

ELSE
STOP

END IF
CASE ”RT”
IF NC - 14 THEN
A1 = 1.064
A2 = .035264
A3 = -.00235524#
A4 = .000040288#
A5 = .54785
A6 = 68.7952
A7 = -37.694176#
A8 = 11.979728#
A9 = -1.245792
A10 = 1.3
ELSEIF NC = 18 THEN
A1 = 1.232
A2 = .040832
A3 = — . 002 7 2 712f A4 = . 000046649# A S = . S 4785
A6 = 79 . 65 7 6
A 7 = — 4 3 . 645 888#
A8 =  13 . 871264#
A9 =— 1 . 4 4 2496 A10 = I . 3
ELSEI F NC = 2 2  THEN
A1 — 1. 4 A2 = . 0464
A3 = - . 003099
A4 - .00005301#
AS = . 5478 5
A6 =  90. 52
A/ =  -49. 5976
A8 = 15 . 7 628
A9 =— I . 63 92
A10 = T . 3



REM RT	14 CAMADAS









* REM RT	18 cAMADA 5








REM RT	22 CAMADAS

EL SE
STOP

END IF CASE ”A12”
Ir NC = 1 THEN A1 = 0
A2 — 0
A3 =   0
A4  =   0
A5 =   0
A6 =   0
A7 = 0
A8 =   0








Pay   na   17

A9 - 0
A10 = O
E LS EI F NC = 2 THEN
AT = 1. 348S A2 = . 06692 A3 = -.00441
A4 - .0000583#
AS - .632
A6 - 73.2905
A7 = -40.853607#
A8 - 11.846
A9 = - 1 . 02988
A10 = 1 . 5 377 5
ELS EI F   NC   =   3   TH EN
A k -  1 . 7438202#
A2 = . 0827 8
A3 = -.0053415# A4 -	.    0000686# A 5 =	. 63 2
A6 - 87 . 8 08 5 8
A7  -  -   0 . 14119
A8 - IS . 3 9677
A9 = — 1.  4 5 671
A10 = 1.S
ELSEIF NC = 4 THEN A1 = Z.2046938# A2 = . 10419
A 3 -	— . 0067 2
A4 = . 00008 9
A 5 = . 63166
A6 = 119. 7 63 2
A7 = —7 2 . 507 7 21#
A8 = 21. 711
A9 = -1.94488
10 = 1. 5I25
E L S EI F NC = S TH EN
A1 = 2. 583
A2 = . 123 7
A 3 = - . 007 97 A4 -	. 0001 A5 — . E3 2
A 6 = 112 . 317 7
7 = — S 8 . 8 47 9
A8 = 17 . 493088 #
A9 = —1.	S 3 3 18
A10 - 1. 5

alpina2




'	RFP A12 — 2 CAMADAS








'   RE ł A12	—  3 CAMA DAS









' REV A12 — 4 CANADA 5







’ REV AND - Ş CAMADAS

ELS E

STOP


ELS E

END I F
END S E LECT
NC	0

END IF
ENO SUB
SUB ENCHOIt (
'VERIFICA SE E' POSSIVEL ESTE ENCHIMENTO NESTE MODELO
VARIAV EIS D E ENTRADA '
' VA R IAV EI S QUE USA :  NC0 ’	E
I F Ę NC >= CNI N	AND	MC <=   CMAX	THEM
ENCHOlt 1 = "TRUE "
E L S E
ENCHOK1 - " FALS E "

E ND I F END SUB
SUB ENTALP ( L9,	r 3




Pag na 18


CA LCULO DA ENTAL PIA DO AR UNIDO

a1 p   na 2

’ VAR ÍA VEI S 0 E ENTRADA ' L9—TENIPERAJ”URA DE BUL BO S ECO ( ' C
L 3=TEOR DE AGUA 00  AR UNIDO C kg / kg ’ SAIDA : =ENTAL PIA DO AR UNIIDO
L4   = 1. 006 "   L9	L 3 * L2501. 6 + 1. 86 " L9J
END  SUB
UNCT 1ON Enta 1 pl a  ( L 9,	L 3
’ CAL CULO DA ENTAL PIA DO AR UNIDO
' VARIAVEI S   DE   ENTRADA :   L9=TEMPERATURA  DE   BULBO   SECO	’ C)
L 3=TEOR OE AGUA DO AR UNIDO C kg/ kg)
' SAIDA : = ENTAL P ÍA DO AR  UMIOO
L4 = 1. 006 ” L9 • L 3 "" (2501. 6 + 1. 86 ” L9J
Ental pJ a  =  L4
END  FUNCTION

SUB L EDADOS (g USUA L
L ETORRE
' LE89EM2
LEVA P EN0 SUB


SUB    L ETORRE    {§ OPEN " I " , #S , FOR I3 = 1 TO
POR 1 = l
INPUT NEXT J
NEXT I3 CLOSE #§
OPEN "I", #3,
POR I3 = 1 TO
FOR 3 = 1
INPUT


"ALPINA.DAT"
NUMTOR
TO 21
#S, S(I3, Jj


"ALTAGUA 1 . DAT"


NEXT
CLOSE  43
OPEN " I ” ,   # 3,
FOR  13  =  1  To
FOR 3   -   1
IN PUT
NEXT  3
NEXT I
CLOSE  #3
OPEN " I " ,  #3 ,
FOR I3 = l TO
FOR	=   1
IN PUT
NEXT	3
NEXT    13
CLOS E 43
OPEN  " I  ” ,    # 3 ,
FOR T 3 = 1 TO
FOR   3   =   I
IN PUT
NEXT	J NEXT I3 CLOSE # 3
OPEN " I " ,   #3 ,
KOR	13	=    I	TO
FOR 3   =   1
IN PU<
NEXT  3
NEXT 13
CLOSE #3
OPEN " I " , # 5 ,
13	= 1



”ALTAGUA3 . OAT ”
6
TO 4
# 3 ,  AA 3 ( I 3 ,  3 g


" A LTAGUA4 . DAT "

TO 43
#3, AA4(I5, j)


" P I LAR ETE . DAT "
6
TO  7
# 3,	PI LAR Ç 1 3,	3


" MI NCAM . DAT "
NUMTO R
TO  7
 , NCM(I3, jj


"TEXTO . DAT "



























Pág  na  19

al p á n a2
FOR 13 - I TO NUMTOR FOR 3 = 1 TO 27
INPUT  # 5 ,    T{I3 ,   3
NEXT	3
NEXT 13
C LOS E # 5 END SUB

SU B   L E VAP    ( )
OPEN "1", #5, "VAP.DAT"
IN PUT   # 5 ,   NLINVAP
FOR   I	= 1 TO NLINVAP FOR 3 = 1 TO 8
INPUT #5, VAP(13, J)
NEXT
NEXT	13
C L OS E   Á § E ND S UB

SU B   MERI+E LENCH   $g
' PARA   CALCULO  DO  MERK   E L   DO   ENCHINVENTO
'VARIAVEIS DE ENTRADA: A1 A A4-COEFICIENTES DO ENCHIMENTO
'	A 5=DERIVADA  DA  CURVA  CARACT ERI ST ICA
'		R= INT ENS I DADE   DE   PP ECI   PI TACAO    {m 3/ (h . m2) )	...-' H0=ALTURA DE ENTRA DA DE AGUA Çm)
E4=DELTA — T
'	L= LAN	BOA	’' ’/^ 'VARIAVEIS DE SAIDA: K3=MERKEL DO ENCHIMENTO		' 'VARIAvEIS USADAS: K0=MERKEL DO ENCHIMENTO - VALORES INTERMEDIARIOS
'	F9=CORRECAO EM FUNCAO DA INTENSIDADE DE PRECIPITACAO
K0 = A1 • A2	R + A3 " R ^ 2 + A4 " R ^ 3
'** Estimativa do Numero de Merkel com Enchimento W20.
" " " Est mado que se ten ha 12% a ma s no ya o r de K0 . conlo rme con s t a
* " * de es t udos da En g enh a rJ a .
I F ENCH = ">20" TH EN
K0 = 1. 12 “ K0
END I F

F9   -	. 2 78 8 67 + . 078 9667 “' R —  3 . 04 S 44 E - 03 ” R ^ 2 + 2 . 802 92E — 0 S “  R ^ 3
K0	= K0	F9	*   {H0	—  8 . S )   /   ( HO   “   1 . 62 •   8 . 5   :   REM CORRECAO   EM NUNCA O DE   ri0 K0   =   K0    '	(1	-  . 007    ”	E4 — 10J	: R EM CORRE CAO EM FUNCAO DO DE LTA -T
K3  =  K0  "  L  ^ A § :  REF	MERK EL DO  E NCHINFNTO END SUB

SUB   MERK E L PRO    Ç$
' CA LCU LO  DO   MERKEL   DO   PROCES SO
' VARIAV EI S  DE   ENTRADA :   TT=TA$
+2=TAF
'	H1=ENTALPIA DO AR NA ENTRADA
L   = LAN BOA
’			L6-PRESSAO      ATMOS FERICA ’ V4R	IAV	EI S   OE SAIDA : K=NERKEL DO PROCES SO
’ VA R IAV E I S   USAOAS :    N—NUN!E RO   DE    I NTERVA LOS    DE    ? ALCULO
'	D 7=INTERVA LO  DE  CEP PERATURA   CORRES POND ENT E
’	D8—I NTERVA LO   DE   E NTAL PIA   CORRES PUNDE NT E
'	K=N!E RK E L  DO   PROCES SO
’		T0=TENPERATURA    DO   AR	UNT0	A GOTA DE AGUA H7= E NTAL PIA DO AR CORRES POND END-E A T0
H 6= ENTA L PIA    DO AP
L 7= UMIDADE   RELATI VA
L 6= P R E S SAO	ATM O S F E R I CA
’	L 5 —A LTI	UDE
L 4= ENTA L PIA   DO AR   UMI DU   ( k 3 / k g )    VINDA   DE   PR OPA R

1 e rta12% =  0
N  =  INT	( (TI	-

CALCULO      PARA ’ N-	4

T2 }	/   2   +   5	/    ?)	’   2	j

FI CAR	IC UAL	A  CAMD INOX

Pág ne 20

alpina2
D7 = {TD — t2	/	¢N — Tg D8 = D7 " 4. 18 68 / L
K = 0
FOR 3 - 1 TO N
T0 = T2 + D 7 ”  (0	1)
H 7 = H1	D8 * (3	1J
' CA LCUL 0 PARA FICAR IGUA L A CASOI M1X
' IF	3 = 1 THEN T0 = T2 +   . 10267 3 "   {TT	T2§ ' I F 3 = 2 THEN T0 =- T2 + . 406204 * {T1 —  T2 I F 3 =  3 THEN T0 —- T2 + . S 93 7 96 “  ÇT1 —  T2) ' I F 3 = 4 TH EN T0 = T2 •	. 89 7 3 2 7 "  (+1	T2) ' H 7 - H T *  (T0	— T2)	"	4 . 1868 / L
'  “- “-ü a“ 7, "r 7, Y- Y, Yr + Y,", ü
L9 = T0
L 8 = +0
L 7 = 100

PROPAR
3 UNTO	A GOTA DE AGUA
H 6 = L4

'	GOSUB   5 110  :   R EN   CAL CULO  DAS   PROPRIEDADES   DO   AR
H6   SERA '   A   E NTAL PIA    DO  AR   SATURADO

I K 3 = 1 OR 2 = N TH EN
14 = 1
ELSEIF INT(J / 2J = J / 2 THEN I4 - 4
ELSE
14 = 2




NEXT J
K = K /	3 E ND S UB

STATIC SUB MODMIN ()


' MERKEL DO PROCESSO

' CALCU LA O MENOR MODELO POS SZVEL PARA O RES FRIAM ENTO EN QUESTA O
FOR M = 1 TO NUMTOR
IF S(M, 3j >  AMIN THEN
MMIN = M EXIT FOR
END IF
NEXT M
END SUB
SUB MostraEncontrado ()
msg8 - MODELO	SPACES(29 - LEN(AcertaB(E1)) - LEN(MODELO))
   =  m s g S	A ce rt a S ( E I)	+    ”	’

valor
 
Vai o r

=	E 3	U 7
=  ms g S +  Ace rca$ (valo r}	+  ’	’
=  E 5  +  U 7

m s g $ = ms g S	Ace ra a$ ÇVa 1 o r)	• "   "
Val o r = E 8
n s g S - m s g S + A c e et a $ ÉVa 1 o r
Valor = U7
I F Ê U 7 > 0} THEN
msg S = msg $ + " " • Ac e rta $ (vai o r)	+ "
E LS EI F (U 7 < 0J THEN
msg5 = msg%	Acerta$(valor)	'	”
END I F
m s g S =  m s g $	sTR S ( E15 )	“ x " * STR $ MOT) + S PAC E $ ( 6 — L EN ( STR S (MOTA  )
Pãgina 21






























END


ms gS = ms gS + SARS ¢S P L}
I F	S E L = 1g THEN
mo s t ra . Label 12 . captl on
E LS EI F	S E L = 2)	TH EN
mostra.Label13.Caption
ELSEIF (SEL - 3) THEN
mostra.LabeJ14.Caption
ELSEIF (SEL = 4J THEN
mostra.Label15.Caption
E L S EI F (S E L = S } THEN
mo st ra . Label 16. Capt on
E LS EI F	S E L = 69 TH EN
mos t ra . Labe117 . Capt:1 on
E L S EI F (S E L = 7’} THEN
mo s t ra . Label 18 . Capt on
ELS EI F	S E L = 8) TH EN
cos t ra . La bel I9. Captl on
E LS EI F {S E L = 9)	TH EN
mo s t ra . L abel20 . Capt on
E L S EI F (S EL > 9J THEN
most ra . Label 12 . Capt on cos t ra . Label I3 . Capt on most ra . Label 14. Capt1 on mostra.Label15.Caption most ra . Label 16. capt 1on mos t ra . Label 17 . Captl on mo s t ra . Lab el 18 . Capt1 on mo s t ra . L abe119 . captl on mos t ra . Label 20 . Capt on
END I F
S UB

alpina2

= msgB
= msgB
= msgs
= msgB
= msg%
- msgB
= msgB
= msg$
= msg$
= most ra . Label 13 . Capti on
= most ra . Label 14. Capt1on
= most ra . Label 15 . CaptJ on
—mo s t ra . Label 16. Captl on
= mo s t r a . Label I7 . Capt on
-mos t ra  . Label 18 . Capt 1on
—mo s t ra . Label 19 . capt   on
-mo s t r a . La he1 20 . captñ on
= ms g S


S UB


NCA IN	(}
SELECT CASE ENCH
CASE "Al9"
CII N = NCM {N , 2
CMAX = NCM {M , 3)
CASE "> 20 "
CAIN = NCM (M , 4g
CMAX = NCM {M , 5) CAS E " SG "
CAIN = NcN (u , 4g
CMAX = NCr4 {M , S
CAS E " RT"
CAIN = NCM (U , 6J
CNAX = NCN M , 7
CAS E ” A12 ”
CMIN = NCM(M, 2)
CMAX = NCM(M, 3j CASE ELSE
PRINTER.PRINT "ENCH="; ENCH STOP
END SELECT

END SUB

FUNc \ ou P r e sArm ( E 20}

P resAtm = 1013 . 2 5 "	( (288	6. S '’' E20 / 100oJ / 2S8J	s . z s s
END FUNCTION

S UB PR ES E ST
CALCULO DA PERDA DE PRESSAO ESTATICA NA TORRE
'VARIAVEIS DE ENTRADA: F1=AREA DE ENTRADA DE AR NA TORRE (m2) '	A6 ATE A1O=COEFICIENTES DO ENCHIMENTO
R=INTENSIDADE DE PRECIPITACAO (m3/(h.m2))
R1=DENSIOADE DO AR NA ENTRADA (kg/m3)
R4-D EN S I DAD E Y EDI A DO AR ( k g / m 3

alpina2
VaZ=VAZAO            VOLUNETRICA     I'4EDIA    DO    AR    (m 3/ s ) E16=PERDA DE PRESSAO ESTATICA ADICIONAL (mmCA) W=VELOCIDADE NEDIA DO AR NA TORRE (m/S)
' VARIAVEIS   DE  SAIDA :   D4=PERDA   DE  PRESSAO   ESTATICA   TOTAL   {+mCA)
'	D1=PERDA OE PRESSAO   E STATTCA NA TORRE   (mmCA$
' VARIAVEIS QUE USA : w1=VELOCIDADE NEDIA  DO  AR  NA  ENTRADA  DA  TORRE   {m/S)
’	D2=PERDA  DE  PRESSAO  ESTATICA  NA  ENTRADA  DA  TORRE	Pa}
'	z1=ZETA  NO  INTERIOR  DA   TORRE
D3=PERDA  DE  PRESSAO  ESTATICA  NO  NEIO  DA  TORRE   {Pa}
I F S(M , 2)   =   1 THEN
D2 = 4 * U ^ 2 * R4 / 2

E LSE

D2 = I . 76 * w1 A 2 " R1 / 2

END I F
z1 = A6 + A7 * U + A8 * U	2 + A9 * w ^  3 + A10 *	R — 10)
D3 = z1 * w ^ 2 * R4 / 2 D1 -   (D2 + D3	/   9. 807 D4 = D1 + E16
END SUB
FUNCTION  Pres Pa rvap  (P4,  L9,  L8,   L6J
’ PRESSAO PARCIAL  DE  VAPOR  DO  AR  Uf•\IDO
' VARIAVEIS   DE  ENTRADA :    P4= PRESSAO   VAPOR   SATURADO ,    COM   BASE   NA   TBU '	L9=TEMPERATURA  DE  BULBO   SECO  DO  AR	' C§
’	L8=TEMPERATURA   DE   BULBO   UMIDO   DO   AR	’ Cg
'	L6=PRESSAO   ATNOS FERICA   (mbar)
’ SAIDA :    {P79=PRESSAO   PARCIAL    DE VAPOR  NO AR  UMIDO  {abará
P7  =  P4   -	7. 4412 -  € (67	- L 8)	/    420J   ^  2)   *  L8   /   (L8   +  23 5)   *   (L9    -   L8)	* L6 / 1006. 7
PresParvap = P7
END FUNCTION
FUNCTZON  Pr es vapsat   ( P8)
’ CALCULO  DA  PRESSAO  DE  VAPOR   SATURADO
' VARIAVEI S  DE  ENTRADA :  P8=TEMPERATURA  DO  AR   (  ’ C)
’ SAIDA : PRESSAO DE VAPOR SATURADO (mbar}
C1 = (7 . 4412 —  ( (67	- P8}	/  420)  ^  2)   °  P8  /   (P8   +  23 5) I F P8 <= 67 THEN
P9 =  6. 1075  ”  10  ^  c1


(- 4)

ELSE

09 = 6. 107 3 * 10	C1 + ((79. 5 — P8)	2 / 1. 11	99) " 9. 807201 * 10 ^

END  I F
Presvapsat = P9
END FUNCTION
SUB  PROPAR  ()
' SUB - ROTINA  PARA   CALCULO  DAS   PROPRZ EDADES   TERNODINANICAS  DO  AR   UMIDO
'UARIAVEIS DE ENTRADA: L9=TEMPERATURA DE 8ULBO SECO ('C)
L8=TEMPERATURA  DE  BULBO   UNIDO     ' C)
L6=PRESSAO  ATMOS FER ICA    (mbar)
'	L §=ALTITUD E {mNM)
' SAÍDAS :  P7=PRES SAO  PARCIAL  DE  VAPOR  NO  AR   UNIDO
'	P5=PRESSAO  DE  VAPOR  SATURADO  COMI BASE  NA  TBS '	P4=PRESSAO  DE  VAPOR   SATURADO  CON  BASE  NA  TBU ’	L7=UMIDAD E RELATIVA DO AR (%)
L4=ENTAL PIA  DO  AR   UNIDO   (k0/kg} L3=TEOR DE AGUA DO AR UMIDO (kg/kg) L2=DENSIDADE DO AR UMIDo (kg/m3)
P5 = eresVapsat(L9)
P4  =    P r e s Vap S at	L 8)
7  — Pre s Pa rvap CP4,   L9,   L 8,   E9}
L3 =  Teo rAgua (P7 ,  E9)
L 7 = UR P 5 , P7)
L4 = Ental pt a(L9, L3}
L2 = Den sldade(L9,  E9,  P7)
END SUB

<*91 na 23


5U8 SAIFICHA {}
' SAIDA VIA INPRESSORA
contaer r  =  0
ON LOCAL ERROR GOTO CheckError PRINTER . Pri   ntTarget   =   "LPT1 : OIM Pressao(1 TO 25)

aJ pt naz

PRINTER.PRINT TAB(23); "TORRE DE RESFRIAMENTO DE AGUA A L P I N A"
TAB (23} ;   " - - - ---- ---—- - — — — 	— -— —— 	’
/t .	TT
O E	2 ECOES . TI\IP"
H LE' NOT’ EOS Î
INPUT #2, ZSEL, ZMODELO#, ZE1. ZE3, ZE5, ZE7, ZE8, ZE13, ZE16, ZE20,

zu7,

ZN , ZNC , ZS , ZMOT , ZS PL , ZF1 sï Ca , ZEN
IF	(ZNODELO $ = MO0ELO§ THEN
EXIT DO
END I F














" / " ¡

LOOP
CLOSE #2

' Retî rar a 1mpressao da cot eranc1a
PRINTER . PRINT  TAB(10J ;   " FICHA  TECFIICA   N 
IF ZF1 SJ Ca >= 0 THEN
PRINTER . PRINT 200 + INT ( {ZFJ s1ca + . 05	° 10)
' ELSE
'	PRINTER . PRINT 100   + ABS (INT( (ZFI s1Ca   -    . 05)	“ 10a ) ;
END I F
PRZNTER . PRINT " / " ; RIGHTS {DATE $ , 2)
PRZNTER . PRINT
PRINTER.PRINT TAB(10); ”CLIENTE: ”; Entra001.txtcliente.Text;
PRINTER . PRINT TAB{60J ; "DATA : " ; MID$ (DATE$ , 4,	2) ; "/" ; LEFT$ {DATE $ , 2) ;
RIGHTS (DATE$ , 2}
PRZNTER . PRINT
’ PRINTER . PRINT
PRINTER . PRINT TAB {10) ; "DADOS DE PRO3 ETO : " ; PRINTER . PRINT TAB (50) ; "PRO3 ETO" ; TAB (65) ; "REAL" PRINTER . PRINT
PRINTER . PRINT TAB {10} ; "CARGA TERMICA	";
PRINTER . PRINT USING ”#####4##" ; E1 ° {T1 — T2) * 1000 ; PRINTER . PRINT TAB (60) ;
PRINTER . PRINT USING " ########" ; E1 *   (T1 - T2)   * 1000 ;
PRINTER . PRINT TAB(73) ; " kcal /h"
PRINTER . PRINT TAB (10) ; "VAZAO DE AGUA TOTAL	" ;
PRINTER . PRINT USZNG "######## . # " ;   E1 ;
PRINTER . PRINT TAB (60) ;
PRINTER . PRINT USING "44#####4 . #" ; E1; PRINTER . PRZNT TAB (73} ; "m3/h "
PRINTER. PRINT TAB(10) ; "TEMPERATURA DE AGUA QUENTE.	” ;
PRINTER . PRINT  USING  "#####4## . #" ;   se J ec0 (1,	3 ;
PRZNTER . PRINT TAB(60} ;
PRZNTER . PRINT  USING   " #4###### . #" ;   T1  ;
PRINTER . PRINT   TAB  73) ;   "  ’ C"
PRINTER. PRINT TAB(10); "TEMPERATURA DE AGUA FRIA.	" ;
PRINTER . PRINT USING " ######## . #" ; Set ec0 (1,	4J ;
PRINTER . PRINT TAB (60} ;
PRZNTER . PRINT USING " ### ##### . # " ; T2 ; PRINTER . PRINT TAB {7 3 } ; " ' C "
PRI NTER . PRI NT TAB (10) ; "TEMP ERATURA DE BULBO UMIDO DO AR	" ;
PRINTER . PRI NT USING "4# #4##4# . #" ; E8 ; PRINTER . PRI NT TAB (60) ;
PRINTER . PRINT USING " ######## . #" ; E8 ;
PRINTER . PRINT TAB É73) ; " ' C"
PRINTER . PRINT TAB {10} ;   "ALTZTUDE LOCAL	";
PRINTER . PRZNT USING "########" ; E20 ;
PRIhTER . PRINT TAB (60} ;
PRINTER . PRINT USING  "########" ;  E20 ;
Pág1 na 24


PRINT ER . PRINT TAB ( 7 3 } ; " mNlvl ”

a1p1 na2

' PRI NTER . PRINT TAB 10J ; "PRE S SAO ATMOS FERICA	";
* PRINTER . PRINT USING   "######## . 4# " :   L 6 ; ' PRINTER . PRINT TA B (60J ;
'PRINTER. PRINT USING "########.##" : L6; 'PRINTER. PRINT TAB(73J ; "mbar"

INT ER’ PRINT T
I	4 B '

0	NIVEg DH

R UIDO’’ ’ ’ ’ ’ ’ 	” ;
E

ELS E

PRINTER. PRINT "SILENCIOSO" ; TAB(60J ; "SILENCIOSO"
IF E18 = 1 THEN
PRINTER. PRINT "STANOARD" ; TAB(60) ; "STANDARD"
ELSEIF E18 = 2 THEN
PRZ NTER . PRINT " SI L ENCI0?O" ; TAB ( 60J ; " SI LENCIOSO"
END IF

END I F
PRINTER. PRINT TAB(10); ”PRESSAO SONORA POR VENTILADOR, A 2m...” ;
PRZNTER . PRINT USING "###4####" ; SPL ; PRINTER . PRINT TAB §60J ;
PRINTER . PRINT USING " ########" ; SPL ; PRINTER . PRINT TAB (73	; " dB {A§ "
IF S(M, 2) = 2 THEN
PRINTER . PRINT TAB ( 10) ;  " ENTRADA DE AR POR	” ;
PRINTER . PRINT US ING	" ### ##4 # # " : E13 ; PRI NTER . PRINT TAB (€i0J ;
PRINTER . PRINT USIN¢i " ########" ;   E13 ;
PRI NTER . PRINT TAB ( 7 3) ; " LADOS "
END I F
PRINTER . PRINT
PRINTER. PRINT TAB(10): ”TORRE SELECIONADA: ” ;
RI NTER . PRINT TABC 3 5 ; NODELO PRINTER . PRINT
PRINTER . PRINT  TAB  10J ;    "TIRAGEM  DO  AR ..   . . ..   . . ...	. . ...	. .	‘ ;
I F S (N , 2)	= I	THEFJ
PRINTER. PRINT "FORCADA"
E LS E
PR I NTER . PRINT "INDUZ IDA "
END I F
PRINTER. PRINT TAB(10) i ”NUMERO DE CELULAS.	” ;
PRINTER . PRINT U SI NG "####### # " ;   E1 §
PRINTER . PRINT TAB (10J ; " TI PO DE ENCH I MENTO	" ;
I F EN - 6 TH EN
PRINTER . PRINT "To r re s em En ch1men to " ; ELS EIF	(ENCH = " SG " } THEN
PRI NTER . PRINT " GRAD ES TRA P E ZOIOA I S ” ;
I	E nt ra001. ch ec k4. Va1 ue =	1 THEN
PRI NTER . PRI NT ‘ ' —  A E ”

EL S E


PRINTER . PRINT " "

END IF

PRINTER.PRINT "BLOCOS DE FILME CORRUGADO" ELSEIP (ENCH - "W20”j THEN
PRINTER. PRINT ”BLOCOS LAVAVEIS DE CHAPAS" ELSEIF (ENCH = "3T") THEN
PRINT ER . PRINT " BARRAS AUTO— LAVAV EI S " END I F
PR  I  NTER . PRINT   TAB   10}  ;    "VAZAO   DE   AR   EM   CADA    V EN+ILADOR 	";
PRI  NTER . PRINT   US ING	” ######## . ##" ;   V 5 ;
PRINTER . PRINT   TAB( 60)  ;    " m 3/   s "
P  R  I   NT   E  R  .  P  R  I   NT	TA    B     10    §  ;     "  D E RDA	D E     P R  E  S  SAO	E  STATECA   NA  ”OMRE 	"  ;
PRINTER . PRINT  USING   “ ## ### # ## . ## " ;    D4	- E 16 ; PRI    NTER . PRINT  TAB(60) ;    " rr+ c    "
I F	E1G	<>	0J	+H EN
PRINTER. PRINT TAB(10) ; ”PERDA DE PRESSAO ESTA*ICA ADICIONAL.	” ;
PRINTER . PRINT USING " 44 ###4#4 . ## " ; E T6 ; PRINTER . PRINT TAB 60} ; " mmcA "
PRINTER . PRINT TAB ( IOJ ;  ” PERDA DE PRES SAO ESTATICA TOTAL	“ ;
Pagina 25

aJpina2
PRINTER. PRINT USING "########.##" : D4; : PRINTER. PRINT TAB(60) i "mmCA"
END I F
PRINTER . PRINT  TAB(10) ;    "D ENS I DADE  DO  AR  NO  VENTILADOR	" ;
PRINTER . PRINT USING "#4### #4# . ## " ; R 5 ; PRINTER . PRINT TAB(60) ; "kg/m3 "
PRINTER.PRINT TAB(10) i "MODELO DO 1’ENTILADOR.	” ;
13 = E18 + 1
I F   {S (M ,   1}	-  63	TH EN
ModVen t =  " vAL— 148 3/8  '
AN = 4 S
E LS EI F  {T ¿M ,  13	=  " 9EM2 " }  OR (T {M , 13)	— " 8EM2 " } TH EN
Modvent   =  T {N ,   13	STR $ {S {M ,  17  + I3	)  +   "  K "
Modvent   = Modvent   + RIGHTS ¢5TR$ {NPg ,   LEN (STR$ {NP)	— 1}
z F	s (¥  ,   7}	<>   S (N ,  17	+   13)	THEN
uodven t    -    ModVent	"   ( "   +   FORSAT $ (S {M ,   7) ,   "# . ## ”)	" ) "
END I F
I F  HAN —  I NT  HAN g	<  . 2 'i TH EN
AN  =  I NT {AND
E LS E
I F   (AN   -   INT  (AND )    < . 7 5 THEN AN = I NT(AN) • . 5

E LS E

AN =  I NT (AN ) + 1

END   I  F
END I F
E LS E I F  QT §M ,  I3 g  -	" VAP "	OR   (T {M ,   13	=   " VA L " }    THEN
ModVen t     =    RIGHT$ (STR$	S (u ,  7}	* 1000a   , LEN (STR$   (S §M , 7)	‘   1000a   )    -
ModVent   =   TQM ,   I3J	•   "— "	nodv ent   +   "/ "
Modvent = YodVent	RIGHT$(STR$(NP), LEN(STR5(NP)) - 1) + "/"
uodvent   =   vodven t	RIGHTS (STR$ {AN) ,   LEN	STR $ LAN) ) — IQ
END  I  F
PRINTER . PRINT "  "  +  uodVen t
PRINTER . PRINT   TAB( 10)  ;    "D I  AMETRO   DO   VENTI  LADOR 	,”
PRINTER . PRINT   USING   " ######## " ,  S (M ,  7)	“ 1000 ;
PRINTER. PRINT TAB(60) ; "mm"
PRI NTER . PRINT  TAB  10J ;   "ANGULO  DAS   PAS	”
I F   ( (S (U , U	= 63 } THEN AN =	4 5
END	I F
PRINTER . PRINT  USING   “ ######## . # " ;   AN ;
PRINTER.PRINT TAB(60) ; "GRAUS"
PRI NTER . PRINT   TAB{10) ;   "TRANSMI S SAO   . . . . . . .....
PRINTER . PRINT  T §M ,   2 é   +  E18J
I F   T (M ,    2 3   •   E18	<>  " DI  RETA"  THEN

PRI NTER . PRINT  TAB(10J ;  " TAXA  DE  R EDUCAO ... PRI NTER . PRI NT USI NG " # . ## " , S {N , 14 • E18
END  I F

. . . . . . . . . . . . . . . . . . .	
/ S(M, 10	El8)

PRINTER. PRINT TAB(10) i ”POTENCIA D0 MOTOR ELETRICO.	” ;
I F NOT   <   1   THEN
PRINTER . PRINT   USING   "######## . 4# “'	NO* :
E LSE
PRINTER. PRINT USING "########.#" : MOT;
END I F
PRINTER . PRINT   TAB(60a ;    " cv    "
PRINTER . PRINT   TAB(10J ;   " POTENCIA    CON SUMI DA   PELO  VENTILADOR	" ;
PRINTER . PRINT   USING   ” ###  ## ###  . # " ;   NV ;
PRI   NTER . PRINT   TABC 60J    ; "B HP "
PR I NTER . PRINT  TAB  10} ;    "ROTACA 0  00   V ENTILADOR	” ;
PRI   NTER . PRINT  USI NG   " ## # # ###4 . #" ;    S (M ,    10	+ E 18)	;
P RI    MT E R . PRINT  TABC 6 0     :  " R P M "
PRINTER. PRIN* TAB(10J ; ”VELOCIDADE PERIFERICA DO VEN*ILAOOR. ” ;
PRI	NTER . PRINT US I NG " ## ## #### . # " ; UA ; PRINTER . PRINT TABL60J ; " m/ s "
PRINTER . PRINT   TA8( 10)  ;    " MOTOR    E L ETR  ICO	TI  PO	"
PRINTER.PRINT T(M, EI8 + 3)
PRINTER.PRINT TAB(10): ”PERDA DE AGUA POR EVAPORACAO.
PRINTER. PRINT USING "########.##" ; 100	(*1 - T2) / LH2   -   H1)	/ 4. 18 7   /
Paq1 na 26

al pl na2
(X2  -  X1)	-   T2}  ;  :   PRINTER . PRINT  TAB(60) ;  "%"
PRINTER.PRINT TAB(10J; ”PERDA DE AGUA POR ARRASTE.............
TAB {60) ; "1"
PRINTER . PRINT  TAB 10) ;   "ALTURA  DE  ENTRADA  DE  AGUA  QUENTE	" ;
PRINTER . PRINT  USING  "######4# . #" ;  H0 ;
I F   (S (N ,  18)	=   2  AND  PI LAR ETE    <>  0}    THEN PRINTER . PRINT   TAB (60) ;    "m	(°)     " ELSE
PRZNTER . PRINT  TAB(60) ;  "m"
END  I F
I F (S (M , 1)	< 100	OR EN = 69   THEN
PRINTER.PRINT TAB(10J; "PRESSAO REQUERIDA NA ARVORE	" ;
ELSE


0. 1" ;

PRINTER . PRINT  TAB 10) ;   "PRESSAO  REQUERIDA  NOS  CANAIS	" ;
END IF
TudoBem% = - 1
PRE! - 0
IF EN = 6 THEN
PRINTER.PRINT USING "########.#”¡ 10;
PRINTER . PRINT  TAB(60) ;  "mCA"
E LSE I F   S (M ,   1)	>=  100	THEN
PRINTER . PRINT USING "4####### . #" ; . 5 ; PRINTER . PRINT TAB{60) ; "mv"
ELSEI F   (S {M ,   1)	<=  80J	THEN
OPEN " I " , #5,	" pressao . dat"
DO WHILE NOT EOF(S)
FOR  I%  =  1  TO  25
INPUT  #5 ,  P re s sao  (I%)
NEXT I%
I F   Pressao (U	= s (M , U	THEN
EXIT DO
END IF
LOOP CLOSE #5
TudoBem% = 0
I% = 2
DO WHILE   II	<= 22
IF (El / EIS + .0001 >= Pressao(I%)) AND (El / El5 + .0001 ‹=
Pressao  2  +  II)  )   THEN
PRE! = ((Pressao(I% + 3) - Pressao(I% + 1)) / (Pressao(I% + 2) - Pressao(I%))) * ((El / E15) - Pressao(I%)) + Pressao(I% + 1)
TudoBem% = -1
EXIT DO END I F
I%  =  I%  +  4
LOOP
IF	TudoBem% =  0  THEN
PRINTER . PRINT " Vej a Obse rvacao (**J " ELSE
PRINTER . PRINT USING "######## . #" ; PRE ! ; PRINTER . PRINT TAB{60} ; "mCA"
END I F END I F
PRINTER . PRINT  TAB {10) ;  "ALTURA  DE  RECALQUE  TOTAL	" ;
IF	CN = 6 THEN
PRINTER . PRINT   USING   "######## . #" ;   H0   +   10 ;
PRINTER . PRINT  TAB(60) ;    "m	" ELSEZF   CS (I'M,   1)	>=	100) THEN
PRINTER . PRINT  USING  "######## . #" ;  H0  +   . 'i ;
PRINTER . PRINT  TAB{60) ;  "rrCA"
EL S E
PRINTER . PRINT USING " ##444### . 4" ; H0 + PRE ! ; PRINTER . PRINT TABC60J ; " mCA"
END  ZF
PRINTER . PRINT  TAB{10) ;   "PRESSAO  NAXINA  ADMI SS IV  EL   NA  ENTRADA . . “ ; I F EN = 6 THEN
PRINTER . PRINT USING "### #### . #" ; 20 ; PRINTER . PRINT TAB(60} ; " rrC "
Pàgï na 27

at pi na2
EL 5El F  (TudoBem% =   - 1}	AND   (S {M ,   1J	< 100) WHEN PRINTER . PRINT   USING    "########  . #" ;     10   ; PRINTER . PRZ NT TAB(60) ; " MCA"
ELS EI F CTudo Berr% = 0)	AND (s CY ,   IQ   < 100)   THEN
PRINTER.PRINT ” Veja Observacao ("”)"
ELSEI F	S (M , 1)	>  80J	THEN
PRINTER . PRINT   USING   "######## . # " ;   1 ;
PRINTER . PRINT  TAB {60a ;   " mCA"
E ND   I F
PRINTER. PRINT TAB(10J;  ”DIMENSOES EXTERNAS.	” ;
PRINTER . PRINT T (M , 7 ;  "  X " ; T {M , 8 ;  ”  x " ; I F ENCH = " SG " OR ENCH — " > 20 " TH EN
I F NC <= 2 TH EN ALTUR = 1
E LS EI F NC <-		3 THE ALTUR —	2
EL S EI F NC <= 5 THEN
A LTUR =	3
END I K
E L S EI F LEFT$ ( ENCH ,  1}	= "A"	THEN I F NC <= 3 THEN
ALTUR = 1
E LS EI F NC <= 4 TH EN A LTUR =	2
E L S EI F NC <= 5 THEN ALTUR =	3
END I F
E L S EI F ENCH =  " RT"	TH EN ALTUR =	3
END IF
COLUNA  =  8	3  “	{ALTUR  — 1g   +   (E13	1) PRINTER . PRINT T §M , COL UNA3 ; "mrs"

PRINTER . PRINT  TAB(10J ;   " P I GMENTACAO . . . . . ...	. . ....
AL PI NA"
PRINTER. PRINT
PRINTER. PRINT TAB(10) i ”MATERIAIS DE CONSTRUCAO: "
PR I NTER . PRI NT
I F  (S (M ,   18)	= 1)	AND (Rev% =   0}  WHEN rrs g S = TCP ,  18		• " — AE"

V ERDE ,   PADRAO

E L S E


I F Ent	ra00l.optBack a (2) . va1 ue = TRUE THEN ms g S  =   " S upe rJ  or   :   PRF   Au to po rtant e" I F Rev% = 0 THEN



EL S E

ms g $   -    ms g $   +    —"
END  I F

ms g S = T (M ,  18)

AE "

END	I F
END IF
PRI  NTER . PRI NT  TAB   10)  ;   ” E STR UTURA . ..   . . ..   . . ..   . ...	. . ...... ......
PRINTER . PRINT   ms g S
I P  Ent ra001 . opt Bac   a ( 2)  . Val  ue  =   TRUE   THEN
PRINTER . PR ZNT   TAB (4 9J  ;   ” I n fe r   or   :   co roa   de   con c   e to "
E ND  I  F
n S g 6  =   ” R E VE ST I MENTO   LATERA L . . . . . . . . . . . . . . . . . .   "   *   T (M ,   19)
PRINTER . PRINT  TAB¢  0J  ; m s g 3 ;
I r  Rev%  — 0 TH EN

E LS E

PRINTER. PRIN*

"	AE "

PRINTER . PRINT ""
E ND  T F
PR I NTER . PRI NT TAB 10J
I P Cs CN , 2) = 1} OR
8) <> " c11ent e } "	THEN



" BACIA  DE AGUA FRIA . . . (M ,   2)	-  2 } AND ( (S ¿M ,



18) - i’AND (RIGHT$(Espec4aJ 5,

PRI   NTER . PRI  NT   " PRF " ; I F Rev% = 0 THEN
PRI NTER . PRI NT  "   — AE ”

E L S E

Pfiqi na  28




ELS E


PRINTER. PRINT
END I F

a1 pl na 2

PRINTER.PRINT ”Concreto (pelo Cliente)”
END I F
PRINTER . PRINT  TAB( 10) ;    " ENCHI BE NTO	” ;
I F EN   =   6   TH EN
PRINTER . PRINT   "— — ”
ELS  EI  F	ENCH   <>    " SG "	THEN
PRINTER . PRINT  "PVC  -  AE "
E L S E
PRI  NTER . PRI NT   " POLI PROPI L ENO" ;
IF Enira00l.check4.Value = 1 THEN
PRINTER . PRINT   "   — AE  "
E L S E
PRZ	NTER . PRINT  ""

END I F
ENO IF
PRINTER . PRINT TAB(10J ; " ELI MINADOR E S  DE  GOTAS ..  . . ..  . .
PRINTER. PRINT TAB(10); "SISTEMA DE DISTRIBUICAO. . . . .... .
I F  EN	=   6 TH EN
PRINTER . PRINT  "A r vo r e   de   o . C .   com  AL PICOAT— CT “


PVC	AE "

EL 5 E


PRINTER . PRINT  T (U ,    20J	; I F Rev% = 0 THEN
PRINTER . PRINT  "  — AE"

EL S E
PRI	NTER . PRINT  ""
END I F END I F
PRI  NTER . PRINT   TAB  10J ;    "VENTILADOR	” ;
PRINTER. PRINT TAB(49) ; T(M, 21)
IP ((RIGHT$(T(M, 21	E18J, 3) = ”PRF”j OR (RIGHT$(T(M, 21 + E18), Il) ”POLIURETANO")) AND (Rev% = 0) THEN
PRINTER.PRINT TAB(49J; ”PAS DE PRF-AE”

ELS E


PRINTER . PRINT    TAB   49)  ;   T	LI ,     Z 1   +    E 18

END IF
PRI NTER . PRINT
I F	S {M , 18)	— 2 AND PI LARETE <> 0	THEN
PRINTER . PRINT TAB (6} ;  ” (”g	P reve r P1 I a r ete	cd  c1on aI  ac ma do N v e	c! a Ba cJ a de " ; PI LAR ETE ; " m "
END I F
PRINTER.PRINT TAB(4); "	PRF = POLIESTER REFORCADO COM FIBRA DE VlOROi”:
PRI  NTER . PRINT  "  AE  =   ' AUTO - EXTINGUIV EL ' “
I F Tu do Bem%   =   0   THEN
PRINTER. PRINT TAB(4);  ”(""j SISTEMA DE DISTRIBUICAO ESPECIAL. FAVOR
CONSULTAR A ALPINA.” END IF
'PRINTER. PRIN* PRINTER. PRINT CHR%(12) PRI NTER . ENDDOC
EXIT SUB
Ch eckEn ro r :
I F	( ERR	-	2 5	AND co ntaer r   <   2  THEN
MSGBOX   " Imp re s s o ra    de s co nectada  ou sem   pa pe1  . "   •    CH R $ ( IOJ	+  CH R $ ( T 3	r
corrija o problema e continue a execucao
con t aer r   =  contae r r	1
RESUME
OLSEIE contaerr,	1 THEN END ELSE

PRINT " E r ro n ume ro	’	STRS ( ERRg END	' A1 n al	za o p rog rama
E NO  I  F

END S UB

Pàgind 29

ERROR $


SUB TemBS (L8, L7, L 6)

al p1na2

’ SUB- ROTINA PARA CAL CULO DA TBS EM FUNCAO DE TBU E UR
’ VARIAVEI S DE ENTRADA : L8=TEMPERATURA DE BULBO UMZDO (  ’ C)
'	L 7=UMIDADE   RELATIVA    {%}
'	L6=PRESSAO ATNOS FERICA (rrba r)
'VARIAVEIS DE SAIDA: L9=TEMPERATURA DE BULBO SECO ('C)
'	P 'i= PRESSAO   DE  VAPOR   SATURADO   CON   BASE   NA   TBU ' VARIAVEIS USADAS : C0=COEFICZENTE
’	I1=INCREMENTO  ( ' Cg   NA  ITERACAO   PARA   CALCULO   DE  TBS
’	I2=PRECISAO  NA  ITE   RACAO  PARA   DETERNINACAO   DE   TBS
’	L9=TEMPERATURA   DE    BULBO    SECO	’ C)
’	L10=TENPERATURA   DO   AR	’ C}
IF	L8	< 0   THEN
C0  =  . 573

ELSE

C0 = . 667

END   I F
11 = .1: 12 = .2: Ll0 = L8
D0 WHILE 11 > .001
FOR L9 = L10 TO L10 + 30 STEP Z1
PA = 6. 107 S + 10 ^ ( (7. 4412 - ((67 — L8) / 463) ^ 2J * L8 / (23 5 +

L8J)

DELTA = 10 ^ ((7.4412 - ((67 - L9J / 463) ^ 2) L9 / (235 + L9)) +

{C0   *  L 6  *  100J  /  (1006. 7  *  6. 107 5  *   L7}	*  L9	-   (P5	*   100)   /   (6. 1075   °   L7)	- (C0
' L6 * 100 * L8) / (1006. 7 * 6. 107 5 ° L 7)
IF ABS {DELTA} < z 2 THEN EXIT FOR
END Z F
NEXT L9
L10 = L9 : I1 = 11 /  10 :  12  =  I2   /  10
LOOP
ZF   Lg   =   L 10   +   30   THEN
PRINT   " ERRO NO CALCULO DE   TBU" :   STOP
END IF END SUB
SUB TemBU (L9, L7, L6J
'SUB-ROTINA PAFtA CALCULO DA TBU EM FUNCAO DE TBS E UR 'VARIAVEIS DE ENTRADA: L9=TEMPERATURA DE BULBO SECO ('C) ’	L 7=UNZDADE RELATIVA (B)
’	L6=PRESSAO ATNOS FERICA (mba r) 'VARIAVEIS DE SAIDA: L8=TEMPERATURA DE BULBO UNIDO ('C) ’ VARIAVEIS USADAS : A0=COEFICIENTE
'	z1=INCRENENTO  É ’C)   NA  ITERACAO  PARA  CAL CULO  DE  TBU
'	I2=PRECISAO NA ITERACAO PARA DETERMINACAO DE TBU
'	L8=TENPERATURA DE BULBO UNIDO	’ C}
’	L9=TEMPERATURA DE BULBO S ECO	’ CQ
’	L10=TENPERATURA DO AR ( ' C)
I F L 8 < 0 THEN
A0 = . 573

ELSE

A0 = . 667





L9))
L 8) )

END I F
I1 = . 1 : 12 = . 2 :   L10 = L9
DO h/HZLE I1	> . 00!
FOR  L8  =  L10   TO  L10   -   30  STEP  11
P9 = 6. 1075 * 10 ^   C(7. 4412   -   € (67	L9J	/ 463)   ^ 2)   "   L9   /   €23 5 +
P4  =  6. 1075   *  10	( (7. 4412  -   ((67  — L8J	/	463J	2)  '	L8 / €23 5 + DELTA  -   P9  "" L 7 /  100	—  ÊP4  — AO *  ÊL9	L8}		* L 6 / 1006. 7)
IF	ABS {DELTA)  <  12  THEN
EXIT FOR
END IF
NEXT L8
L10 = L 8 : T1 = I1   /   10: I2   = I2   /   10

LOOP
IF	L8 = LI0	— 30 THEN



Página 30

al p1na2
PRINT   "ERRO  NO  CA LCU LO  DE   TBS " :   STOP
END IF
END   S UB

SUB TENP1  (}
' SUB— ROTINA PARA   CAL CULAR   A   T E AIPE RATURA   DO
'	AR SATURADO   EM KUNCAO DA   SUA   ENTAL PIA
'VARIAVEIS DE ENTRADA: K9-KNTALPIA DO AR SATURADO
'	L8=TEMPERATURA     IN  FERIOR	A DES E 3 ADA
L6=PRESSAO   ATMOS FERICA   ¢mbar }
' VAR IAV  E I S   D E   SAIDA :   K8=TEMPERATURA  DO   AR   SATURADO   ( ’ C) ' VAR IAV  E IS	QUE USA : DELTA=
P4=






K 8}

11 = 1
I2 = 4
UO = L8
DO GHZ LE 11  >  . 001
FOR K8  =  L 10  TO  L 10  +  30  STEP  11
P4 = 6. 107 5  ^ 10 ^ ( (7 . 4412 -  ( (67 — K8)	/  463	^ 2)   "   K 8   /   (23 5 •
DELTA = K9 —  ( 1.  006 * K8 +   . 62 2   * P4   /	L 6  —  P4)	”  L25 01. 6 -r  t . 9C "
I F    ÇABS ÇDE LTA$   <   12	OR  ÇDE LTA	< 0J THE N EXIT FOR
END I F NEXT K8
L10 = ‹8 - 11: 11	Il / 10: 12 = i2 / 10

LOOP
END   5 UB

FUNCTION Teo rAg ua (P7 , L 6J
’ CALC ULO DO TEOR 0 E AGUA NO AR UMIDO
'VARIAVEIS DE ENTRADA: L6=PRESSAO ATMOSFERICA (mbar)
P 7= PR ES SAO PARCIA L DE VAPOR NO AR UNIIDO	C mb a r )
' SAIDA : L 3 = TEOR DE AGUA   NO AR UMIDO   ( kg/ k g
L 3 =  . 62 2 ”  PA /	L 6 -   P7}
Teo rAg ua = L 3
END FUNCTION

SU B   TOL	{)
’ SU B — ROTINA PARA   CAL CULO    DA   TOL E RANCIA ' VAR I AV E I S D E ENTRADA :

' MARIAV  E I S  OE  SAIDA : TI — E 3
U9 - D
’ DS = 100
D	= D U8 = E 5 T7   -	. 4
I K D 5 < 100	THEN
V9 - - 1
T7  =  T7 '	V9
E LSE
V9	1
END  I F
DO

Descrd cao : Di <s nu1 cao do decremento de T2 para hai xor aooroaches
at  raves  da rre d  a  com  a  TBU
PO P  ”   C 0M	Dat a  : 1 5 . I2 . 94
V9   -    - 1 AND   T2   ‹—   E8   +   . 4   THEN T2 — (+2	+ E 8	/ 2
TO =   T2   +   E 3   -   E §

ELSE



Pág1 na 31

al pJ na2
T2  -   T2   +   T7
END I F
I F V9 = — 1 AND T2 < E8 +  . 0 5 THEN U7 = E8 — E 5
al ert a to	=   —1
EXIT DO END IF












LOOP

v8 -1
END I F
IF	V8 <> v9 THEN
EXIT DO END I F
iJ 8 = T2 U9 = D

I F at e rtatol  <> - 1 THEN
u6 -	(u8 - T2) /   (U9 — D)
U S = U8 - U6 ” U9
U 7 - U 6 “ 100 + U §	E 5
END I F T1 = E 3 T2 = E 5
BE RK EL PRO
PERK E L ENCH
D =   100	” K / K 3
END  S UB

FUNCTION UR (P5, P?)
'CALCULO DA UMIDADE RELATIVA DO AR
'VARIAVEIS DE ENTRAOA: PS=PRESSAO DE VAPOR SATURADO, COM BASE TBS
P Z= PR E S SAO   PARCIA L   DE  VAPOR   (mba r ) ’ SAIDA :   =UMIDAO E   RELATIVA  DO  AR   {%}
L7 =   PA   /   P 5   "   100
UR = L 7 END FUNCTION


SUB USUAL  () VARIAVEIS DE ENTRADA
'VARIAvEIS DE SAIDA:



E2= E3= E 5=
E 7= E8= EI6= EI8= E20-
E2 1—NUMERO MINIMO DE CELULAS ENCH=NON E DO ENCHINENTO
CLI-RAZAO SOCIAL DO CLIENTE NOM— NOME DA P ES SOA PARA CONTATO FAX=FAX DA PES SOA PARA CONTATO
E L -T   E L E FON E    DA    P E S S OA    PA RA    CONTA TO




END

OPE N    " I  "  ,    # 5 ,     " U SUA L . DAT "
T N P UN     # 5 ,     E 2 ,     E 3 ,     E 5 ,     E 7 ,     F 8 ,     z I C ,     E   B ,	2 O ,     E 2 I   ,     E 2 2 , D   a   S
I N PUT	# 5 ,    E NC H ,    Ro	d O% ,    R e vÜ ,    C l. I   ,    NOU ,    FAX   , T E L C LO S E    # 5
S UB



SUB

VENTILADOR (j
A1 e rtü%  =   0 13 = E 18 • 1




Púg1na 3 2


Rev	s ao	: 2
' Descricao	O calculo de Nv deve ser realizado apenas quando nao

SPL    =   10   “	LOG {UA	4   ”   NV   /    S {N ,    7$    ”   NP§    /   LOG ( 10)	— 5
SPL	=   INT	S P L   +   . S
I F   T (< ,   13	= " 8 EM2 "  TH EN  S P L  =  S P L  —  3
E LS E I F    (T{M ,    13	=  " VAP   ”)   OR   (T {M ,   13	=   "VA  L" )   TH EN
I F  ¿V 5 >=  VAP  S {PI , 13	+ 17)	,   7)   )    AND   (A1 e rta%  =  0J   THEN
CA L CULAVA P
NV   =    VA P (S {M ,    13    +   17J   ,   6)	/   ET
' >>> >>> >> >>>   E '    USADO   O   S PL    A   2m—5 P L    A   1m— 3   <<< <<<<< <<<<< <<
UA   =   ROTV EN   /   60   ”   3 . 14 15 9   *   s (M ,    7 g	' VE LOCIDADE PERIFERICA AN =  VAP {S (M ,   15	+ 17}	, 8J
S PL	— 10	*	LOG (!JA	4 "   NV   /   S (M ,   7)	*   NP}	/   LoG ( 101 )	3
S PL    =   I NT    S P L	. 5 )
ELS EI F  (V 5 <  VA P (S (II ,  13  +  17)  ,  7)	AND   (ml e rta%   =  0J   THEN

E L S E


SD =   1 . 5   °   D4
ml erta% = — 1

NV = MOT /  1 . 1 SD = 1. 5  "  D4 STOP

END I F
END  I F
NV1  =   MOT   /   NV
END   S UB


























Pãg na 3 3