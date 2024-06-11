'*****************************************************************************************************
'
' ***********************************************************************
'                           Mr Doom
'							 ****
'                     *** SYSTEM III *** 
'						script version
'							
'
'  Mr. Doom / IPD No. 1637 / January, 1979 / 4 Players
'  VPX version by NestorGian. Build in 2021, version 1.3
'  
'				
' ***********************************************************************
 
Option Explicit

'First, try to load the Controller.vbs (DOF), which helps controlling additional hardware like lights, gears, knockers, bells and chimes (to increase realism)
'This table uses DOF via the 'SoundFX' calls that are inserted in some of the PlaySound commands, which will then fire an additional event, instead of just playing a sample/sound effect

Const BallSize = 25   ' 50 is the normal size
Const BallMass = 1.7  ' JP's Arcade Physics V3.0


On Error Resume Next
ExecuteGlobal GetTextFile("core.vbs")
If Err Then MsgBox "Can't open core.vbs"
On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "Can't open controller.vbs"



'*****************************************************************************************************
'									CONFIGURATION/CONFIGURACION
'*****************************************************************************************************
'
Const BallsPerGame = 3 	' normalmente 3 o 5 / usually 3 or 5
'
'*****************************************************************************************************
'
Const Special1 = 550000  ' puntuación a obtener para Bola extra
Const Special2 = 750000  ' puntuación a obtener para Special
Const Special3 = 920000  ' puntuación a obtener para Special
'
'*****************************************************************************************************



'both for VPM, and DOF to load the right DOF config from the Configtool, whether it's a VPM or an Original table

Const cGameName = "MrDoom_1979"
Const TableName = "MrDoom_1979"
Const MaxPlayers = 4    ' de 1 a 4
Const MaxMultiplier = 5 ' limita el bonus multiplicador a 5

' Variables Globales
Dim Score(4)
Dim HighScore
Dim Credits
Dim PlayersPlayingGame
Dim CurrentPlayer
Dim BallsRemaining(4)
Dim ExtraBallsAwards(4)
Dim Special1Awarded(4)
Dim Special2Awarded(4)
Dim Special3Awarded(4)
Dim Special4Awarded(4)
Dim LightP1(4)
Dim LightP2(4)
Dim LightP3(4)
Dim LightP4(4)
Dim Match
Dim Tilt
Dim TiltSensitivity
Dim Tilted
Dim Add10
Dim Add100
Dim Add1000
Dim Add10000
Dim Bonus
Dim DBonus
Dim TBonus
Dim DoubleBonus
Dim LRoad
Dim StarCount
Dim SwordCount
Dim NewTone
Dim FPlays
Dim LBankDown
Dim RBankDown


' Variables de control
Dim BallsOnPlayfield

' Variables de tipo Boolean (verdadero Ã³ falso, True Ã³ False)
Dim bAttractMode
Dim bFreePlay
Dim bGameInPlay
Dim bOnTheFirstBall
Dim bExtraBallWonThisBall
Dim bJustStarted
Dim bBallInPlungerLane
Dim bBallSaverActive
Dim PaySpecial




' *********************************************************************
'                Rutinas comunes para todas las mesas
' *********************************************************************




Sub Table1_Init()
	Dim x
    ' Inicializar diversos objetos de la mesa, como droptargets, animations...
    VPObjects_Init
	LoadEM
    ' Carga los valores grabados highscore y crÃ©ditos si no hay HS pone uno
    Loadhs
	If HighScore = 0 then
		HighScore = 300000
		Savehs
	End If
	
'    HighscoreReel.SetValue Highscore
	' tambiÃ©n pon el highscore en el reel del primer jugador
	ScoreReel1.AddValue Highscore
	SetHSLine 2,Highscore
'	If B2SOn then
'		Controller.B2SSetScorePlayer1 HighScore
'		AddScore 100
'	End If
    UpdateCredits



    ' Juego libre o con monedas: si es True entonces no se usarÃ¡n monedas
    bFreePlay = False 'queremos monedas

    ' Inicialiar las variables globales de la mesa
    bAttractMode = False
    bOnTheFirstBall = False
    bGameInPlay = False
    bBallInPlungerLane = False
    BallsOnPlayfield = 0
    Tilt = 0
    TiltSensitivity = 6
    Tilted = False
	Match = 0
    bJustStarted = True
	PaySpecial = False
    Add10 = 0
    Add100 = 0
    Add1000 = 0
	SwordCount = 1
	StarCount = 0
	FPlays = 0

    ' pone la mesa en modo de espera
	ResetNewBallLights()
    EndOfGame

    'Enciende las luces GI despues de un segundo
'    vpmtimer.addtimer 1000, "GiOn '"

    ' Quita los laterales y las puntuaciones cuando la mesa se juega en modo FS
    If Table1.ShowDT = False then
'        lrail.Visible = False
'        rrail.Visible = False
'		 Scoretext.Visible = false
		For each x in aReels
			x.Visible = 0
		Next
		For each x in aMatchLights
			x.Visible = 0
		Next
		Else
		For each x in aMatchLights
			x.Visible = 1
		Next
		For each x in aReels
			x.Visible = 1
		Next
    End If

	If Credits > 0 Then DOF 118, DOFOn

	LoadLUT
End Sub


Dim EnableBallControl
EnableBallControl = false 'Change to true to enable manual ball control (or press C in-game) via the arrow keys and B (boost movement) keys





'******
' Keys
'******


Sub Table1_KeyDown(ByVal keycode)

	' aÃ±ade monedas
    If Keycode = AddCreditKey Then
        If(Tilted = False) Then
            AddCredits 1
            PlaySound "fx_coin"
			DOF 118, DOFOn
		End If
    End If

	If keycode = PlungerKey Then
		Plunger.PullBack
		PlaySoundAt "fx_plungerpull", plunger
	End If

' iluminaciÃ³n
		If keycode = LeftMagnaSave Then bLutActive = True
	If keycode = RightMagnaSave Then 
		If bLutActive Then NextLUT: End If
	End If
		

	If bGameInPlay AND NOT Tilted Then
        ' teclas de la falta
        If keycode = LeftTiltKey Then Nudge 90, 8:PlaySound "fx_nudge", 0, 1, -0.1, 0.25:CheckTilt
        If keycode = RightTiltKey Then Nudge 270, 8:PlaySound "fx_nudge", 0, 1, 0.1, 0.25:CheckTilt
        If keycode = CenterTiltKey Then Nudge 0, 9:PlaySound "fx_nudge", 0, 1, 1, 0.25:CheckTilt
		If keycode = MechanicalTilt Then Tilt = 10: CheckTilt

        ' teclas de los flipers
		If keycode = LeftFlipperKey Then SolLFlipper 1
        If keycode = RightFlipperKey Then SolRFlipper 1


        ' tecla de empezar el juego
        If keycode = StartGameKey Then
            If((PlayersPlayingGame <MaxPlayers) AND(bOnTheFirstBall = True) ) Then

                If(bFreePlay = True) Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                    PlayersReel.SetValue, PlayersPlayingGame
                    PlaySound "start"
                Else
                    If(Credits> 0) then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        Credits = Credits - 1
                        UpdateCredits
                        UpdateBallInPlay
						PlaySound "start"
						If(Credits= 0)then DOF 118,DOFOff
                    Else
                        ' no hay suficientes crÃ©ditos para empezar el juego.
                        PlaySound "error"
						DOF 118, DOFOff
                    End If
                End If
            End If
        End If
        Else ' If (GameInPlay)

            If keycode = StartGameKey Then
                If(bFreePlay = True) Then
                    If(BallsOnPlayfield = 0) Then
						ResetScores
                        ResetForNewGame()
						PlaySound "start"
                    End If
                Else
                    If(Credits> 0) Then
                        If(BallsOnPlayfield = 0) Then
                            Credits = Credits - 1
                            UpdateCredits
							ResetScores
                            ResetForNewGame()
							PlaySound "start"
							If(Credits= 0)then DOF 118,DOFOff
                        End If
                    Else
                        ' Not Enough Credits to start a game.
                        PlaySound "error"
						DOF 118, DOFOff
                    End If
                End If
            End If
    End If ' If (GameInPlay)



    ' Manual Ball Control
	If keycode = 46 Then	 				' C Key
		If EnableBallControl = 1 Then
			EnableBallControl = 0
		Else
			EnableBallControl = 1
		End If
	End If
    If EnableBallControl = 1 Then
		If keycode = 48 Then 				' B Key
			If BCboost = 1 Then
				BCboost = BCboostmulti
			Else
				BCboost = 1
			End If
		End If
		If keycode = 203 Then BCleft = 1	' Left Arrow
		If keycode = 200 Then BCup = 1		' Up Arrow
		If keycode = 208 Then BCdown = 1	' Down Arrow
		If keycode = 205 Then BCright = 1	' Right Arrow
	End If
End Sub



Sub Table1_KeyUp(ByVal keycode)

	If bGameInPlay AND NOT Tilted Then
		' teclas de los flipers
        If keycode = LeftFlipperKey Then SolLFlipper 0
        If keycode = RightFlipperKey Then SolRFlipper 0
	End If

	If keycode = PlungerKey Then
		Plunger.Fire
		If bBallInPlungerLane Then
            PlaySoundAt "fx_plunger", plunger
        Else
            PlaySoundAt "fx_plunger_empty", plunger
        End If
	End If

    'Manual Ball Control
	If EnableBallControl = 1 Then
		If keycode = 203 Then BCleft = 0	' Left Arrow
		If keycode = 200 Then BCup = 0		' Up Arrow
		If keycode = 208 Then BCdown = 0	' Down Arrow
		If keycode = 205 Then BCright = 0	' Right Arrow
	End If
End Sub


'*********
'   LUT
'*********

Dim bLutActive, LUTImage
Sub LoadLUT
	bLutActive = False
    x = LoadValue(cGameName, "LUTImage")
    If(x <> "") Then LUTImage = x Else LUTImage = 0
	UpdateLUT
End Sub

Sub SaveLUT
    SaveValue cGameName, "LUTImage", LUTImage
End Sub

Sub NextLUT: LUTImage = (LUTImage +1 ) MOD 9: UpdateLUT: SaveLUT: End Sub

Sub UpdateLUT
Select Case LutImage
Case 0: table1.ColorGradeImage = "LUT0"
Case 1: table1.ColorGradeImage = "LUT1"
Case 2: table1.ColorGradeImage = "LUT2"
Case 3: table1.ColorGradeImage = "LUT3"
Case 4: table1.ColorGradeImage = "LUT4"
Case 5: table1.ColorGradeImage = "LUT5"
Case 6: table1.ColorGradeImage = "LUT6"
Case 7: table1.ColorGradeImage = "LUT7"
Case 8: table1.ColorGradeImage = "LUT8"
End Select
End Sub



Sub Drain_Hit()
	Drain.DestroyBall
	BallsOnPlayfield = BallsOnPlayfield - 1
	PlaySoundAt "fx_drain", Drain
	DOF 115, DOFPulse
'	RecordPLights
	GiOff

    ' si estÃ¡s jugando y no hay falta
    If(bGameInPLay = True) AND(Tilted = False) Then

        ' Â¿estÃ¡ el salva bolas activado?
        If(bBallSaverActive = True) Then
            ' Â¿sÃ­?, pues creamos una bola
            CreateNewBall()
        Else
            ' Â¿es Ã©sta la Ãºltima bola en juego?
            If(BallsOnPlayfield = 0) Then
                vpmtimer.addtimer 500, "EndOfBall '" 'hacemos una pequeÃ±a pausa anter de continuar con el fin de bola
				Exit Sub
            End If
        End If
    End If
End Sub


Dim BIP
BIP = 0

'Sub Plunger_Init()
'	PlaySound SoundFX("ballrelease",DOFContactors), 0,1,AudioPan(BallRelease),0.25,0,0,1,AudioFade(BallRelease)
'	'Plunger.CreateBall
'	BallRelease.CreateBall
'	BallRelease.Kick 90, 7
'	BIP = BIP + 1
'End Sub

Sub swPlungerRest_Hit()
    bBallInPlungerLane = True
End Sub

' La bola ha sido disparada, asÃ­ que cambiamos la variable, que en esta mesa se usa solo para que el sonido del disparador cambie segÃºn hay allÃ­ una bola o no
' En otras mesas podrÃ¡ usarse para poner en marcha un contador para salvar la bola

Sub swPlungerRest_UnHit()
    bBallInPlungerLane = False
End Sub

'*******************
' Flipper Subs v3.0
'*******************

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFXDOF("fx_flipperup",101,DOFOn,DOFFlippers), LeftFlipper
        LeftFlipper.EOSTorque = 0.75:LeftFlipper.RotateToEnd
    Else
        PlaySoundAt SoundFXDOF("fx_flipperdown",101,DOFOff,DOFFlippers), LeftFlipper
        LeftFlipper.EOSTorque = 0.1:LeftFlipper.RotateToStart
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFXDOF("fx_flipperup",102,DOFOn,DOFFlippers), RightFlipper
        RightFlipper.EOSTorque = 0.75:RightFlipper.RotateToEnd
    Else
        PlaySoundAt SoundFXDOF("fx_flipperdown",102,DOFOff,DOFFlippers), RightFlipper
        RightFlipper.EOSTorque = 0.1:RightFlipper.RotateToStart
    End If
End Sub

' el sonido de la bola golpeando los flipers

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub



'**************************************************
' Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************************************************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
    PlaySoundAt SoundFXDOF("fx_slingshot",104,DOFPulse,DOFContactors), Sling1
	DOF 114, DOFPulse
	Addscore 100
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.rotx = 20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
	gi1.State = 0:Gi2.State = 0
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.rotx = 10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.rotx = 0:RightSlingShot.TimerEnabled = 0:gi1.State = 1:Gi2.State = 1
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
    PlaySoundAt SoundFXDOF("fx_slingshot",103,DOFPulse,DOFContactors), Sling2
	DOF 113, DOFPulse
	Addscore 100
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.rotx = 20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
	gi3.State = 0:Gi4.State = 0
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.rotx = 10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.rotx = 0:LeftSlingShot.TimerEnabled = 0:gi3.State = 1:Gi4.State = 1
    End Select
    LStep = LStep + 1
End Sub


'**************************************************
' Captive BallL Animations
' 
'****************************************************

Dim RCStep, LCstep

Sub LeftCaptiveTrigger_Hit
	Captive1.rotx = 5
    LCStep = 0
    LeftCaptiveTrigger.TimerEnabled = 1
End Sub
   
Sub LeftCaptiveTrigger_Timer
	Select Case LCStep
        Case 1:Captive1.rotx = 0:LeftCaptiveTrigger.TimerEnabled = 0
    End Select
    LCStep = LCStep + 1
End Sub

Sub RightCaptiveTrigger_Hit
	Captive2.rotx = 5
    RCStep = 0
    RightCaptiveTrigger.TimerEnabled = 1
End Sub
   
Sub RightCaptiveTrigger_Timer
	Select Case RCStep
        Case 1:Captive2.rotx = 0:RightCaptiveTrigger.TimerEnabled = 0
    End Select
    RCStep = RCStep + 1
End Sub




'*******************
' Luces GI
'*******************

Sub GiOn 'enciende las luces GI
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 1
    Next
End Sub

Sub GiOff 'apaga las luces GI
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 0
    Next
	DTLOff
End Sub

Sub DTLOff
	DTRi1.State = 0
	DTRi2.State = 0
	DTRi3.State = 0
	DTRi4.State = 0
	DTRi5.State = 0
	DTRi6.State = 0
	Gi12.State = 0
	Gi41.State = 0
	DTLi1.State = 0
	DTLi2.State = 0
	DTLi3.State = 0
	DTLi4.State = 0
	DTLi5.State = 0
	DTLi6.State = 0
	Gi35.State = 0
	Gi40.State = 0
End Sub

'**************
' TILT - Falta
'**************

'el "timer" TiltDecreaseTimer resta .01 de la variable "Tilt" cada ronda

Sub CheckTilt                            'esta rutina se llama cada vez que das un golpe a la mesa
	if CurrentPlayer = 0 then exit Sub
    Tilt = Tilt + TiltSensitivity        'aÃ±ade un valor al contador "Tilt"
    TiltDecreaseTimer.Enabled = True
    If Tilt> 15 Then                     'Si la variable "Tilt" es mÃ¡s de 15 entonces haz falta
		PlaySound SoundFXDOF("fx_knocker",133, DOFPulse,DOFKnocker)
        Tilted = True
        TiltReel.SetValue 1              'muestra Tilt en la pantalla
		If B2SOn then
			Controller.B2SSetTilt 1
		end if
        DisableTable True
        TiltRecoveryTimer.Enabled = True 'empieza una pausa a fin de que todas las bolas se cuelen
    End If
End Sub

Sub TiltDecreaseTimer_Timer
    If Tilt> 0 Then
        Tilt = Tilt - 0.1
    Else
        TiltDecreaseTimer.Enabled = False
    End If
End Sub

Sub DisableTable(Enabled)
    If Enabled Then
		'Apaga todas las luces Gi de la mesa
		GiOff
        'Disable slings, bumpers etc
        LeftFlipper.RotateToStart
        RightFlipper.RotateToStart
        Bumper001.Force = 0
        Bumper002.Force = 0
        LeftSlingshot.Disabled = 1
        RightSlingshot.Disabled = 1
    Else
        'enciende de nuevo todas las luces GI
        GiOn
        Bumper001.Force = 10
        Bumper002.Force = 10
        LeftSlingshot.Disabled = 0
        RightSlingshot.Disabled = 0
    End If
End Sub

Sub TiltRecoveryTimer_Timer()
    ' si todas las bolas se han colado, entonces ..
    If(BallsOnPlayfield = 0) Then
        '... haz el fin de bola normal
        EndOfBall()
        TiltRecoveryTimer.Enabled = False
    End If
' de lo contrario la rutina mirarÃ¡ si todavÃ­a hay bolas en la mesa
End Sub

'****************************************
' Inicializa la mesa para un juego nuevo
'****************************************

Sub ResetForNewGame()
    'debug.print "ResetForNewGame"
    Dim i

    bGameInPLay = True
    bBallSaverActive = False
	PaySpecial = False
	FPlays = 0
    'pone a cero los marcadores y apaga las luces de espera.
    StopAttractMode
	If B2SOn then
		Controller.B2SSetGameOver 0
'		Controller.B2SSetScorePlayer1 HighScore
	end if
    ' enciende las luces GI si estuvieran apagadas
    GiOn

	MatchWheel.Enabled = true

	CurrentPlayer = 1
    PlayersPlayingGame = 1
    bOnTheFirstBall = True
    For i = 1 To MaxPlayers
        Score(i) = 0
        ExtraBallsAwards(i) = 0
		LightP1(i) = 0
		LightP2(i) = 0
		LightP3(i) = 0
		LightP4(i) = 0
        Special1Awarded(i) = False
        Special2Awarded(i) = False
        Special3Awarded(i) = False
		Special4Awarded(i) = False
        BallsRemaining(i) = BallsPerGame
    Next
    DoubleBonus = 1
    Bonus = 0
    UpdateBallInPlay

    Match = INT(RND(1) * 8) * 10 + 10 'empieza con un nÃºmero aleatorio de 10 a 90 para la loteria
    Clear_Match

    ' inicializa otras variables
    Tilt = 0

    ' inicializa las variables del juego
    Game_Init()

    ' ahora puedes empezar una mÃºsica si quieres
    ' empieza la rutina "Firstball" despues de una pequeÃ±a pausa
    vpmtimer.addtimer 2000, "FirstBall '"
End Sub

' esta pausa es para que la mesa tenga tiempo de poner los marcadores a cero y actualizar las luces

Sub FirstBall
    'debug.print "FirstBall"
    ' ajusta la mesa para una bola nueva, sube las dianas abatibles, etc
    ResetForNewPlayerBall()
    ' crea una bola nueva en la zona del plunger
    CreateNewBall()
End Sub

' (Re-)inicializa la mesa para una bola nueva, tanto si has perdido la bola, oe le toca el turno al otro jugador

Sub ResetForNewPlayerBall()
    'debug.print "ResetForNewPlayerBall"
    ' Se asegura que los marcadores estÃ¡n activados para el jugador de turno
    AddScore 0

    ' ajusta el multiplicador del bonus multiplier a 1X (si hubiese multiplicador en la mesa)

    ' enciende las luces, reinicializa las variables del juego, etc
	bExtraBallWonThisBall = False    
    ResetNewBallVariables
	ResetNewBallLights
	FPReel.SetValue FPlays
End Sub

' Crea una bola nueva en la mesa

Sub CreateNewBall()
    ' crea una bola nueva basada en el tamaÃ±o y la masa de la bola especificados al principio del script
    BallRelease.CreateSizedBallWithMass BallSize, BallMass
	PlaySoundAt SoundFXDOF("fx_Ballrel",110,DOFPulse,DOFContactors), BallRelease

    ' incrementa el nÃºmero de bolas en el tablero, ya que hay que contarlas
    BallsOnPlayfield = BallsOnPlayfield + 1

    ' actualiza las luces del backdrop
    UpdateBallInPlay

    ' y expulsa la bola	
 	PlaySoundAt "fx_Ballrel", BallRelease
    BallRelease.Kick 90, 4

End Sub

' El jugador ha perdido su bola, y ya no hay mÃ¡s bolas en juego
' Empieza a contar los bonos

Sub EndOfBall()
    'debug.print "EndOfBall"
    Dim AwardPoints, TotalBonus, ii
    AwardPoints = 0
    TotalBonus = 0
    ' La primera se ha perdido. Desde aquÃ­ ya no se puede aceptar mÃ¡s jugadores
    bOnTheFirstBall = False

    ' solo recoge los bonos si no hay falta
    ' el sistema del la falta se encargarÃ¡ de nuevas bolas o del fin de la partida

    If NOT Tilted Then
		If DoubleBonus = 2 Then
			BonusCountTimer.Interval = 400
		Else
			BonusCountTimer.Interval = 250
		End If
        BonusCountTimer.Enabled = 1
    Else 'Si hay falta simplemente espera un momento y va directo a la segunta parte despuÃ©s de perder la bola
        vpmtimer.addtimer 400, "EndOfBall2 '"
    End If
End Sub



Sub BonusCountTimer_Timer 'aÃ±ade los bonos y actualiza las luces
    'debug.print "BonusCount_Timer"
    If Bonus> 0 Then
        Bonus = Bonus -1
        AddScore 10000 * DoubleBonus
        UpdateBonusLights
    Else
        ' termina la cuenta de los bonos y continÃºa con el fin de bola
        BonusCountTimer.Enabled = 0
        vpmtimer.addtimer 1000, "EndOfBall2 '"
    End If
End Sub





Sub UpdateBonusLights 'enciende o apaga las luces de los bonos segÃºn la variable "Bonus"
	Select Case Bonus
        Case 0:LiBo1.State = 0:LiBo2.State = 0:LiBo3.State = 0:LiBo4.State = 0:LiBo5.State = 0:LiBo6.State = 0:LiBo7.State = 0:LiBo8.State = 0:LiBo9.State = 0:LiBo10.State = 0
        Case 1:LiBo1.State = 1:LiBo2.State = 0:LiBo3.State = 0:LiBo4.State = 0:LiBo5.State = 0:LiBo6.State = 0:LiBo7.State = 0:LiBo8.State = 0:LiBo9.State = 0:LiBo10.State = 0
        Case 2:LiBo1.State = 0:LiBo2.State = 1:LiBo3.State = 0:LiBo4.State = 0:LiBo5.State = 0:LiBo6.State = 0:LiBo7.State = 0:LiBo8.State = 0:LiBo9.State = 0:LiBo10.State = 0
        Case 3:LiBo1.State = 0:LiBo2.State = 0:LiBo3.State = 1:LiBo4.State = 0:LiBo5.State = 0:LiBo6.State = 0:LiBo7.State = 0:LiBo8.State = 0:LiBo9.State = 0:LiBo10.State = 0
        Case 4:LiBo1.State = 0:LiBo2.State = 0:LiBo3.State = 0:LiBo4.State = 1:LiBo5.State = 0:LiBo6.State = 0:LiBo7.State = 0:LiBo8.State = 0:LiBo9.State = 0:LiBo10.State = 0
        Case 5:LiBo1.State = 0:LiBo2.State = 0:LiBo3.State = 0:LiBo4.State = 0:LiBo5.State = 1:LiBo6.State = 0:LiBo7.State = 0:LiBo8.State = 0:LiBo9.State = 0:LiBo10.State = 0
        Case 6:LiBo1.State = 0:LiBo2.State = 0:LiBo3.State = 0:LiBo4.State = 0:LiBo5.State = 0:LiBo6.State = 1:LiBo7.State = 0:LiBo8.State = 0:LiBo9.State = 0:LiBo10.State = 0
        Case 7:LiBo1.State = 0:LiBo2.State = 0:LiBo3.State = 0:LiBo4.State = 0:LiBo5.State = 0:LiBo6.State = 0:LiBo7.State = 1:LiBo8.State = 0:LiBo9.State = 0:LiBo10.State = 0
        Case 8:LiBo1.State = 0:LiBo2.State = 0:LiBo3.State = 0:LiBo4.State = 0:LiBo5.State = 0:LiBo6.State = 0:LiBo7.State = 0:LiBo8.State = 1:LiBo9.State = 0:LiBo10.State = 0
        Case 9:LiBo1.State = 0:LiBo2.State = 0:LiBo3.State = 0:LiBo4.State = 0:LiBo5.State = 0:LiBo6.State = 0:LiBo7.State = 0:LiBo8.State = 0:LiBo9.State = 1:LiBo10.State = 0
        Case 10:LiBo1.State = 0:LiBo2.State = 0:LiBo3.State = 0:LiBo4.State = 0:LiBo5.State = 0:LiBo6.State = 0:LiBo7.State = 0:LiBo8.State = 0:LiBo9.State = 0:LiBo10.State = 1
        Case 11:LiBo1.State = 1:LiBo2.State = 0:LiBo3.State = 0:LiBo4.State = 0:LiBo5.State = 0:LiBo6.State = 0:LiBo7.State = 0:LiBo8.State = 0:LiBo9.State = 0:LiBo10.State = 1
        Case 12:LiBo1.State = 0:LiBo2.State = 1:LiBo3.State = 0:LiBo4.State = 0:LiBo5.State = 0:LiBo6.State = 0:LiBo7.State = 0:LiBo8.State = 0:LiBo9.State = 0:LiBo10.State = 1
        Case 13:LiBo1.State = 0:LiBo2.State = 0:LiBo3.State = 1:LiBo4.State = 0:LiBo5.State = 0:LiBo6.State = 0:LiBo7.State = 0:LiBo8.State = 0:LiBo9.State = 0:LiBo10.State = 1
        Case 14:LiBo1.State = 0:LiBo2.State = 0:LiBo3.State = 0:LiBo4.State = 1:LiBo5.State = 0:LiBo6.State = 0:LiBo7.State = 0:LiBo8.State = 0:LiBo9.State = 0:LiBo10.State = 1
        Case 15:LiBo1.State = 0:LiBo2.State = 0:LiBo3.State = 0:LiBo4.State = 0:LiBo5.State = 1:LiBo6.State = 0:LiBo7.State = 0:LiBo8.State = 0:LiBo9.State = 0:LiBo10.State = 1
    End Select   
End Sub


 

' La cuenta de los bonos ha terminado. Mira si el jugador ha ganado bolas extras
' y si no mira si es el Ãºltimo jugador o la Ãºltima bola
'
Sub EndOfBall2()
    'debug.print "EndOfBall2"
    ' si hubiese falta, quÃ­tala, y pon la cuenta a cero del la falta para el prÃ³ximo jugador, Ã³ bola

    Tilted = False
    Tilt = 0
    TiltReel.SetValue 0
	If B2SOn then
		Controller.B2SSetTilt 0
	end if
    DisableTable False 'activa de nuevo los bumpers y los slingshots

    ' Â¿ha ganado el jugador una bola extra?
    If(ExtraBallsAwards(CurrentPlayer)> 0) Then
        'debug.print "Extra Ball"

        ' sÃ­? entonces se la das al jugador
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) - 1
		EBReel.SetValue ExtraBallsAwards(CurrentPlayer)
		If B2SOn then
				Controller.B2SSetScorePlayer5 ExtraBallsAwards(CurrentPlayer)
			end if
        ' si no hay mÃ¡s bolas apaga la luz de jugar de nuevo
        If(ExtraBallsAwards(CurrentPlayer) = 0) Then
            ShootAgain.SetValue 0
			LiExtraBall.State = 0
			If B2SOn then
				Controller.B2SSetShootAgain 0
				Controller.B2SSetScorePlayer5 ExtraBallsAwards(CurrentPlayer)
			end if
        End If

' aquÃ­ se podrÃ­a poner algÃºn sonido de bola extra o alguna luz que parpadee

' En esta mesa hacemos la bola extra igual como si fuese la siguente bola, haciendo un reset de las variables y dianas
        ResetForNewPlayerBall()

        ' creamos una bola nueva en el pasillo de disparo
        CreateNewBall()
    Else ' no hay bolas extras

        BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer) - 1

        ' Â¿Es Ã©sta la Ãºltima bola?
        If(BallsRemaining(CurrentPlayer) <= 0) Then

            ' miramos si la puntuaciÃ³n clasifica como el Highscore
            CheckHighScore()
        End If

        ' Ã©sta no es la Ãºltima bola para Ã©ste jugador
        ' y si hay mÃ¡s de un jugador continÃºa con el siguente
        EndOfBallComplete()
    End If
End Sub

' Esta rutina se llama al final de la cuenta del bonus
' y pasa a la siguente bola o al siguente jugador
'
Sub EndOfBallComplete()
    'debug.print "EndOfBallComplete"
    Dim NextPlayer

    'debug.print "EndOfBall - Complete"

    ' Â¿hay otros jugadores?
    If(PlayersPlayingGame> 1) Then
        ' entonces pasa al siguente jugador
        NextPlayer = CurrentPlayer + 1
        ' Â¿vamos a pasar del Ãºltimo jugador al primero?
        ' (por ejemplo del jugador 4 al no. 1)
        If(NextPlayer> PlayersPlayingGame) Then
            NextPlayer = 1
        End If
    Else
        NextPlayer = CurrentPlayer
    End If

    'debug.print "Next Player = " & NextPlayer

    ' Â¿Hemos llegado al final del juego? (todas las bolas se han jugado de todos los jugadores)
    If((BallsRemaining(CurrentPlayer) <= 0) AND(BallsRemaining(NextPlayer) <= 0) ) Then

        ' aquÃ­ se empieza la loterÃ­a, normalmente cuando se juega con monedas
        If bFreePlay = False Then
            Verification_Match
        End If

        ' ahora se pone la mesa en el modo de final de juego
        EndOfGame()
    Else
        ' pasamos al siguente jugador
        CurrentPlayer = NextPlayer

        ' nos aseguramos de que el backdrop muestra el jugador actual
        AddScore 0

        ' hacemos un reset del la mesa para el siguente jugador (Ã³ bola)
        ResetForNewPlayerBall()

        ' y sacamos una bola
        CreateNewBall()
    End If
End Sub

' Esta funciÃ³n se llama al final del juego

Sub EndOfGame()
    'debug.print "EndOfGame"
    bGameInPLay = False
    bJustStarted = False
	If B2SOn then
		Controller.B2SSetGameOver 1
		Controller.B2SSetBallInPlay 0
		Controller.B2SSetPlayerUp 0 
		Controller.B2SSetCanPlay 0
	end if
    ' asegÃºrate de que los flippers estÃ¡n en modo de reposo
	SolLFlipper 0
    SolRFlipper 0


    ' pon las luces en el modo de fin de juego
    vpmtimer.addtimer 1000, "StartAttractMode '"
End Sub
t
' Esta funciÃ³n calcula el no de bolas que quedan
Function Balls
    Dim tmp
    tmp = BallsPerGame - BallsRemaining(CurrentPlayer) + 1
    If tmp> BallsPerGame Then
        Balls = BallsPerGame
    Else
        Balls = tmp
    End If
End Function

' Esta funciÃ³n calcula el Highscore y te da una partida gratis si has conseguido el Highscore
Sub CheckHighscore
    Dim x
    For x = 1 to PlayersPlayingGame
        If Score(x)> Highscore Then
            Highscore = Score(x)
            PlaySound "fx_knocker"
'            HighscoreReel.SetValue Highscore
			SetHSLine 2,Highscore
            AddCredits 1
        End If
    Next
End Sub

Function GetHSChar(String, Index)
	dim ThisChar
	dim FileName
	ThisChar = Mid(String, Index, 1)
	FileName = "PostIt"
	if ThisChar = " " or ThisChar = "" then
		FileName = FileName & "BL"
	elseif ThisChar = "<" then
		FileName = FileName & "LT"
	elseif ThisChar = "_" then
		FileName = FileName & "SP"
	else
		FileName = FileName & ThisChar
	End If
	GetHSChar = FileName
End Function

Sub SetHsLine(LineNo, String)
	dim Letter
	dim ThisDigit
	dim ThisChar
	dim StrLen
	dim LetterLine
	dim Index
	dim StartHSArray
	dim EndHSArray
	dim LetterName
	dim xfor
	StartHSArray=array(0,1,12,22)
	EndHSArray=array(0,11,21,31)
	StrLen = len(string)
	Index = 1

'	for xfor = StartHSArray(LineNo) to EndHSArray(LineNo)
'		Eval("HS"&xfor).image = GetHSChar(String, Index)
'		Index = Index + 1
'	next
End Sub





'******************
'  Match - Loteria
'******************

Dim MatchCounter
Dim x
MatchCounter = 0

sub MatchWheel_timer
	MatchCounter=(MatchCounter+1) MOD 10

	If B2SOn then
		If MatchCounter = 0 Then
			Controller.B2SSetMatch 100
		Else
			If B2SOn then Controller.B2SSetMatch MatchCounter*10
		End If
	end if
	
	For each x in aMatchLights
		x.state=0
	next
	aMatchLights(MatchCounter).state=1		
End Sub



'Sub IncreaseMatch
'    Match = (Match + 10) MOD 100
'End Sub

Sub Verification_Match()
	MatchWheel.Enabled = False
    PlaySound "fx_match"
    Match = MatchCounter*10
    If(Score(CurrentPlayer)MOD 100) = Match Then
        PlaySound "fx_knocker"
        AddCredits 1
    End If
End Sub

Sub Clear_Match()
    For each x in aMatchLights
        x.State = 0
    Next
    If B2SOn then
        Controller.B2SSetMatch 0
    end if
End Sub









'************************************
' Diverse Collection Hit Sounds v3.0
'************************************

Sub aMetals_Hit(idx):PlaySoundAtBall "fx_MetalHit":End Sub
Sub aMetalWires_Hit(idx):PlaySoundAtBall "fx_MetalWire":End Sub
Sub aRubber_Bands_Hit(idx):PlaySoundAtBall "fx_rubber_band":End Sub
Sub aRubber_LongBands_Hit(idx):PlaySoundAtBall "fx_rubber_longband":End Sub
Sub aRubber_Posts_Hit(idx):PlaySoundAtBall "fx_rubber_post":End Sub
Sub aRubber_Pins_Hit(idx):PlaySoundAtBall "fx_rubber_pin":End Sub
Sub aRubber_Pegs_Hit(idx):PlaySoundAtBall "fx_rubber_peg":End Sub
Sub aPlastics_Hit(idx):PlaySoundAtBall "fx_PlasticHit":End Sub
Sub aGates_Hit(idx):PlaySoundAtBall "fx_Gate":End Sub
Sub aWoods_Hit(idx):PlaySoundAtBall "fx_Woodhit":End Sub



'***************************************************************
'             Supporting Ball & Sound Functions v3.0
'  includes random pitch in PlaySoundAt and PlaySoundAtBall
'***************************************************************

Dim TableWidth, TableHeight

TableWidth = Table1.width
TableHeight = Table1.height

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / TableWidth-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10))
    End If
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = (SQR((ball.VelX ^2) + (ball.VelY ^2)))
End Function

Function AudioFade(ball) 'only on VPX 10.4 and newer
    Dim tmp
    tmp = ball.y * 2 / TableHeight-1
    If tmp > 0 Then
        AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10))
    End If
End Function

Sub PlaySoundAt(soundname, tableobj) 'play sound at X and Y position of a fast object, like bumpers, flippers and other solenoids
    PlaySound soundname, 0, 1, Pan(tableobj), 0.1, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname) ' play a sound at the ball position, like rubbers, targets, metals, plastics
    PlaySound soundname, 0, Vol(ActiveBall), pan(ActiveBall), 0.4, 0, 0, 0, AudioFade(ActiveBall)
End Sub


'Set position as table object and Vol manually.

Sub PlaySoundAtVol(sound, tableobj, Vol)
  PlaySound sound, 1, Vol, Pan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'*****************************************
'   rothbauerw's Manual Ball Control
'*****************************************

Dim BCup, BCdown, BCleft, BCright
Dim ControlBallInPlay, ControlActiveBall
Dim BCvel, BCyveloffset, BCboostmulti, BCboost

BCboost = 1				'Do Not Change - default setting
BCvel = 4				'Controls the speed of the ball movement
BCyveloffset = -0.01 	'Offsets the force of gravity to keep the ball from drifting vertically on the table, should be negative
BCboostmulti = 3		'Boost multiplier to ball veloctiy (toggled with the B key) 

ControlBallInPlay = false

Sub StartBallControl_Hit()
	Set ControlActiveBall = ActiveBall
	ControlBallInPlay = true
End Sub

Sub StopBallControl_Hit()
	ControlBallInPlay = false
End Sub	

Sub BallControlTimer_Timer()
	If EnableBallControl and ControlBallInPlay then
		If BCright = 1 Then
			ControlActiveBall.velx =  BCvel*BCboost
		ElseIf BCleft = 1 Then
			ControlActiveBall.velx = -BCvel*BCboost
		Else
			ControlActiveBall.velx = 0
		End If

		If BCup = 1 Then
			ControlActiveBall.vely = -BCvel*BCboost
		ElseIf BCdown = 1 Then
			ControlActiveBall.vely =  BCvel*BCboost
		Else
			ControlActiveBall.vely = bcyveloffset
		End If
	End If
End Sub




'***********************************************
'   JP's VP10 Rolling Sounds + Ballshadow v3.0
'   uses a collection of shadows, aBallShadow
'***********************************************

Const tnob = 19   'total number of balls, 20 balls, from 0 to 19
Const lob = 0     'number of locked balls
Const maxvel = 28 'max ball velocity
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingUpdate()
    Dim BOT, b, ballpitch, ballvol, speedfactorx, speedfactory
    BOT = GetBalls

    ' stop the sound of deleted balls and hide the shadow
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
        aBallShadow(b).Y = 3000
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table

    ' play the rolling sound for each ball and draw the shadow
    For b = lob to UBound(BOT)
        aBallShadow(b).X = BOT(b).X
        aBallShadow(b).Y = BOT(b).Y

        If BallVel(BOT(b))> 1 Then
            If BOT(b).z <30 Then
                ballpitch = Pitch(BOT(b))
                ballvol = Vol(BOT(b))
            Else
                ballpitch = Pitch(BOT(b)) + 25000 'increase the pitch on a ramp
                ballvol = Vol(BOT(b)) * 10
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, ballvol, Pan(BOT(b)), 0, ballpitch, 1, 0, AudioFade(BOT(b))
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If

        ' rothbauerw's Dropping Sounds
        If BOT(b).VelZ <-1 and BOT(b).z <55 and BOT(b).z> 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_balldrop", 0, ABS(BOT(b).velz) / 17, Pan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If

        ' jps ball speed control
        If BOT(b).VelX AND BOT(b).VelY <> 0 Then
            speedfactorx = ABS(maxvel / BOT(b).VelX)
            speedfactory = ABS(maxvel / BOT(b).VelY)
            If speedfactorx <1 Then
                BOT(b).VelX = BOT(b).VelX * speedfactorx
                BOT(b).VelY = BOT(b).VelY * speedfactorx
            End If
            If speedfactory <1 Then
                BOT(b).VelX = BOT(b).VelX * speedfactory
                BOT(b).VelY = BOT(b).VelY * speedfactory
            End If
        End If
    Next
End Sub


'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub




'*****************************************
'	ninuzzu's	FLIPPER SHADOWS
'*****************************************

sub FlipperTimer_Timer()
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle

End Sub





'*****************************************
'	SPINNER
'*****************************************

Sub Spinner_Spin
	PlaySoundAt "fx_spinner", Spinner
	AddScore 100
End Sub






' *********************************************************************
'               Funciones para la cuenta de los puntos
' *********************************************************************

' AÃ±ade puntos al jugador, hace sonar las campanas y actualiza el backdrop


Sub AddScore(Points)
    If Tilted Then Exit Sub
'	ScLightsOn
    Select Case Points
        Case 10, 100, 1000,10000
            ' aÃ±ade los puntos a la variable del actual jugador
            Score(CurrentPlayer) = Score(CurrentPlayer) + points
            ' actualiza los contadores
            UpdateScore points
            ' hace sonar las campanillas de acuerdo a los puntos obtenidos
            If Points = 1000 AND(Score(CurrentPlayer)MOD 10000) \ 1000 = 0 Then  'nuevo reel de 10000
                PlaySound "reel_10000"
            ElseIf Points = 100 AND(Score(CurrentPlayer)MOD 1000) \ 100 = 0 Then 'nuevo reel de 1000
				If NewTone=1 Then
					PlaySound "reelH_1000"
					Else
					PlaySound "reel_1000"
				End If
			ElseIf Points = 10 AND(Score(CurrentPlayer)MOD 100) \ 10 = 0 Then    'nuevo reel de 100
                PlaySound "reel_100"
            Else
				If NewTone=1 Then
					PlaySound "reelH_" &Points
					Else
					PlaySound "reel_" &Points
				End If
            End If
        Case 20, 30, 40, 50
			Add10 = Add10 + Points \ 10
            AddScore10Timer.Enabled = TRUE
		Case 200, 300, 400, 500
            Add100 = Add100 + Points \ 100
            AddScore100Timer.Enabled = TRUE
        Case 2000, 3000, 4000, 5000
            Add1000 = Add1000 + Points \ 1000
            AddScore1000Timer.Enabled = TRUE
		Case 20000,30000,40000,50000
			AddScore10000Timer.Enabled = True
			Add10000 = Add10000 + Points \ 10000
	End Select

    ' ' aqui se puede hacer un chequeo si el jugador ha ganado alguna puntuacion alta y darle un credito o bola extra
    If Score(CurrentPlayer) >= Special1 AND Special1Awarded(CurrentPlayer) = False Then
        AwardExtraBall()
        Special1Awarded(CurrentPlayer) = True
    End If
    If Score(CurrentPlayer) >= Special2 AND Special2Awarded(CurrentPlayer) = False Then
        AwardSpecial()
        Special2Awarded(CurrentPlayer) = True
    End If
    If Score(CurrentPlayer) >= Special3 AND Special3Awarded(CurrentPlayer) = False Then
        AwardSpecial()
        Special3Awarded(CurrentPlayer) = True
    End If
	If Score(CurrentPlayer) >= HighScore AND Special4Awarded(CurrentPlayer) = False Then
		NewTone = 1
        Special4Awarded(CurrentPlayer) = True
    End If
	If Score(CurrentPlayer) >= 1000000 Then
		UMLightOn
		Else
		UMLightOff
	End If
	LedScTimer.Interval = 600
	If B2SOn then
		Select Case CurrentPlayer
				Case 1:Controller.B2SSetData 71, 1
				Case 2:Controller.B2SSetData 72, 1
				Case 3:Controller.B2SSetData 73, 1
				Case 4:Controller.B2SSetData 74, 1
			End Select
	End If
	Select Case CurrentPlayer
        Case 1:sc1.State = 1:sc2.State = 0:sc3.State = 0:sc4.State = 0
        Case 2:sc1.State = 0:sc2.State = 1:sc3.State = 0:sc4.State = 0
        Case 3:sc1.State = 0:sc2.State = 0:sc3.State = 1:sc4.State = 0
        Case 4:sc1.State = 0:sc2.State = 0:sc3.State = 0:sc4.State = 1
    End Select
	LedScTimer.Enabled = True
End Sub




'******************************
'TIMER DE 10, 100 y 1000 PUNTOS
'******************************

' hace sonar las campanillas segÃºn los puntos

Sub AddScore10Timer_Timer()
    if Add10> 0 then
        AddScore 10
        Add10 = Add10 - 1
    Else
        Me.Enabled = FALSE
    End If
End Sub

Sub AddScore100Timer_Timer()
    if Add100> 0 then
        AddScore 100
        Add100 = Add100 - 1
    Else
        Me.Enabled = FALSE
    End If
End Sub

Sub AddScore1000Timer_Timer()
    if Add1000> 0 then
        AddScore 1000
        Add1000 = Add1000 - 1
    Else
        Me.Enabled = FALSE
    End If
End Sub

Sub AddScore10000Timer_Timer()
    if Add10000> 0 then
        AddScore 10000
        Add10000 = Add10000 - 1
    Else
        Me.Enabled = FALSE
    End If
End Sub

Sub LedScTimer_timer
	If B2SOn then
		Select Case CurrentPlayer
			Case 1:Controller.B2SSetData 71, 0
			Case 2:Controller.B2SSetData 72, 0
			Case 3:Controller.B2SSetData 73, 0
			Case 4:Controller.B2SSetData 74, 0
		End Select
	End If
	sc1.State = 0
	sc2.State = 0
	sc3.State = 0
	sc4.State = 0
	LedScTimer.Enabled = False
End Sub

'******************************
' 		Captive Balls
'******************************

	CaptiveBallL.CreateBall
	CaptiveBallL.Kick 70,1
	CaptiveBallL.Enabled=False

	CaptiveBallR.CreateBall
	CaptiveBallR.Kick 70,1
	CaptiveBallR.Enabled=False



'******************************
'     Pasillos 
'******************************

Sub SwPS4_Hit 
    PlaySoundAt "target", SwPS4
	DOF 131, DOFPulse
    If Tilted Then Exit Sub
    AddScore 5000
End Sub

Sub SwPS3_Hit 
    PlaySoundAt "target", SwPS3
	DOF 132, DOFPulse
    If Tilted Then Exit Sub
    AddScore 5000
	If LiPS3EB.State=1 then AwardExtraBall() :LiPS3EB.State=0 :End If
End Sub

Sub SwPS2_Hit 
    PlaySoundAt "target", SwPS2
	DOF 134, DOFPulse
    If Tilted Then Exit Sub
    AddScore 50000
End Sub

Sub SwPS1_Hit 
    PlaySoundAt "target", SwPS1
	DOF 135, DOFPulse
    If Tilted Then Exit Sub
    AddScore 5000
	If LiPS1DB.State=1 then LiDB.State= 1 :DoubleBonus = 2 :LiPS1DB.State=0 :LiDBH.State=0 :End If
End Sub

Sub SwORU_Hit 
    PlaySoundAt "target", SwORU
	DOF 130, DOFPulse
    If Tilted Then Exit Sub
    AddScore 5000
End Sub

Sub SwORM_Hit 
    PlaySoundAt "target", SwORM
	DOF 129, DOFPulse
    If Tilted Then Exit Sub
    AddScore 10000
End Sub

Sub SwRD_Hit 
    PlaySoundAt "target", SwRD
	DOF 122, DOFPulse
    If Tilted Then Exit Sub
    AddScore 10000
End Sub

Sub SwORD_Hit 
    PlaySoundAt "target", SwORD
	DOF 121, DOFPulse
    If Tilted Then Exit Sub
    AddScore 20000
End Sub


Sub SwOLM_Hit 
    PlaySoundAt "target", SwOLM
	DOF 136, DOFPulse
    If Tilted Then Exit Sub
    AddScore 10000
End Sub

Sub SwLD_Hit 
    PlaySoundAt "target", SwLD
	DOF 119, DOFPulse
    If Tilted Then Exit Sub
	If LiDBH.State=1 then LiDB.State= 1 :DoubleBonus = 2 :LiPS1DB.State=0 :LiDBH.State=0 :End If
    AddScore 10000
End Sub

Sub SwOLD_Hit 
    PlaySoundAt "target", SwOLD
	DOF 137, DOFPulse
    If Tilted Then Exit Sub
	If LiDBH.State=1 then LiDB.State= 1 :DoubleBonus = 2 :LiPS1DB.State=0 :LiDBH.State=0 :End If
    AddScore 10000
End Sub


'*****************************************
' 	Triggers
'*****************************************



Sub TriggerPl_Unhit()	
	DOF 116,DOFPulse
End Sub

'**************************************
' 	Bumpers
'**************************************

Sub Bumper001_Hit
    If NOT Tilted Then
        PlaySoundAt SoundFXDOF("fx_Bumper",106,DOFPulse,DOFContactors), bumper001
		DOF 124, DOFPulse
        AddScore 100
    End If
End Sub

Sub Bumper002_Hit
    If NOT Tilted Then
        PlaySoundAt SoundFXDOF("fx_Bumper",105,DOFPulse,DOFContactors), bumper002
		DOF 123, DOFPulse
        AddScore 100
    End If
End Sub

'***********************************
'      Switchs
'***********************************
Dim Rub1, Rub2

Sub rlband006_Hit:Rub1 = 1:rlband006_Timer:End Sub
Sub rlband002_Hit:Rub2 = 1:rlband002_Timer:End Sub

Sub rlband006_Timer
    Select Case Rub1
        Case 1:R01.Visible = 0:R02.Visible = 1:rlband006.TimerEnabled = 1
        Case 2:R02.Visible = 0:R03.Visible = 1
        Case 3:R03.Visible = 0:R01.Visible = 1:rlband006.TimerEnabled = 0
    End Select
    Rub1 = Rub1 + 1
End Sub

Sub rlband002_Timer
    Select Case Rub2
        Case 1:R04.Visible = 0:R05.Visible = 1:rlband002.TimerEnabled = 1
        Case 2:R05.Visible = 0:R06.Visible = 1
        Case 3:R06.Visible = 0:R04.Visible = 1:rlband002.TimerEnabled = 0
    End Select
    Rub2 = Rub2 + 1
End Sub


'***********************************
'      6 Dianas abatibles
'***********************************
Dim DTStatus(5)
DTStatus = Array(DT28, DT29, DT30, DT31, DT32, DT33)


Sub Sw28_Hit
	If Tilted Then Exit Sub
    AddScore 5000
	
	DTStatus(0)=1
	AddBonus 1
	UpdateBonusLights
	DTLightsL 
    CheckLBankTargets
	PlaySoundAtVol  SoundFXDOF("targethit",107,DOFPulse,DOFContactors), Activeball, Vol(Activeball)*22.5
	DTHit 28
	vpmtimer.addtimer 200, "psw28.TransZ = -50 '"
	
End Sub

Sub Sw29_Hit
	If Tilted Then Exit Sub
    AddScore 5000
	
	DTStatus(1)=1
	AddBonus 1
	UpdateBonusLights
	DTLightsL 
    CheckLBankTargets
	PlaySoundAtVol  SoundFXDOF("targethit",107,DOFPulse,DOFContactors), Activeball, Vol(Activeball)*22.5
	DTHit 29
	vpmtimer.addtimer 200, "psw29.TransZ = -50 '"
End Sub

Sub Sw30_Hit
	If Tilted Then Exit Sub
    AddScore 5000
	
	DTStatus(2)=1
	AddBonus 1
	UpdateBonusLights
	DTLightsL 
    CheckLBankTargets
	PlaySoundAtVol  SoundFXDOF("targethit",107,DOFPulse,DOFContactors), Activeball, Vol(Activeball)*22.5
	DTHit 30
	vpmtimer.addtimer 200, "psw30.TransZ = -50 '"
End Sub


Sub Sw31_Hit
	If Tilted Then Exit Sub
    AddScore 5000
	
	DTStatus(3)=1
	AddBonus 1
	UpdateBonusLights
	DTLightsR
    CheckRBankTargets
	PlaySoundAtVol  SoundFXDOF("targethit",108,DOFPulse,DOFContactors), Activeball, Vol(Activeball)*22.5
	DTHit 31
	vpmtimer.addtimer 200, "psw31.TransZ = -50 '"
End Sub

Sub Sw32_Hit
	If Tilted Then Exit Sub
    AddScore 5000
	
	DTStatus(4)=1
	AddBonus 1
	UpdateBonusLights
	DTLightsR
    CheckRBankTargets
	PlaySoundAtVol  SoundFXDOF("targethit",108,DOFPulse,DOFContactors), Activeball, Vol(Activeball)*22.5
	DTHit 32
	vpmtimer.addtimer 200, "psw32.TransZ = -50 '"
End Sub


Sub Sw33_Hit
	If Tilted Then Exit Sub
    AddScore 5000
	
	DTStatus(5)=1
	AddBonus 1
	UpdateBonusLights
	DTLightsR
    CheckRBankTargets
	PlaySoundAtVol  SoundFXDOF("targethit",108,DOFPulse,DOFContactors), Activeball, Vol(Activeball)*22.5
	DTHit 33
	vpmtimer.addtimer 200, "psw33.TransZ = -50 '"
End Sub




Sub CheckLBankTargets     'mira si todas las dianas estan abatidas e incrementa el numero de veces que se han abatido y otorga diferentes bonos Ã³ especial
    If DTStatus(0) + DTStatus(1) + DTStatus(2) = 3 Then 'todas las dianas han sido abatidas
        LBankDown = LBankDown + 1
'debug.print BankDown
        Select Case LBankDown
            Case 1:LiCap1.State = 1 :LiPS1DB.State=1 :LiDBH.State=1
            Case 2:LiCapEB.state = 1 :LiPS1DB.State=0 :LiDBH.State=0
			Case 3:LiCapSP.state = 1
            Case Else
        End Select
        vpmtimer.addtimer 1000, "DTLdrop 1 '"
    End If
End Sub




Sub CheckRBankTargets     'mira si todas las dianas estÃ¡n abatidas e incrementa el nÃºmero de veces que se han abatido y otorga diferentes bonos Ã³ especial
    If DTStatus(3) + DTStatus(4) + DTStatus(5) = 3 Then 'todas las dianas han sido abatidas
'		OffDTLi
'		gi40.State=1
        RBankDown = RBankDown + 1
		'debug.print BankDown
		If RBankDown >2 Then RBankDown=2 
        Select Case RBankDown
            Case 1:LiCap2.State = 1 :LiPS3EB.State=1
            Case 2:LiCapDB.state = 1 :LiPS3EB.State=0
            Case Else
		End Select
        vpmtimer.addtimer 1000, "DTRdrop 1 '"
    End If
End Sub


Sub DTLightsL 
		OffDTLiL
		If DTStatus(0)=1 And DTStatus(1)=1 And DTStatus(2)=1 Then gi40.State=1
		If DTStatus(0)=0 And DTStatus(1)=0 And DTStatus(2)=0 Then gi35.State=1
		If DTStatus(0)=1 And DTStatus(1)=0 And DTStatus(2)=0 Then DTLi1.State=1
		If DTStatus(0)=0 And DTStatus(1)=1 And DTStatus(2)=0 Then DTLi2.State=1
		If DTStatus(0)=1 And DTStatus(1)=1 And DTStatus(2)=0 Then DTLi5.State=1
		If DTStatus(0)=0 And DTStatus(1)=0 And DTStatus(2)=1 Then DTLi3.State=1
		If DTStatus(0)=1 And DTStatus(1)=0 And DTStatus(2)=1 Then DTLi4.State=1
		If DTStatus(0)=0 And DTStatus(1)=1 And DTStatus(2)=1 Then DTLi6.State=1
End Sub

Sub DTLightsR
		OffDTLiR
		If DTStatus(3)=1 And DTStatus(4)=1 And DTStatus(5)=1 Then gi12.State=1
		If DTStatus(3)=0 And DTStatus(4)=0 And DTStatus(5)=0 Then gi41.State=1
		If DTStatus(3)=1 And DTStatus(4)=0 And DTStatus(5)=0 Then DTRi1.State=1
		If DTStatus(3)=0 And DTStatus(4)=1 And DTStatus(5)=0 Then DTRi2.State=1
		If DTStatus(3)=1 And DTStatus(4)=1 And DTStatus(5)=0 Then DTRi5.State=1
		If DTStatus(3)=0 And DTStatus(4)=0 And DTStatus(5)=1 Then DTRi3.State=1
		If DTStatus(3)=1 And DTStatus(4)=0 And DTStatus(5)=1 Then DTRi4.State=1
		If DTStatus(3)=0 And DTStatus(4)=1 And DTStatus(5)=1 Then DTRi6.State=1
End Sub

Sub OffDTLiL
	gi40.State=0
	gi35.State=0
	DTLi1.State=0
	DTLi2.State=0
	DTLi3.State=0
	DTLi4.State=0
	DTLi5.State=0
	DTLi6.State=0
End Sub

Sub OffDTLiR
	gi12.State=0
	gi41.State=0
	DTRi1.State=0
	DTRi2.State=0
	DTRi3.State=0
	DTRi4.State=0
	DTRi5.State=0
	DTRi6.State=0
End Sub

'******************************************************
'		TRACK ALL BALL VELOCITIES
' 		FOR RUBBER DAMPENER AND DROP TARGETS
'******************************************************

dim cor : set cor = New CoRTracker

Class CoRTracker
	public ballvel, ballvelx, ballvely

	Private Sub Class_Initialize : redim ballvel(0) : redim ballvelx(0): redim ballvely(0) : End Sub 

	Public Sub Update()	'tracks in-ball-velocity
		dim str, b, AllBalls, highestID : allBalls = getballs

		for each b in allballs
			if b.id >= HighestID then highestID = b.id
		Next

		if uBound(ballvel) < highestID then redim ballvel(highestID)	'set bounds
		if uBound(ballvelx) < highestID then redim ballvelx(highestID)	'set bounds
		if uBound(ballvely) < highestID then redim ballvely(highestID)	'set bounds

		for each b in allballs
			ballvel(b.id) = BallSpeed(b)
			ballvelx(b.id) = b.velx
			ballvely(b.id) = b.vely
		Next
	End Sub
End Class

Sub RDampen_Timer()
	Cor.Update
End Sub


'******************************************************
'		DROP TARGETS INITIALIZATION
'******************************************************

Class DropTarget
  Private m_primary, m_secondary, m_prim, m_sw, m_animate, m_isDropped

  Public Property Get Primary(): Set Primary = m_primary: End Property
  Public Property Let Primary(input): Set m_primary = input: End Property

  Public Property Get Secondary(): Set Secondary = m_secondary: End Property
  Public Property Let Secondary(input): Set m_secondary = input: End Property

  Public Property Get Prim(): Set Prim = m_prim: End Property
  Public Property Let Prim(input): Set m_prim = input: End Property

  Public Property Get Sw(): Sw = m_sw: End Property
  Public Property Let Sw(input): m_sw = input: End Property

  Public Property Get Animate(): Animate = m_animate: End Property
  Public Property Let Animate(input): m_animate = input: End Property

  Public Property Get IsDropped(): IsDropped = m_isDropped: End Property
  Public Property Let IsDropped(input): m_isDropped = input: End Property

  Public default Function init(primary, secondary, prim, sw, animate, isDropped)
    Set m_primary = primary
    Set m_secondary = secondary
    Set m_prim = prim
    m_sw = sw
    m_animate = animate
    m_isDropped = isDropped

    Set Init = Me
  End Function
End Class

'Define a variable for each drop target
Dim  DT28, DT29, DT30, DT31, DT32, DT33

'Set array with drop target objects
'
'DropTargetvar = Array(primary, secondary, prim, swtich, animate)
' 	primary: 			primary target wall to determine drop
'	secondary:			wall used to simulate the ball striking a bent or offset target after the initial Hit
'	prim:				primitive target used for visuals and animation
'							IMPORTANT!!! 
'							rotz must be used for orientation
'							rotx to bend the target back
'							transz to move it up and down
'							the pivot point should be in the center of the target on the x, y and at or below the playfield (0) on z
'	switch:				ROM switch number
'	animate:			Arrary slot for handling the animation instrucitons, set to 0
'
'	Values for annimate: 1 - bend target (hit to primary), 2 - drop target (hit to secondary), 3 - brick target (high velocity hit to secondary), -1 - raise target 

' Right Bank
Set DT31 = (new DropTarget)(sw31, sw31y, psw31, 31, 0, false)
Set DT32 = (new DropTarget)(sw32, sw32y, psw32, 32, 0, false)
Set DT33 = (new DropTarget)(sw33, sw33y, psw33, 33, 0, false)

' Left Bank
Set DT28 = (new DropTarget)(sw28, sw28y, psw28, 28, 0, false)
Set DT29 = (new DropTarget)(sw29, sw29y, psw29, 29, 0, false)
Set DT30 = (new DropTarget)(sw30, sw30y, psw30, 30, 0, false)



'Add all the Drop Target Arrays to Drop Target Animation Array
' DTAnimationArray = Array(DT1, DT2, ....)
Dim DTArray	', DTStatus
DTArray = Array(DT28, DT29, DT30, DT31, DT32, DT33)
'DTStatus = Array(DT28, DT29, DT30, DT31, DT32, DT33)


'Configure the behavior of Drop Targets.
Const DTDropSpeed = 110 			'in milliseconds
Const DTDropUpSpeed = 40 			'in milliseconds
Const DTDropUnits = 44 			'VP units primitive drops
Const DTDropUpUnits = 10 			'VP units primitive raises above the up position on drops up
Const DTMaxBend = 8 				'max degrees primitive rotates when hit
Const DTDropDelay = 20	 		'time in milliseconds before target drops (due to friction/impact of the ball)
Const DTRaiseDelay = 40	 		'time in milliseconds before target drops back to normal up position after the solendoid fires to raise the target
Const DTBrickVel = 30				'velocity at which the target will brick, set to '0' to disable brick

Const DTEnableBrick = 1			'Set to 0 to disable bricking, 1 to enable bricking
Const DTHitSound = "targethit"	'Drop Target Hit sound
Const DTDropSound = "DTDrop"		'Drop Target Drop sound
Const DTResetSound = "DTReset"	'Drop Target reset sound

Const DTMass = 0.2				'Mass of the Drop Target (between 0 and 1), higher values provide more resistance

'******************************************************
'				DROP TARGETS FUNCTIONS
'******************************************************

Sub DTHit(switch)
	Dim i
	i = DTArrayID(switch)

'	PlaySoundAtVol  SoundFXDOF("targethit",106,DOFPulse,DOFContactors), Activeball, Vol(Activeball)*22.5
	DTArray(i).animate =  DTCheckBrick(Activeball,DTArray(i).prim)
	If DTArray(i).animate = 1 or DTArray(i).animate = 3 Then
		DTBallPhysics Activeball, DTArray(i).prim.rotz, DTMass
	End If
	DoDTAnim	
End Sub



Sub DTRaise(switch)
	Dim i
	i = DTArrayID(switch)

	DTArray(i).animate = -1
	DoDTAnim
End Sub

Sub DTDrop(switch)
	Dim i
	i = DTArrayID(switch)

	DTArray(i).animate = 1
	DoDTAnim
End Sub

Function DTArrayID(switch)
	Dim i
	For i = 0 to uBound(DTArray) 
		If DTArray(i).sw = switch Then DTArrayID = i:Exit Function 
	Next
End Function


sub DTBallPhysics(aBall, angle, mass)
	dim rangle,bangle,calc1, calc2, calc3
	rangle = (angle - 90) * 3.1416 / 180
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))

	calc1 = cor.BallVel(aball.id) * cos(bangle - rangle) * (aball.mass - mass) / (aball.mass + mass)
	calc2 = cor.BallVel(aball.id) * sin(bangle - rangle) * cos(rangle + 4*Atn(1)/2)
	calc3 = cor.BallVel(aball.id) * sin(bangle - rangle) * sin(rangle + 4*Atn(1)/2)

	aBall.velx = calc1 * cos(rangle) + calc2
	aBall.vely = calc1 * sin(rangle) + calc3
End Sub

'Add Timer name DTAnim to editor to handle drop target animations
DTAnim.interval = 10
DTAnim.enabled = True

Sub DTAnim_Timer()
	DoDTAnim
End Sub

'Check if target is hit on it's face or sides and whether a 'brick' occurred
Function DTCheckBrick(aBall, dtprim) 
	dim bangle, bangleafter, rangle, rangle2, Xintersect, Yintersect, cdist, perpvel, perpvelafter
	rangle = (dtprim.rotz - 90) * 3.1416 / 180
	rangle2 = dtprim.rotz * 3.1416 / 180
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
	bangleafter = Atn2(aBall.vely,aball.velx)

	Xintersect = (aBall.y - dtprim.y - tan(bangle) * aball.x + tan(rangle2) * dtprim.x) / (tan(rangle2) - tan(bangle))
	Yintersect = tan(rangle2) * Xintersect + (dtprim.y - tan(rangle2) * dtprim.x)

	cdist = Distance(dtprim.x, dtprim.y, Xintersect, Yintersect)

	perpvel = cor.BallVel(aball.id) * cos(bangle-rangle)
	perpvelafter = BallSpeed(aBall) * cos(bangleafter - rangle) 

	If perpvel <= 0 or perpvelafter >= 0 Then
		DTCheckBrick = 0 
	ElseIf DTEnableBrick = 1 and  perpvel > DTBrickVel and DTBrickVel <> 0 and cdist < 8 Then
		DTCheckBrick = 3
	Else
		DTCheckBrick = 1
	End If
End Function


Sub DoDTAnim()
	Dim i
	For i=0 to Ubound(DTArray)
		DTArray(i).animate = DTAnimate(DTArray(i).primary,DTArray(i).secondary,DTArray(i).prim,DTArray(i).sw,DTArray(i).animate)
	Next
End Sub

Function DTAnimate(primary, secondary, prim, switch,  animate)
	dim transz
	Dim animtime, rangle

	rangle = prim.rotz * 3.1416 / 180

	DTAnimate = animate

	if animate = 0  Then
		primary.uservalue = 0
		DTAnimate = 0
		Exit Function
	Elseif primary.uservalue = 0 then 
		primary.uservalue = gametime
	end if

	animtime = gametime - primary.uservalue

	If animate = 1 and animtime < DTDropDelay Then
		primary.collidable = 0
		secondary.collidable = 1
		prim.rotx = DTMaxBend * cos(rangle)
		prim.roty = DTMaxBend * sin(rangle)
		DTAnimate = 1
		Exit Function
	elseif animate = 1 and animtime > DTDropDelay Then
		primary.collidable = 0
		secondary.collidable = 1
		prim.rotx = DTMaxBend * cos(rangle)
		prim.roty = DTMaxBend * sin(rangle)
		animate = 2
		PlaySoundAt SoundFX(DTDropSound,DOFDropTargets),prim
	End If

	if animate = 2 Then
		transz = (animtime - DTDropDelay)/DTDropSpeed *  DTDropUnits * -1
		if prim.transz > -DTDropUnits  Then
			prim.transz = transz
		end if

		prim.rotx = DTMaxBend * cos(rangle)/2
		prim.roty = DTMaxBend * sin(rangle)/2

		if prim.transz <= -DTDropUnits Then 
			prim.transz = -DTDropUnits
			secondary.collidable = 0
'			controller.Switch(Switch) = 1
			EMDTScore (Switch)
			primary.uservalue = 0
			DTAnimate = 0
			Exit Function
		Else
			DTAnimate = 2
			Exit Function
		end If 
	End If

	If animate = 3 and animtime < DTDropDelay Then
		primary.collidable = 0
		secondary.collidable = 1
		prim.rotx = DTMaxBend * cos(rangle)
		prim.roty = DTMaxBend * sin(rangle)
	elseif animate = 3 and animtime > DTDropDelay Then
		primary.collidable = 1
		secondary.collidable = 0
		prim.rotx = 0
		prim.roty = 0
		primary.uservalue = 0
		DTAnimate = 0
		Exit Function
	End If

	if animate = -1 Then
		transz = (1 - (animtime)/DTDropUpSpeed) *  DTDropUnits * -1

		If prim.transz = -DTDropUnits Then
			Dim BOT, b
			BOT = GetBalls

			For b = 0 to UBound(BOT)
				If InRect(BOT(b).x,BOT(b).y,prim.x-25,prim.y-10,prim.x+25, prim.y-10,prim.x+25,prim.y+25,prim.x -25,prim.y+25) Then
					BOT(b).velz = 20
				End If
			Next
		End If

		if prim.transz < 0 Then
			prim.transz = transz
		elseif transz > 0 then
			prim.transz = transz
		end if

		if prim.transz > DTDropUpUnits then 
			DTAnimate = -2
			prim.rotx = 0
			prim.roty = 0
			primary.uservalue = gametime
		end if
		primary.collidable = 0
		secondary.collidable = 1
'		controller.Switch(Switch) = 0

	End If

	if animate = -2 and animtime > DTRaiseDelay Then
		prim.transz = (animtime - DTRaiseDelay)/DTDropSpeed *  DTDropUnits * -1 + DTDropUpUnits 
		if prim.transz < 0 then
			prim.transz = 0
			primary.uservalue = 0
			DTAnimate = 0

			primary.collidable = 1
			secondary.collidable = 0
		end If 
	End If
End Function


Sub EMDTScore(switch)
	Select Case switch
		Case 0: 'DT scoring logic for DT 0 here
		Case 1: 'DT scoring logic for DT 1 here
		Case 2: 'DT scoring logic for DT 2 here
		End Select
End Sub


Sub DTLdrop(enabled)
	if enabled then
		PlaySoundAt SoundFXDOF("DTReset",107,DOFPulse,DOFContactors), psw28
		gi35.State=1
		gi40.state=0
		DTRaise 28
		DTStatus(0)=0
		DTRaise 29
		DTStatus(1)=0
		DTRaise 30
		DTStatus(2)=0
	end if
End Sub

Sub DTRdrop(enabled)
	if enabled then
		PlaySoundAt SoundFXDOF("DTReset",108,DOFPulse,DOFContactors), psw31
		gi12.State=0
		gi41.state=1
		DTRaise 31
		DTStatus(3)=0
		DTRaise 32
		DTStatus(4)=0
		DTRaise 33
		DTStatus(5)=0
	end if
End Sub


'******************************************************
'		DROP TARGET
'		SUPPORTING FUNCTIONS 
'******************************************************

' Used for flipper correction, rubber dampeners, and drop targets
Function BallSpeed(ball) 'Calculates the ball speed
    BallSpeed = SQR(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

' Used for drop targets
Function Atn2(dy, dx)
	dim pi
	pi = 4*Atn(1)

	If dx > 0 Then
		Atn2 = Atn(dy / dx)
	ElseIf dx < 0 Then
		If dy = 0 Then 
			Atn2 = pi
		Else
			Atn2 = Sgn(dy) * (pi - Atn(Abs(dy / dx)))
		end if
	ElseIf dx = 0 Then
		if dy = 0 Then
			Atn2 = 0
		else
			Atn2 = Sgn(dy) * pi / 2
		end if
	End If
End Function

' Used for drop targets
'*** Determines if a Points (px,py) is inside a 4 point polygon A-D in Clockwise/CCW order
Function InRect(px,py,ax,ay,bx,by,cx,cy,dx,dy)
	Dim AB, BC, CD, DA
	AB = (bx*py) - (by*px) - (ax*py) + (ay*px) + (ax*by) - (ay*bx)
	BC = (cx*py) - (cy*px) - (bx*py) + (by*px) + (bx*cy) - (by*cx)
	CD = (dx*py) - (dy*px) - (cx*py) + (cy*px) + (cx*dy) - (cy*dx)
	DA = (ax*py) - (ay*px) - (dx*py) + (dy*px) + (dx*ay) - (dy*ax)
 
	If (AB <= 0 AND BC <=0 AND CD <= 0 AND DA <= 0) Or (AB >= 0 AND BC >=0 AND CD >= 0 AND DA >= 0) Then
		InRect = True
	Else
		InRect = False       
	End If
End Function

' Used for drop targets
Function Distance(ax,ay,bx,by)
	Distance = SQR((ax - bx)^2 + (ay - by)^2)
End Function

'***********************************
'      3 Dianas No abatibles
'***********************************

Sub Target7_Hit
    PlaySoundAtBall SoundFXDOF("fx_target",140,DOFPulse,DOFTargets)
    If Tilted Then Exit Sub
    AddScore 500
End Sub

Sub Target8_Hit
    PlaySoundAtBall SoundFXDOF("fx_target",140,DOFPulse,DOFTargets)
    If Tilted Then Exit Sub
    AddScore 500
End Sub

Sub Target9_Hit
    PlaySoundAtBall SoundFXDOF("fx_target",141,DOFPulse,DOFTargets)
    If Tilted Then Exit Sub
    AddScore 500
End Sub

'***********************************
'*	Captive SW/Target
'***********************************

Sub CaptiveSw1_Hit
    PlaySoundAt "fx_sensor", CaptiveSw1
	DOF 136, DOFPulse
    If Tilted Then Exit Sub
    AddScore 500
End Sub
it
Sub CaptiveSw2_Hit
    PlaySoundAt "fx_sensor", CaptiveSw2
	DOF 136, DOFPulse
	DOF 112, DOFPulse 
    If Tilted Then Exit Sub
    Select Case LBankDown
			Case 0:AddScore 0 
            Case 1:AddScore 50000
            Case 2:AwardExtraBall
			Case 3:AwardSpecial
            Case Else
                AddScore 0
        End Select
End Sub

Sub TargetCaptive_Hit
    PlaySoundAtBall SoundFXDOF("fx_target",140,DOFPulse,DOFTargets)
	DOF 112, DOFPulse 
	DOF 125, DOFPulse 
    If Tilted Then Exit Sub
    Select Case RBankDown
			Case 0:AddScore 0 
            Case 1:AddScore 50000
            Case 2:DoubleBonus = 2 : LiCapDB.State=0 : LiDB.State=1
            Case Else
                AddScore 0
        End Select
End Sub


'***********************************
'*	Rubbers
'***********************************

Sub Rubber011_Hit
	If NOT Tilted Then AddScore 10
End Sub

Sub Rubber012_Hit
	If NOT Tilted Then AddScore 10
End Sub

Sub Wall007_Hit
	If NOT Tilted Then AddScore 10
End Sub

Sub Rubber015_Hit
	If NOT Tilted Then AddScore 10
End Sub

'***********************************
'*	Lanes
'***********************************


Sub Table1_Exit()
	Savehs
End Sub

'*******************
'     BONOS
'*******************

' avanza el bono y actualiza las luces
' Los bonos estÃ¡n limitados a 15000 puntos

Sub AddBonus(bonuspoints)
    If(Tilted = False) Then
        ' aÃ±ade los bonos al jugador actual
        Bonus = Bonus + bonuspoints
        If Bonus> 15 Then
            Bonus = 15
'			If LiAdCo.State = 1 Then
'			   LiVari20M.State = 1
'			End If
        End If
		' actualiza las luces
        UpdateBonusLights
	    End if
End Sub


'***********************************************************************************
'        Score reels - puntuaciones - y actualiza otras luces del backdrop
'***********************************************************************************
'esta es al rutina que actualiza la puntuaciÃ³n del jugador

Sub UpdateScore(playerpoints)
    Select Case CurrentPlayer
        Case 1:ScoreReel1.Addvalue playerpoints
        Case 2:ScoreReel2.Addvalue playerpoints
        Case 3:ScoreReel3.Addvalue playerpoints
        Case 4:ScoreReel4.Addvalue playerpoints
    End Select
	If B2SOn then
		Controller.B2SSetScorePlayer CurrentPlayer,Score(CurrentPlayer)
		If Score(CurrentPlayer)>=100000 then	
			Controller.B2SSetScoreRollover 24+CurrentPlayer,1
		end if
	end if
End Sub

' pone todos los marcadores a 0
Sub ResetScores
    ScoreReel1.ResetToZero
    ScoreReel2.ResetToZero
    ScoreReel3.ResetToZero
    ScoreReel4.ResetToZero
	If B2SOn then
		Controller.B2SSetScorePlayer1 0
		Controller.B2SSetScorePlayer2 0
		Controller.B2SSetScorePlayer3 0
		Controller.B2SSetScorePlayer4 0
		Controller.B2SSetScorePlayer5 0
		Controller.B2SSetScorePlayer6 0
		Controller.B2SSetScoreRolloverPlayer1 0
		Controller.B2SSetScoreRolloverPlayer2 0
		Controller.B2SSetScoreRolloverPlayer3 0
		Controller.B2SSetScoreRolloverPlayer4 0
		Controller.B2SSetData 81, 0
		Controller.B2SSetData 82, 0
		Controller.B2SSetData 83, 0
		Controller.B2SSetData 84, 0
	end if
End Sub

Sub AddCredits(value)
    If Credits <9 Then
        Credits = Credits + value
        CreditReel.SetValue Credits
        UpdateCredits
	end if
End Sub

Sub UpdateCredits
    If Credits> 0 Then
        DOF 118, DOFOn
    Else
        DOF 118, DOFOff
    End If
    CreditReel.SetValue credits
	If B2SOn then
		Controller.B2SSetCredits Credits
	end if
End Sub

Sub UpdateBallInPlay 
 	If ExtraBallsAwards(CurrentPlayer) > 0 then
		LiExtraBall.State = 2
	end if
	Select Case Balls
        Case 0:bip1.State = 0:bip2.State = 0:bip3.State = 0:bip4.State = 0:bip5.State = 0
        Case 1:bip1.State = 1:bip2.State = 0:bip3.State = 0:bip4.State = 0:bip5.State = 0
        Case 2:bip1.State = 0:bip2.State = 1:bip3.State = 0:bip4.State = 0:bip5.State = 0
        Case 3:bip1.State = 0:bip2.State = 0:bip3.State = 1:bip4.State = 0:bip5.State = 0
        Case 4:bip1.State = 0:bip2.State = 0:bip3.State = 0:bip4.State = 1:bip5.State = 0
        Case 5:bip1.State = 0:bip2.State = 0:bip3.State = 0:bip4.State = 0:bip5.State = 1
    End Select
    If B2SOn then
        Controller.B2SSetBallInPlay Balls
	End If

    Select Case CurrentPlayer
        Case 1:Player1.State = 1:Player2.State = 0:Player3.State = 0:Player4.State = 0
        Case 2:Player1.State = 0:Player2.State = 1:Player3.State = 0:Player4.State = 0
        Case 3:Player1.State = 0:Player2.State = 0:Player3.State = 1:Player4.State = 0
        Case 4:Player1.State = 0:Player2.State = 0:Player3.State = 0:Player4.State = 1
    End Select
	If B2SOn then
		Controller.B2SSetPlayerUp CurrentPlayer
	end if
    Select Case PlayersPlayingGame
        Case 1:Pl1.SetValue 1:Pl2.SetValue 0:Pl3.SetValue 0:Pl4.SetValue 0
        Case 2:Pl1.SetValue 0:Pl2.SetValue 1:Pl3.SetValue 0:Pl4.SetValue 0
        Case 3:Pl1.SetValue 0:Pl2.SetValue 0:pl3.SetValue 1:Pl4.SetValue 0
        Case 4:Pl1.SetValue 0:Pl2.SetValue 0:Pl3.SetValue 0:Pl4.SetValue 1
    End Select
	If B2SOn then
		Controller.B2SSetCanPlay PlayersPlayingGame
	end if
End Sub


Sub UMLightOn
	Select Case CurrentPlayer
        Case 1:LiM1.State = 1
        Case 2:LiM2.State = 1
        Case 3:LiM3.State = 1
        Case 4:LiM4.State = 1
    End Select
	If B2SOn then
		Select Case CurrentPlayer
			Case 1:Controller.B2SSetData 81, 1
			Case 2:Controller.B2SSetData 82, 1
			Case 3:Controller.B2SSetData 83, 1
			Case 4:Controller.B2SSetData 84, 1
		End Select
	end if
End Sub

Sub UMLightOff
	Select Case CurrentPlayer
        Case 1:LiM1.State = 0
        Case 2:LiM2.State = 0
        Case 3:LiM3.State = 0
        Case 4:LiM4.State = 0
    End Select
	If B2SOn then
		Select Case CurrentPlayer
			Case 1:Controller.B2SSetData 81, 0
			Case 2:Controller.B2SSetData 82, 0
			Case 3:Controller.B2SSetData 83, 0
			Case 4:Controller.B2SSetData 84, 0
		End Select
	end if
End Sub


'*************************
' Partidas y bolas extras
'*************************

Sub AwardExtraBall()
    If NOT bExtraBallWonThisBall Then
        PlaySound "fx_knocker"
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
        bExtraBallWonThisBall = True	'Comentar para mas bolas extras
        ShootAgain.SetValue 1
		LiExtraBall.State = 2
		EBReel.SetValue ExtraBallsAwards(CurrentPlayer)
		If B2SOn then
			Controller.B2SSetShootAgain 1
			Controller.B2SSetScorePlayer5 ExtraBallsAwards(CurrentPlayer)
		end if
    END If
End Sub



Sub AwardSpecial()
	If PaySpecial = True Then Exit Sub
    PlaySound SoundFXDOF("fx_knocker",133, DOFPulse,DOFKnocker)
	DOF 112, DOFPulse
    AddCredits 1
'	LiSpecial.State = 0
	PaySpecial = True
	FPlays = FPlays + 1
	FPReel.SetValue FPlays
	If B2SOn then
		Controller.B2SSetScorePlayer6 FPlays
	End If
End Sub

' ********************************
'        Attract Mode
' ********************************
' las luces simplemente parpadean de acuerdo a los valores que hemos puesto en el "Blink Pattern" de cada luz

Sub StartAttractMode()
'    Dim x
    bAttractMode = True
    For each x in aLights
        x.State = 2
    Next
    ' enciente la luz de fin de partida
    GameOverR.SetValue 1
	AttractTimer.Enabled = 1	'enciende timer
End Sub

Sub StopAttractMode()
    Dim x
    bAttractMode = False
    ResetScores
'    For each x in aLights
'        x.State = 0
'    Next

    ' apaga la luz de fin de partida
    GameOverR.SetValue 0
	AttractTimer.Enabled = 0	'apaga timer
End Sub

Dim AttractStep
AttractStep = 0

Sub AttractTimer_Timer
	Player1.State = 0:Player2.State = 0:Player3.State = 0:Player4.State = 0
	If B2SOn then
		Controller.B2SSetData 71, 0 : Controller.B2SSetData 72, 0 : Controller.B2SSetData 73, 0 :Controller.B2SSetData 74, 0
		Controller.B2SSetScorePlayer5 0
        Controller.B2SSetScorePlayer6 FPlays
	End If
    Select Case AttractStep
        Case 0
            AttractStep = 1
			ScoreReel1.SetValue Score(1) : CurrentPlayer = 1 : Check1M
            ScoreReel2.SetValue Score(2) : CurrentPlayer = 2 : Check1M
            ScoreReel3.SetValue Score(3) : CurrentPlayer = 3 : Check1M
            ScoreReel4.SetValue Score(4) : CurrentPlayer = 4 : Check1M
		    If B2SOn then
				Controller.B2SSetScorePlayer1 Score(1) : CurrentPlayer = 1 : Check1M
                Controller.B2SSetScorePlayer2 Score(2) : CurrentPlayer = 2 : Check1M
                Controller.B2SSetScorePlayer3 Score(3) : CurrentPlayer = 3 : Check1M
                Controller.B2SSetScorePlayer4 Score(4) : CurrentPlayer = 4 : Check1M
            end if
        Case 1
            AttractStep = 0
			If Highscore >= 1000000 Then LiM1.State = 1 : End If
            ScoreReel1.SetValue Highscore 
            ScoreReel2.SetValue 0 : LiM2.State = 0
            ScoreReel3.SetValue 0 : LiM3.State = 0
            ScoreReel4.SetValue 0 : LiM4.State = 0
            If B2SOn then
                Controller.B2SSetScorePlayer1 Highscore : If Highscore >= 1000000 Then Controller.B2SSetData 81, 1 : End If
                Controller.B2SSetScorePlayer2 0 : Controller.B2SSetData 82, 0
                Controller.B2SSetScorePlayer3 0 : Controller.B2SSetData 83, 0
                Controller.B2SSetScorePlayer4 0	: Controller.B2SSetData 84, 0
            end if
    End Select
End Sub

Sub Check1M
	If Score(CurrentPlayer) >= 1000000 Then
		UMLightOn
		Else
		UMLightOff
	End If	




End Sub



'************************************************
'    Load (cargar) / Save (guardar)/ Highscore
'************************************************

' solamente guardamos el numero de créditos y la puntuación más alta

Sub Loadhs
    Dim x
    x = LoadValue(TableName, "HighScore")
    If(x <> "") Then HighScore = CDbl(x) Else HighScore = 0 End If
    x = LoadValue(TableName, "Credits")
    If(x <> "") then Credits = CInt(x) Else Credits = 0 End If
End Sub

Sub Savehs
    SaveValue TableName, "HighScore", HighScore
    SaveValue TableName, "Credits", Credits
End Sub

' por si se necesitara quitar la actual puntuacion mas alta, se le puede poner a una tecla,
' o simplemente abres la ventana de debug y escribes Reseths y le das al enter
Sub Reseths
    HighScore = 0
    Savehs
End Sub

'****************************************
' Actualizaciones en tiempo real
'****************************************
' se usa sobre todo para hacer animaciones o sonidos que cambian en tiempo real
' como por ejemplo para sincronizar los flipers, puertas Ã³ molinillos con primitivas

Sub GameTimer_Timer
    RollingUpdate 'actualiza el sonido de la bola rodando
    ' y tambiÃ©n algunas animaciones, sobre todo de primitivas
'    gatep.RotY = gatef.CurrentAngle
End Sub






'***********************************************************************
' *********************************************************************
'            Aqui­ empieza el codigo particular a la mesa
' (hasta ahora todas las rutinas han sido muy generales para todas las mesas)
' (y hay muy pocas rutinas que necesitan cambiar de mesa a mesa)
' *********************************************************************
'***********************************************************************

' se inicia las dianas abatibles, primitivas, etc.
' aunque en el VPX no hay muchos objetos que necesitan ser iniciados

Sub VPObjects_Init'en esta mesa no hay nada que necesite iniciarse, pero dejamos la rutina para prÃ³ximas mesas
End Sub


' variables de la mesa
Dim BankDown    'esta variable contentdrÃ¡ el nÃºmero de veces que el jugador ha abatido las dianas

Sub Game_Init() 'esta rutina se llama al principio de un nuevo juego

    'Empezar alguna mÃºsica, si hubiera mÃºsica en esta mesa

    'iniciar variables, en esta mesa hay muy pocas variables ya que usamos las luces, y el UserValue de las dianas

    'iniciar algÃºn timer

    'Iniciar algunas luces, en esta mesa son las mismas luces que las de una bola nueva
    TurnOffPlayfieldLights()
'    li27.State = 1
'    li28.State = 1
'    li32.State = 1
'    ChangeToBumper1Lights
End Sub

Sub ResetNewBallVariables() 'inicia las variable para una bola Ã³ jugador nuevo
    Bonus = 0
	AddBonus 0			 	'esta tabla empieza con cero bonos
	DoubleBonus = 1			'multiplicador de los bonos
    RBankDown = 0           'a cada nueva bola pone a cero el numero de veces que las dianas han sido abatidas
	LBankDown = 0 
	     
	DTLdrop 1				'sube las dianas de la izquierda
	DTRdrop 1				'sube las dianas de la derecha
End Sub

Sub ResetNewBallLights()    'enciende Ã³ apaga las luces para una bola nueva
	LiCap1.State = 0
	LiCap2.State = 0
	LiDB.State = 0
	LiCapEB.State = 0
	LiDBH.State = 0
	LiCapSP.State = 0
	LiCapDB.State = 0
	LiExtraBall.State = 0
	LiPS1DB.State = 0
	LiPS3EB.sTATE = 0
'    ChangeToBumper1Lights
End Sub

Sub TurnOffPlayfieldLights()
    Dim a
    For each a in aTargetLights
        a.State = 0
    Next
End Sub





Sub LiS1_Init()
	
End Sub


