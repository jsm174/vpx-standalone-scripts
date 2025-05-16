'----------------------------
'JUNGLE QUEEN (Gottlieb 1977)
'----------------------------
'VP 9,15^ - FS, B2S script 
'a starman recreation - November 2013 - V 1.0.0
'----------------------------------------------

'+ analog plunger support (Thanks to koadic!)

Option Explicit
Randomize


Const BallSize = 50
Const BallMass = 1.7

ExecuteGlobal GetTextFile("core.vbs")

On Error Resume Next
ExecuteGlobal GetTextFile("Controller.vbs")
If Err Then MsgBox "Unable to open Controller.vbs. Ensure that it is in the scripts folder."
On Error Goto 0

Const cGameName = "Jungle_Queen"
Dim Controller
Set Controller = CreateObject("B2S.Server")
Controller.B2SName = "Jungle_Queen"
Controller.Run
Dim B2SOn

Dim Tablename
Dim GameOver, GameInProgress, Started, Tilt, OldTilt, NewTilt, TiltSwing
Dim i,x,y,z
Dim RingAni(2)
Dim PCount, PTime, plungerIM
Dim Score(4), Players, Currentplayer, PU, Points, sTsd, sHdd, sHdd2
Dim BIP, Balls, aBall, maxBalls, LastBall
Dim Bonus, DoubleBonus, BonusCounted, Credits
Dim FirstGame, GameCount
Dim EB, EBReady, SpecialReady, Rep1(4), Rep2(4), Rep3(4)
Dim HighScore3, HighScore5, PreScore1, PreScore2, PreScore3, PreScore4
Dim Score1000K, Score100K, Score10K, ScoreK, Score100, Score10, ScoreUnit
Dim Matchnumber
Dim h3s, h3s1000K, h3s100K, h3s10k, h3sK, h3s100, h3s10, h3sUnit
Dim h5s, h5s1000K, h5s100K, h5s10k, h5sK, h5s100, h5s10, h5sUnit
Dim PreScore1000K, PreScore100K, PreScore10K, PreScoreK, PreScore100, PreScore10, PreScoreUnit
Dim Cup, mHole, mKickL, mKickR, BOn, Blink
Dim BRoll, KVar, BallSpeed, Speedx, Speedy, FinalSpeed
Dim ScoreDT(4), ScoreReel


Const FiveBalls = true'True'False'True'False '(select True for 5-balls or False for 3-balls per game)
Const IMPowerSetting = 48
Const IMTime = 0.8

Sub SetB2SData(ByVal Pos,ByVal Value)
	Controller.B2SSetData Pos, Value
End Sub

Sub StepB2SData (ByVal startpos,ByVal endpos,ByVal value,ByVal direction,ByVal steptime,ByVal stepsound)
	' TODO this should use a timer to animate the stepping?
	Dim b2si
	For b2si=startpos to endpos
		Controller.B2SSetData b2si, value
	Next
End Sub

Sub ResetB2SData(ByVal StartPos,ByVal EndPos,ByVal Value)
	Dim b2si
	For b2si=StartPos to EndPos
		Controller.B2SSetData b2si, Value
	Next
End Sub

Sub table_Init()
	TableName = "JungleQueen"
	LoadData()
	On Error Resume Next
	LoadEM
	ReSetB2SData 0, 49, 0
	Table_Reset
End Sub

Sub Table_Reset()
'	Impulse Plunger
	Set plungerIM = New cvpmImpulseP
	With plungerIM
		.InitImpulseP swplunger, IMPowerSetting, IMTime
		.Random 0.0
		.InitExitSnd "", "PRelease"
		.CreateEvents "plungerIM"
	End With

	Set mKickL = New cvpmMagnet
	With mKickL
		.InitMagnet GrabL, 3.5
		.GrabCenter = 0
		.MagnetOn = 1
		.CreateEvents "mKickL"
	End With

	Set mKickR = New cvpmMagnet
	With mKickR
		.InitMagnet GrabR, 3.5
		.GrabCenter = 0
		.MagnetOn = 1
		.CreateEvents "mKickR"
	End With
	TextGC.Text = GameCount
	GameOver = True : Controller.B2SSetGameover 1 : dtgameover.state=1
	Started = False
	For i = 1 To 4 : Score(i) = 0 : Next
	Players = 0 : CurrentPlayer = 1 : PU = 0 : Controller.B2SSetPlayerUp PU : Setpup : Controller.B2SSetCanPlay  Players
	BIP = 0 : maxBalls = True
	EB = False : SpecialReady = True : EBReady = True
	DropOff()
	Bonus = 0 : BonusCounted = True : BonusText.Text = Bonus
	Blink = 0
	If FiveBalls = True Then
		LightBalls1.State = 1 : LightBalls2.State = 1 : Balls = 5 : updatehs
	Else
		LightBalls1.State = 0 : LightBalls2.State = 0 : Balls = 3 : updatehs
	End If
	BOn = False
	LastBall = False
	Tilt = False : NewTilt = 0 : OldTilt = 0 : TiltSwing = 0 : Controller.B2SSetTilt 0
	BumperAttTimer.Enabled = True
	TimerFade.Enabled = True
	AllLampsOff
	UpdatePreScore : UpdateMatch : UpdateCredits
	FirstGame = True
	BallsText.Text = fiveballs
	SetB2SData 31,0
Controller.B2SSetCredits Credits
CoinReel.SetValue Credits

If table.showDT=True Then
dim dtobj
dim fsobj
for each dtobj in DTHS : dtobj.visible = 1 : next
for each fsobj in FSHS : fsobj.visible = 0 : next

else

for each dtobj in DTHS : dtobj.visible = 0 : next
for each fsobj in FSHS : fsobj.visible = 1 : next
End If

End Sub

Sub Table_KeyDown(ByVal keycode)

	If Keycode = StartGameKey And Players <4 And Credits > 0 And maxBalls = True Then

		If TimerPUP.Enabled = False Then
			Players = Players + 1 : Playsound"Added" : Controller.B2SSetCanPlay  Players : SetPlayers
			Credits = Credits - 1 :	UpdateCredits : CoinReel.SetValue Credits
		Else
			Playsound"Tilt"
		End If
		If Started = False And GameOver = True Then
			Started = True
			AllLampsOn
			GameOver = False : Controller.B2SSetGameover 0 : dtgameover.state=0
			If Players > 1 Then PU = PU + 1 : Controller.B2SSetPlayerUp PU : TimerPUP.Enabled = False : Else TimerPUP.Enabled = True:PU = 1 : SetPup
		Controller.B2SSetScoreRolloverPlayer1 0 : P1ScoreRoll.state=0 
		Controller.B2SSetScoreRolloverPlayer2 0 : P2ScoreRoll.state=0
		Controller.B2SSetScoreRolloverPlayer3 0 : P3ScoreRoll.state=0
		Controller.B2SSetScoreRolloverPlayer4 0 : P4ScoreRoll.state=0
			For i = 1 To 4
			Score(i) = 0 
			Controller.B2SSetScorePlayer i, 0
			Next
			For i = 1 To 4 : Rep1(i) = False : Next
			For i = 1 To 4 : Rep2(i) = False : Next
			For i = 1 To 4 : Rep3(i) = False : Next
			ScoreDT1.ResetToZero
			ScoreDT2.ResetToZero
			ScoreDT3.ResetToZero
			ScoreDT4.ResetToZero

   
			StepB2SData 0, 23, 0, 2, 180,"reels"
			SetB2SData 33,11
			SetB2SData 24,0 : SetB2SData 25,0 : SetB2SData 26,0 : SetB2SData 27,0
			Bonus = 0 : BonusCounted = True : DoubleBonus = False
			BIP = 0' : UpdateBIP
			MatchNumber = 11 : UpdateMatch
			EB = False : SpecialReady = True : EBReady = True
			LastBall = False
			FirstGame = False
			GameCount = GameCount + 1 : TextGC.Text = GameCount
			TimerFade.Enabled = True
			LightReset() : TargetReset()
			KickerRelease.TimerInterval = 6500 : KickerRelease.TimerEnabled = True
			TimerBall.Enabled = True : TimerBall.Interval = 6500
			If FiveBalls = False Then Playsound"initialize" : DOF 113, 2 Else Playsound"initialize"
			BallsText.Text = Balls
		End If
	End If

	If Keycode = AddCreditKey And Credits < 15 And TimerCoin.Enabled = False Then Playsound"Coin" : TimerCoin.Enabled = True

	If keycode = PlungerKey Then
		Plunger.Pullback
		PlaysoundAt "PlungerPull", plunger
	End If

	If keycode = LeftFlipperKey And GameOver = False Then
		If Tilt = False Then 
		DOF 101, 1
        PlaySoundAt SoundFX("FLLUp", DOFFlippers), LeftFlipper
         LeftFlipper.RotateToEnd
        LeftFlipperOn = 1
        LeftFlipper1.RotateToEnd
        LeftFlipper1On = 1
		Stopsound"Fliptric"
		Playsound"Fliptric",-1
		End If
	End If
    
	If keycode = RightFlipperKey And GameOver = False Then
		If Tilt = False Then 
		DOF 102, 1
        PlaySoundAt SoundFX("FLRUp", DOFFlippers), RightFlipper
        RightFlipper.RotateToEnd
        RightFlipperOn = 1
         RightFlipper1.RotateToEnd
        RightFlipper1On = 1
		Stopsound"Fliptric"
		Playsound"Fliptric",-1
		End If
	End If
    
	If keycode = LeftTiltKey Then Nudge 90, 1.4 : TiltCheck()
	If keycode = RightTiltKey Then Nudge 270, 1.4 : TiltCheck()
	If keycode = CenterTiltKey Then Nudge 0, 1.8 : TiltCheck()

End Sub

Sub Table_KeyUp(ByVal keycode)
 
	If keycode = PlungerKey Then
 Plunger.Fire
PlaysoundAt "Plunger", Plunger
End If
    
	If keycode = LeftFlipperKey Then
		DOF 101, 0
        PlaySoundAt SoundFX("FLLDown", DOFFlippers), LeftFlipper
        LeftFlipper.RotateToStart
        LeftFlipperOn = 0
        LeftFlipper1.RotateToStart
        LeftFlipper1On = 0
		Stopsound"Fliptric"
	End If
    
	If keycode = RightFlipperKey Then
		DOF 102, 0
        PlaySoundAt SoundFX("FLLDown", DOFFlippers), RightFlipper
        RightFlipper.RotateToStart
        RightFlipperOn = 0
        RightFlipper1.RotateToStart
        RightFlipper1On = 0
		Stopsound"Fliptric"
	End If

End Sub

Sub TimerPUP_Timer
	PU = PU + 1
	If PU = 5 Then PU = 1 : Me.Enabled = False : If Players > 1 Then PU = PU + 1 : Controller.B2SSetPlayerUp PU : Else PU = 1 : Controller.B2SSetPlayerUp PU : SetPup
	Select Case PU
		Case 1: Controller.B2SSetPlayerUp PU : SetPup
		Case 2: Controller.B2SSetPlayerUp PU : SetPup
		Case 3: Controller.B2SSetPlayerUp PU : SetPup
		Case 4: Controller.B2SSetPlayerUp PU : SetPup
		Case 5: PU = 1 : If Players > 1 Then PU = PU + 1 : Controller.B2SSetPlayerUp PU :  : SetPup : Else PU = 1 : Controller.B2SSetPlayerUp PU : SetPup
	End Select
	TextPU.Text = PU
End Sub

Sub TimerFlRot_Timer
	LFP.RotAndTra2=LeftFlipper.CurrentAngle
	RFP.RotAndTra2=RightFlipper.CurrentAngle
End Sub

Sub TimerCoin_Timer
    Credits = Credits + 1 : UpdateCredits
	Playsound"AddCredit"
	Me.Enabled = False
End Sub


'*********************************************************
' Real Time Flipper adjustments - by JLouLouLou & JPSalas
'        (to enable flipper tricks) 
'*********************************************************

Dim FlipperPower
Dim FlipperElasticity
Dim SOSTorque, SOSAngle
Dim FullStrokeEOS_Torque, LiveStrokeEOS_Torque
Dim LeftFlipperOn
Dim RightFlipperOn
Dim LeftFlipper1On
Dim RightFlipper1On

Dim LLiveCatchTimer
Dim RLiveCatchTimer
Dim L1LiveCatchTimer
Dim R1LiveCatchTimer
Dim LiveCatchSensivity

FlipperPower = 5000
FlipperElasticity = 0.85
FullStrokeEOS_Torque = 0.3 	' EOS Torque when flipper hold up ( EOS Coil is fully charged. Ampere increase due to flipper can't move or when it pushed back when "On". EOS Coil have more power )
LiveStrokeEOS_Torque = 0.2	' EOS Torque when flipper rotate to end ( When flipper move, EOS coil have less Ampere due to flipper can freely move. EOS Coil have less power )

LeftFlipper.EOSTorqueAngle = 10
RightFlipper.EOSTorqueAngle = 10
LeftFlipper1.EOSTorqueAngle = 10
RightFlipper1.EOSTorqueAngle = 10

SOSTorque = 0.1
SOSAngle = 6

LiveCatchSensivity = 10

LLiveCatchTimer = 0
RLiveCatchTimer = 0
L1LiveCatchTimer = 0
R1LiveCatchTimer = 0

LeftFlipper.TimerInterval = 1
LeftFlipper.TimerEnabled = 1

Sub LeftFlipper_Timer 'flipper's tricks timer
'Start Of Stroke Flipper Stroke Routine : Start of Stroke for Tap pass and Tap shoot
    If LeftFlipper.CurrentAngle >= LeftFlipper.StartAngle - SOSAngle Then LeftFlipper.Strength = FlipperPower * SOSTorque else LeftFlipper.Strength = FlipperPower : End If
 
'End Of Stroke Routine : Livecatch and Emply/Full-Charged EOS
	If LeftFlipperOn = 1 Then
		If LeftFlipper.CurrentAngle = LeftFlipper.EndAngle then
			LeftFlipper.EOSTorque = FullStrokeEOS_Torque
			LLiveCatchTimer = LLiveCatchTimer + 1
			If LLiveCatchTimer < LiveCatchSensivity Then
				LeftFlipper.Elasticity = 0
			Else
				LeftFlipper.Elasticity = FlipperElasticity
				LLiveCatchTimer = LiveCatchSensivity
			End If
		End If
	Else
		LeftFlipper.Elasticity = FlipperElasticity
		LeftFlipper.EOSTorque = LiveStrokeEOS_Torque
		LLiveCatchTimer = 0
	End If

'Start Of Stroke Flipper Stroke Routine : Start of Stroke for Tap pass and Tap shoot
    If LeftFlipper1.CurrentAngle >= LeftFlipper1.StartAngle - SOSAngle Then LeftFlipper1.Strength = FlipperPower * SOSTorque else LeftFlipper1.Strength = FlipperPower : End If
 
'End Of Stroke Routine : Livecatch and Emply/Full-Charged EOS
	If LeftFlipper1On = 1 Then
		If LeftFlipper1.CurrentAngle = LeftFlipper1.EndAngle then
			LeftFlipper1.EOSTorque = FullStrokeEOS_Torque
			L1LiveCatchTimer = L1LiveCatchTimer + 1
			If L1LiveCatchTimer < LiveCatchSensivity Then
				LeftFlipper1.Elasticity = 0
			Else
				LeftFlipper1.Elasticity = FlipperElasticity
				L1LiveCatchTimer = LiveCatchSensivity
			End If
		End If
	Else
		LeftFlipper1.Elasticity = FlipperElasticity
		LeftFlipper1.EOSTorque = LiveStrokeEOS_Torque
		L1LiveCatchTimer = 0
	End If


	

'Start Of Stroke Flipper Stroke Routine : Start of Stroke for Tap pass and Tap shoot
    If RightFlipper.CurrentAngle <= RightFlipper.StartAngle + SOSAngle Then RightFlipper.Strength = FlipperPower * SOSTorque else RightFlipper.Strength = FlipperPower : End If
 
'End Of Stroke Routine : Livecatch and Emply/Full-Charged EOS
 	If RightFlipperOn = 1 Then
		If RightFlipper.CurrentAngle = RightFlipper.EndAngle Then
			RightFlipper.EOSTorque = FullStrokeEOS_Torque
			RLiveCatchTimer = RLiveCatchTimer + 1
			If RLiveCatchTimer < LiveCatchSensivity Then
				RightFlipper.Elasticity = 0
			Else
				RightFlipper.Elasticity = FlipperElasticity
				RLiveCatchTimer = LiveCatchSensivity
			End If
		End If
	Else
		RightFlipper.Elasticity = FlipperElasticity
		RightFlipper.EOSTorque = LiveStrokeEOS_Torque
		RLiveCatchTimer = 0
	End If

'Start Of Stroke Flipper Stroke Routine : Start of Stroke for Tap pass and Tap shoot
    If RightFlipper1.CurrentAngle <= RightFlipper1.StartAngle + SOSAngle Then RightFlipper1.Strength = FlipperPower * SOSTorque else RightFlipper1.Strength = FlipperPower : End If
 
'End Of Stroke Routine : Livecatch and Emply/Full-Charged EOS
 	If RightFlipper1On = 1 Then
		If RightFlipper1.CurrentAngle = RightFlipper1.EndAngle Then
			RightFlipper1.EOSTorque = FullStrokeEOS_Torque
			R1LiveCatchTimer = R1LiveCatchTimer + 1
			If R1LiveCatchTimer < LiveCatchSensivity Then
				RightFlipper1.Elasticity = 0
			Else
				RightFlipper1.Elasticity = FlipperElasticity
				R1LiveCatchTimer = LiveCatchSensivity
			End If
		End If
	Else
		RightFlipper1.Elasticity = FlipperElasticity
		RightFlipper1.EOSTorque = LiveStrokeEOS_Torque
		R1LiveCatchTimer = 0
	End If


End Sub


'-----------------------------------------------------------------------

Sub TimerTilt_Timer()
    Tiltswing = (Tiltswing / 6) * 5
    If Tiltswing < 0.001 Then Tiltswing = 0 : TimerTilt.Enabled = False 
End Sub

Sub Tiltcheck() 
	TimerTilt.Enabled = True
	If Newtilt = Oldtilt Then
		Tiltswing = Tiltswing + 0.40 : Playsound"NudgeA"
	Else
		Tiltswing = Tiltswing + 0.25 : Playsound"NudgeB"
	End If
	Oldtilt = Newtilt
	If Tiltswing > 1 And GameOver = False And Tilt = False Then
		Tilt = True : BumperOff() : Controller.B2SSetTilt 1 : AllLampsOff
		LeftFlipper.RotateToStart : RightFlipper.RotateToStart : Playsound"FLLDown"
		LeftFlipper1.RotateToStart : RightFlipper1.RotateToStart : Playsound"FLRDown"		
		Playsound"Tilt"
	End If
End Sub

Sub CheckSpeed
	SpeedX = BallSpeed.velx: SpeedY = BallSpeed.vely
	FinalSpeed = SQR(BallSpeed.velx * BallSpeed.velx + BallSpeed.vely * BallSpeed.vely)
End Sub

Sub DropOff
	BR1On.WidthBottom = 0 : BR1On.WidthTop = 0 : Abs(LightAniBR1.State -1)
	BR2On.WidthBottom = 0 : BR2On.WidthTop = 0 : Abs(LightAniBR2.State -1)
	BBOn.WidthBottom = 0 : BBOn.WidthTop = 0 : Abs(LightAniBB.State -1)
	BR1Ring1.IsDropped = True : BR1Ring2.IsDropped = True : BR1Ring3.IsDropped = True
	BR2Ring1.IsDropped = True : BR2Ring2.IsDropped = True : BR2Ring3.IsDropped = True
	BBRing1.IsDropped = True : BBRing2.IsDropped = True : BBRing3.IsDropped = True	
End Sub

Sub Drain_Hit()
		DOF 112, 2
	Drain.DestroyBall
	BonusCount
	PlaySound "Drain"
	Controller.B2SSetBallInPlay 0 : BallInPlay.state=0
End Sub

Sub TimerNewBall_Timer
	GameInProgress = False
	Tilt = False : Controller.B2SSetTilt 0
	If Balls = 1 And CurrentPlayer = Players And EB = False Then 
		GameOver = True : Playsound"Reset" : TimernClose.Enabled = True : AllLampsOff
P1ScoreRoll.state=0 
P2ScoreRoll.state=0
P3ScoreRoll.state=0
P4ScoreRoll.state=0

	Else
		If EB = True Then KickerRelease.TimerInterval = 1000 : KickerRelease.TimerEnabled = True
		If EB = False Then
			KickerRelease.TimerInterval = 1000 : KickerRelease.TimerEnabled = True
			If CurrentPlayer = Players Then
				PU = PU - CurrentPlayer : Controller.B2SSetPlayerUp PU : SetPup
				UpdateScore
				CurrentPlayer = 1 : Balls = Balls - 1 : maxBalls = False
				TimerBall.Enabled = True
			Else
				UpdateScore
				CurrentPlayer = CurrentPlayer + 1 : UpdateBIP
			End If
			PU = PU + 1 : Controller.B2SSetPlayerUp PU : SetPup
			TargetReset()
			BumperReset()
			LightReset()
			BOn = False
			Bonus = 0 : BonusCounted = True : DoubleBonus = False
		End If
	End If
	UpdateBIP
	Me.Enabled = False
End Sub

Sub TimerBall_Timer
	If EB = False Then BIP = BIP + 1 : BallsText.Text = Balls
	UpdateBIP
	Me.Enabled = False : Me.Interval = 1000
End Sub

Sub KickerRelease_Timer
	Set BallSpeed = KickerRelease.Createsizedball(29.2)
	CheckSpeed
	BumperAttTimer.Enabled = True
	If EB = True Then
		SetLamp 32,1 : SetB2SData 35, 1 
		TargetReset()
		BumperReset()
		LightReset()
		BOn = False
		Bonus = 0 : BonusCounted = True : DoubleBonus = False
		UpdateBIP
	End If
	CenterOut.Enabled = True
	KickerRelease.Kick 90,8 : Playsound"BallRel" : DOF 112, 2 : GameInProgress = True
	Me.TimerEnabled = False
End Sub

Sub AddScore(Points)
	If GameInProgress = False Or Tilt = True Then Exit Sub
	Score(CurrentPlayer) = Score(CurrentPlayer) + Points
if Score(CurrentPlayer)=>99000 then
			If PU=1 Then
				Controller.B2SSetScoreRolloverPlayer1 1 : P1ScoreRoll.state=1
			End If
			If PU=2 Then
				Controller.B2SSetScoreRolloverPlayer2 1 : P2ScoreRoll.state=1
			End If

			If PU=3 Then
				Controller.B2SSetScoreRolloverPlayer3 1 : P3ScoreRoll.state=1
			End If

			If PU=4 Then
				Controller.B2SSetScoreRolloverPlayer4 1 : P4ScoreRoll.state=1
			End If
		End If
Controller.B2SSetScorePlayer PU, Score(CurrentPlayer)
	
If PU=1 then ScoreDT1.AddValue(Points)
If PU=2 then ScoreDT2.AddValue(Points)
If PU=3 then ScoreDT3.AddValue(Points)
If PU=4 then ScoreDT4.AddValue(Points)

	UpdateScore
	CheckReplay()
	
End Sub

Sub TimerScoreTsd_Timer
	If GameInProgress = False Or Tilt = True Then Exit Sub
	Me.Interval = 75
	Score(CurrentPlayer) = Score(CurrentPlayer) + Points
	UpdateScore
If PU=1 then ScoreDT1.AddValue(Points)
If PU=2 then ScoreDT2.AddValue(Points)
If PU=3 then ScoreDT3.AddValue(Points)
If PU=4 then ScoreDT4.AddValue(Points)
	sTsd = sTsd+1
	If sTsd >5 Then sTsd = 0 : Me.Enabled = False
		Select Case sTsd
			Case 1: AddScore 1000 : UpdateScore : Playsound"1000A" : DOF 131, 2 : Me.Interval = 100 : BumperReset() : BumperAttTimer.Interval = 150 : BumperAttTimer.Enabled = True : Playsound"Switch4"
			Case 2: AddScore 1000 : UpdateScore : Playsound"1000B" : DOF 131, 2 : Me.Interval = 75
			Case 3: AddScore 1000 : UpdateScore : Playsound"1000A" : DOF 131, 2' : Me.Interval = 75
			Case 4: AddScore 1000 : UpdateScore : Playsound"1000B" : DOF 131, 2 : Me.Interval = 75
			Case 5: AddScore 1000 : UpdateScore : Playsound"1000A" : DOF 131, 2 : Me.Interval = 75 : BumperReset2()
		End Select
	CheckReplay()
End Sub

Sub TimerScoreHdd_Timer
	If GameInProgress = False Or Tilt = True Then Exit Sub
	Me.Interval = 75
	Score(CurrentPlayer) = Score(CurrentPlayer) + Points
	UpdateScore
If PU=1 then ScoreDT1.AddValue(Points)
If PU=2 then ScoreDT2.AddValue(Points)
If PU=3 then ScoreDT3.AddValue(Points)
If PU=4 then ScoreDT4.AddValue(Points)
	sHdd = sHdd+1
	If sHdd >5 Then sHdd = 0 : Me.Enabled = False
		Select Case sHdd
			Case 1: AddScore 100 : UpdateScore : Playsound"100A" : DOF 130, 2 : Me.Interval = 100 : BumperReset() : Playsound"Switch4"
			Case 2: AddScore 100 : UpdateScore : Playsound"100B" : DOF 130, 2 : Me.Interval = 75
			Case 3: AddScore 100 : UpdateScore : Playsound"100A" : DOF 130, 2' : Me.Interval = 75
			Case 4: AddScore 100 : UpdateScore : Playsound"100B" : DOF 130, 2 : Me.Interval = 75
			Case 5: AddScore 100 : UpdateScore : Playsound"100A" : DOF 130, 2 : Me.Interval = 75 : BumperReset2()
		End Select
	CheckReplay()
End Sub

Sub TimerScoreHdd2_Timer
	If GameInProgress = False Or Tilt = True Then Exit Sub
	Me.Interval = 75
	Score(CurrentPlayer) = Score(CurrentPlayer) + Points
	UpdateScore
If PU=1 then ScoreDT1.AddValue(Points)
If PU=2 then ScoreDT2.AddValue(Points)
If PU=3 then ScoreDT3.AddValue(Points)
If PU=4 then ScoreDT4.AddValue(Points)

	sHdd2 = sHdd2+1
	If sHdd2 >5 Then sHdd2 = 0 : Me.Enabled = False
		Select Case sHdd2
			Case 1: AddScore 100 : UpdateScore : Playsound"100A" : DOF 130, 2 : Me.Interval = 100 : BumperReset() : Playsound"Switch4"
			Case 2: AddScore 100 : UpdateScore : Playsound"100B" : DOF 130, 2 : Me.Interval = 75
			Case 3: AddScore 100 : UpdateScore : Playsound"100A" : DOF 130, 2' : Me.Interval = 75
			Case 4: AddScore 100 : UpdateScore : Playsound"100B" : DOF 130, 2 : Me.Interval = 75
			Case 5: AddScore 100 : UpdateScore : Playsound"100A" : DOF 130, 2 : Me.Interval = 75 : BumperReset2()
		End Select
	CheckReplay()
End Sub

Sub CheckReplay()
	If FiveBalls = True Then
		If Score(CurrentPlayer) + Points => 100000 And Rep1(CurrentPlayer) = False Then Special() : Rep1(CurrentPlayer) = True
		If Score(CurrentPlayer) + Points => 140000 And Rep2(CurrentPlayer) = False Then Special() : Rep2(CurrentPlayer) = True
		If Score(CurrentPlayer) + Points => 180000 And Rep3(CurrentPlayer) = False Then Special() : Rep3(CurrentPlayer) = True
	Else
		If Score(CurrentPlayer) + Points => 70000 And Rep1(CurrentPlayer) = False Then Special() : Rep1(CurrentPlayer) = True
		If Score(CurrentPlayer) + Points => 100000 And Rep2(CurrentPlayer) = False Then Special() : Rep2(CurrentPlayer) = True
		If Score(CurrentPlayer) + Points => 140000 And Rep3(CurrentPlayer) = False Then Special() : Rep3(CurrentPlayer) = True
    End If
	If CurrentPlayer = 1 Then
		If Score(1) + Points =>100000 And Score(1) + Points <200000 Then Controller.B2SSetScoreRolloverPlayer1 1 : P1ScoreRoll.state=1
		If Score(1) + Points =>200000 And Score(1) + Points <300000 Then SetB2SData 24,2
		If Score(1) + Points =>300000 And Score(1) + Points <400000 Then SetB2SData 24,3
		If Score(1) + Points =>400000 And Score(1) + Points <500000 Then SetB2SData 24,4
		If Score(1) + Points =>500000 Then SetB2SData 24,5
	End If
	If CurrentPlayer = 2 Then
		If Score(2) + Points =>100000 And Score(2) + Points <200000 Then Controller.B2SSetScoreRolloverPlayer2 1 : P2ScoreRoll.state=1
		If Score(2) + Points =>200000 And Score(2) + Points <300000 Then SetB2SData 25,2
		If Score(2) + Points =>300000 And Score(2) + Points <400000 Then SetB2SData 25,3
		If Score(2) + Points =>400000 And Score(2) + Points <500000 Then SetB2SData 25,4
		If Score(2) + Points =>500000 Then SetB2SData 25,5
	End If
	If CurrentPlayer = 3 Then
		If Score(3) + Points =>100000 And Score(3) + Points <200000 Then Controller.B2SSetScoreRolloverPlayer3 1 : P3ScoreRoll.state=1
		If Score(3) + Points =>200000 And Score(3) + Points <300000 Then SetB2SData 26,2
		If Score(3) + Points =>300000 And Score(3) + Points <400000 Then SetB2SData 26,3
		If Score(3) + Points =>400000 And Score(3) + Points <500000 Then SetB2SData 26,4
		If Score(3) + Points =>500000 Then SetB2SData 26,5
	End If
	If CurrentPlayer = 4 Then
		If Score(4) + Points =>100000 And Score(4) + Points <200000 Then Controller.B2SSetScoreRolloverPlayer4 1 : P4ScoreRoll.state=1
		If Score(4) + Points =>200000 And Score(4) + Points <300000 Then SetB2SData 27,2
		If Score(4) + Points =>300000 And Score(4) + Points <400000 Then SetB2SData 27,3
		If Score(4) + Points =>400000 And Score(4) + Points <500000 Then SetB2SData 27,4
		If Score(4) + Points =>500000 Then SetB2SData 27,5
	End If
End Sub

Sub UpdateScore
	Score(Currentplayer) = Score(Currentplayer) + Points
If PU=1 then ScoreDT1.AddValue(Points)
If PU=2 then ScoreDT2.AddValue(Points)
If PU=3 then ScoreDT3.AddValue(Points)
If PU=4 then ScoreDT4.AddValue(Points)
	Score1000K=Int (Score(Currentplayer)/1000000)
	Score100K=Int ((Score(Currentplayer)/100000))	
	Score10K=Int ((Score(Currentplayer)/10000))														
	ScoreK=Int (((Score(Currentplayer)-(Score10K*10000)))/1000)										
	Score100=Int (((Score(Currentplayer)-(Score10K*10000)-(ScoreK*1000)))/100)						
	Score10=Int (((Score(Currentplayer)-(Score10K*10000)-(ScoreK*1000)-(Score100*100)))/10)			
	ScoreUnit=Int ((Score(Currentplayer)-(Score10K*10000)-(ScoreK*1000)-(Score100*100))-(Score10*10))
	b2sSplitScore CurrentPlayer, Score(CurrentPlayer)
End Sub

Sub b2sSplitScore (Byval b2splayer, Byval b2sscorevalue)
	Dim B2Ssplit
	b2splayer=(b2splayer-1)*6
	B2Ssplit= Int(b2sscorevalue/1000000):SetB2SData b2splayer,Chr(B2Ssplit+1):b2sscorevalue=b2sscorevalue-Int(B2Ssplit*1000000)
	B2Ssplit= Int(b2sscorevalue/100000):SetB2SData b2splayer,Chr(B2Ssplit+1):b2sscorevalue=b2sscorevalue-Int(B2Ssplit*100000)
	B2Ssplit= Int(b2sscorevalue/10000):SetB2SData b2splayer+1,Chr(B2Ssplit+1):b2sscorevalue=b2sscorevalue-Int(B2Ssplit*10000)
	B2Ssplit= Int(b2sscorevalue/1000):SetB2SData b2splayer+2,Chr(B2Ssplit+1):b2sscorevalue=b2sscorevalue-Int(B2Ssplit*1000)
	B2Ssplit= Int(b2sscorevalue/100):SetB2SData b2splayer+3,Chr(B2Ssplit+1):b2sscorevalue=b2sscorevalue-Int(B2Ssplit*100)
	B2Ssplit= Int(b2sscorevalue/10):SetB2SData b2splayer+4,Chr(B2Ssplit+1):b2sscorevalue=b2sscorevalue-Int(B2Ssplit*10)
	SetB2SData b2splayer+5,b2sscorevalue
End Sub

Sub UpdatePreScore
'	Score1.SetValue(PreScore)
  	PreScore100K=Int (PreScore1/100000)	
  	PreScore10K=Int (PreScore1/10000)
 	PreScoreK=Int((PreScore1-(PreScore10K*10000))/1000)
 	PreScore100=Int((PreScore1-(PreScore10K*10000)-(PreScoreK*1000))/100)
 	PreScore10=Int((PreScore1-(PreScore10K*10000)-(PreScoreK*1000)-(PreScore100*100))/10)
 	PreScoreUnit=Int(PreScore1-(PreScore10K*10000)-(PreScoreK*1000)-(PreScore100*100)-(PreScore10*10))
'  	ScoreReel01.SetValue(PreScore100K)
'  	ScoreReel02.SetValue(PreScore10K)
' 	ScoreReel03.SetValue(PreScoreK)
' 	ScoreReel04.SetValue(PreScore100)
' 	ScoreReel05.SetValue(PreScore10)
' 	ScoreReel06.SetValue(PreScoreUnit)
	b2sSplitScore 1, PreScore1
	b2sSplitScore 2, PreScore2
	b2sSplitScore 3, PreScore3
	b2sSplitScore 4, PreScore4

	If Prescore1 =>100000 And Prescore1 <200000 Then Controller.B2SSetScoreRolloverPlayer1 1 : P1ScoreRoll.state=1
	If Prescore1 =>200000 And Prescore1 <300000 Then SetB2SData 24,2
	If Prescore1 =>300000 And Prescore1 <400000 Then SetB2SData 24,3
	If Prescore1 =>400000 And Prescore1 <500000 Then SetB2SData 24,4
	If Prescore1 =>500000 Then SetB2SData 24,5

	If Prescore2 =>100000 And Prescore2 <200000 Then Controller.B2SSetScoreRolloverPlayer2 1 : P2ScoreRoll.state=1
	If Prescore2 =>200000 And Prescore2 <300000 Then SetB2SData 25,2
	If Prescore2 =>300000 And Prescore2 <400000 Then SetB2SData 25,3
	If Prescore2 =>400000 And Prescore2 <500000 Then SetB2SData 25,4
	If Prescore2 =>500000 Then SetB2SData 25,5

	If Prescore3 =>100000 And Prescore3 <200000 Then Controller.B2SSetScoreRolloverPlayer3 1 : P3ScoreRoll.state=1
	If Prescore3 =>200000 And Prescore3 <300000 Then SetB2SData 26,2
	If Prescore3 =>300000 And Prescore3 <400000 Then SetB2SData 26,3
	If Prescore3 =>400000 And Prescore3 <500000 Then SetB2SData 26,4
	If Prescore3 =>500000 Then SetB2SData 26,5

	If Prescore4 =>100000 And Prescore4 <200000 Then Controller.B2SSetScoreRolloverPlayer4 1 : P4ScoreRoll.state=1
	If Prescore4 =>200000 And Prescore4 <300000 Then SetB2SData 27,2
	If Prescore4 =>300000 And Prescore4 <400000 Then SetB2SData 27,3
	If Prescore4 =>400000 And Prescore4 <500000 Then SetB2SData 27,4
	If Prescore4 =>500000 Then SetB2SData 27,5

	If Prescore1 => 0 Then SetB2SData 36,1 Else SetB2SData 36,0
	If Prescore2 => 0 Then SetB2SData 37,1 Else SetB2SData 37,0
	If Prescore3 => 0 Then SetB2SData 38,1 Else SetB2SData 38,0
	If Prescore4 => 0 Then SetB2SData 39,1 Else SetB2SData 39,0 
End Sub

Sub Updatehs
	h3s = HighScore3
	h5s = HighScore5

	h3s1000K=Int (h3s/1000000)
	h3s100K=Int (h3s/100000)	
	h3s10K=Int (h3s/10000)														
	h3sK=Int((h3s-(h3s10K*10000))/1000)										
	h3s100=Int((h3s-(h3s10K*10000)-(h3sK*1000))/100)						
	h3s10=Int((h3s-(h3s10K*10000)-(h3sK*1000)-(h3s100*100))/10)			
	h3sUnit=Int(h3s-(h3s10K*10000)-(h3sK*1000)-(h3s100*100)-(h3s10*10))

	h5s1000K=Int (h5s/1000000)
	h5s100K=Int (h5s/100000)	
	h5s10K=Int (h5s/10000)														
	h5sK=Int((h5s-(h5s10K*10000))/1000)										
	h5s100=Int((h5s-(h5s10K*10000)-(h5sK*1000))/100)						
	h5s10=Int((h5s-(h5s10K*10000)-(h5sK*1000)-(h5s100*100))/10)			
	h5sUnit=Int(h5s-(h5s10K*10000)-(h5sK*1000)-(h5s100*100)-(h5s10*10))

	If FiveBalls = False Then
		hsg.SetValue(h3s1000K)
		hsa.SetValue(h3s100K)
		hsb.SetValue(h3s10K)
		hsc.SetValue(h3sK)
		hsd.SetValue(h3s100)
		hse.SetValue(h3s10)
		hsf.SetValue(h3sUnit)

		hsg001.SetValue(h3s1000K)
		hsa001.SetValue(h3s100K)
		hsb001.SetValue(h3s10K)
		hsc001.SetValue(h3sK)
		hsd001.SetValue(h3s100)
		hse001.SetValue(h3s10)
		hsf001.SetValue(h3sUnit)
	Else 
		hsg.SetValue(h5s1000K)
		hsa.SetValue(h5s100K)
		hsb.SetValue(h5s10K)
		hsc.SetValue(h5sK)
		hsd.SetValue(h5s100)
		hse.SetValue(h5s10)
		hsf.SetValue(h5sUnit)	

		hsg001.SetValue(h5s1000K)
		hsa001.SetValue(h5s100K)
		hsb001.SetValue(h5s10K)
		hsc001.SetValue(h5sK)
		hsd001.SetValue(h5s100)
		hse001.SetValue(h5s10)
		hsf001.SetValue(h5sUnit)	
	End If
End Sub

Sub UpdateCredits
	Controller.B2SSetCredits Credits
CoinReel.SetValue Credits
End Sub

Sub UpdateBIP
	If FiveBalls = True Then
		If BIP = 5 Then LastBall = True
	Else
		If BIP = 3 Then LastBall = True
	End If
	Controller.B2SSetCredits BIP : CoinReel.SetValue BIP
	If LastBall = True Then DoubleBonus = True : SetLamp 0, 1
End Sub

Sub UpdateMatch
    If Matchnumber = 00 Then SetB2SData 33, 0' : MatchReel.SetValue (1)
    If Matchnumber = 10 Then SetB2SData 33, 1' : MatchReel.SetValue (2)
    If Matchnumber = 20 Then SetB2SData 33, 2' : MatchReel.SetValue (3)
    If Matchnumber = 30 Then SetB2SData 33, 3' : MatchReel.SetValue (4)
    If Matchnumber = 40 Then SetB2SData 33, 4' : MatchReel.SetValue (5)
    If Matchnumber = 50 Then SetB2SData 33, 5' : MatchReel.SetValue (6)
    If Matchnumber = 60 Then SetB2SData 33, 6' : MatchReel.SetValue (7)
    If Matchnumber = 70 Then SetB2SData 33, 7' : MatchReel.SetValue (8)
    If Matchnumber = 80 Then SetB2SData 33, 8' : MatchReel.SetValue (9)
    If Matchnumber = 90 Then SetB2SData 33, 9' : MatchReel.SetValue (10)
End Sub

Sub MatchDisplay()
    If y = 00 Then SetB2SData 33, 0' : MatchReel.SetValue (1)
    If y = 10 Then SetB2SData 33, 1' : MatchReel.SetValue (2)
    If y = 20 Then SetB2SData 33, 2' : MatchReel.SetValue (3)
    If y = 30 Then SetB2SData 33, 3' : MatchReel.SetValue (4)
    If y = 40 Then SetB2SData 33, 4' : MatchReel.SetValue (5)
    If y = 50 Then SetB2SData 33, 5' : MatchReel.SetValue (6)
    If y = 60 Then SetB2SData 33, 6' : MatchReel.SetValue (7)
    If y = 70 Then SetB2SData 33, 7' : MatchReel.SetValue (8)
    If y = 80 Then SetB2SData 33, 8' : MatchReel.SetValue (9)
    If y = 90 Then SetB2SData 33, 9' : MatchReel.SetValue (10)
	Playsound"Switch4"
End Sub

Sub Match()
    Randomize
    y = Int(Rnd(10) * 10) * 10
    MatchNumber = y
    For i = 1 To Players
        If Int(Score(i)/10)-Int(Score(i)/100)*10 = y/10 Then
		   If Credits < 15 Then Call Credit() Else Playsound"Knocker" : DOF 111, 2
		End If
	Next
    Call Matchdisplay()
End Sub

Sub TimernClose_Timer
	Match() : nClose() : Playsound"Close"
	Me.Enabled = False
End Sub

Sub nClose()
	SaveData()
	UpdatePreScore
	TimerInit.Enabled = True : Playsound"MotorEmpty" : DOF 113, 2
End Sub

Sub SaveData()
	If FiveBalls = True Then
		For i = 1 To Players
			If Score(i) > HighScore5 Then
				HighScore5 = Score(i)
				Updatehs
				SaveValue TableName, "HighScore5", HighScore5
			End If
		Next
	Else
		For i = 1 To Players
			If Score(i) > HighScore3 Then
				HighScore3 = Score(i)
				Updatehs
				SaveValue TableName, "HighScore3", HighScore3
			End If
		Next
	End If

	PreScore1 = Score(1)
   	SaveValue TableName, "PreScore1", PreScore1
	PreScore2 = Score(2)
   	SaveValue TableName, "PreScore2", PreScore2
	PreScore3 = Score(3)
   	SaveValue TableName, "PreScore3", PreScore3
	PreScore4 = Score(4)
   	SaveValue TableName, "PreScore4", PreScore4
    SaveValue TableName, "Credits", Credits
    SaveValue TableName, "MatchNumber", MatchNumber
	SaveValue TableName, "GameCount", GameCount
End Sub

Sub TimerInit_Timer
	Table_Reset : TargetReset()
	Me.Enabled = False
End Sub

Sub LoadData()
	Dim Value
	Highscore3 = h3s
	Highscore5 = h5s
	Value = (LoadValue(TableName,"PreScore1"))
    If (Value <> "") Then PreScore1 = CDbl(Value) End If
	Value = (LoadValue(TableName,"PreScore2"))
    If (Value <> "") Then PreScore2 = CDbl(Value) End If
	Value = (LoadValue(TableName,"PreScore3"))
    If (Value <> "") Then PreScore3 = CDbl(Value) End If
	Value = (LoadValue(TableName,"PreScore4"))
    If (Value <> "") Then PreScore4 = CDbl(Value) End If
	Value = (LoadValue(TableName,"HighScore3"))
    If (Value <> "") Then HighScore3 = CDbl(Value) End If
	Value = (LoadValue(TableName,"HighScore5"))
    If (Value <> "") Then HighScore5 = CDbl(Value) End If
	Value = (LoadValue(TableName,"Credits"))
    If (Value <> "") Then Credits = CDbl(Value) End If
	Value = (LoadValue(TableName,"MatchNumber"))
    If (Value <> "") Then MatchNumber = CDbl(Value) End If
	Value = (LoadValue(TableName,"GameCount"))
    If (Value <> "") Then GameCount = CDbl(Value) End If
	If Credits > 15 Then Credits = 15
End Sub

Sub Special()
	Call Credit()
End Sub

Sub Credit()
    If Credits < 15 Then
		Credits = Credits + 1 : Playsound"Knocker" : UpdateCredits
	Else
		Playsound"Knocker"
	End If
End Sub

Sub BonusCount
	If Tilt = False Then
		If Bonus > 10 Then
			BonusCounted = False
			TimerBonus10.Enabled = True
		Else
			If Bonus <1 Then
				Bonuscounted = True : TimerTemp.Enabled = True
			Else
				BonusCounted = False : TimerBonus.Enabled = True
			End If				
		End If
	Else
		TimerNewBall.Enabled = True
	End If
End Sub

Sub TimerDouble_Timer
	If GameInProgress = False Or Tilt = True Then Exit Sub
	Me.Interval = 60
	Score(CurrentPlayer) = Score(CurrentPlayer) + Points
	UpdateScore
If PU=1 then ScoreDT1.AddValue(Points)
If PU=2 then ScoreDT2.AddValue(Points)
If PU=3 then ScoreDT3.AddValue(Points)
If PU=4 then ScoreDT4.AddValue(Points)
	TimerBonus.Interval = 150 : TimerBonus10.Interval = 150
	sTsd = sTsd+1
	If sTsd >2 Then sTsd = 0 : Me.Enabled = False
		Select Case sTsd
			Case 1: AddScore 1000 : UpdateScore : Playsound"1000A"
			Case 2: AddScore 1000 : UpdateScore : Playsound"1000B"
		End Select
	CheckReplay()
End Sub

Sub TimerBonus_Timer()
	If Tilt = True Then Exit Sub
	If DoubleBonus = True Then
		TimerDouble.Enabled = True
	Else 
		AddScore 1000 : Playsound"1000"
	End If
	Bonus = Bonus - 1
	AdvanceLights()
	
	If Bonus => 10 Then SetLamp 10,1 : Else SetLamp 10,0
	If Bonus = 9 Or Bonus = 19 Then SetLamp 9,1 : Else SetLamp 9,0
	If Bonus = 8 Or Bonus = 18 Then SetLamp 8,1 : Else SetLamp 8,0
	If Bonus = 7 Or Bonus = 17 Then SetLamp 7,1 : Else SetLamp 7,0
	If Bonus = 6 Or Bonus = 16 Then SetLamp 6,1 : Else SetLamp 6,0
	If Bonus = 5 Or Bonus = 15 Then SetLamp 5,1 : Else SetLamp 5,0
	If Bonus = 4 Or Bonus = 14 Then SetLamp 4,1 : Else SetLamp 4,0
	If Bonus = 3 Or Bonus = 13 Then SetLamp 3,1 : Else SetLamp 3,0
	If Bonus = 2 Or Bonus = 12 Then SetLamp 2,1 : Else SetLamp 2,0
	If Bonus = 1 Or Bonus = 11 Then SetLamp 1,1 : Else SetLamp 1,0

	If Bonus <1 Then Me.Enabled = False : TimerTemp.Enabled = True
	BonusText.Text = Bonus
End Sub

Sub TimerTemp_Timer
	BonusCounted = True : Playsound "Switch2"
	TimerNewBall.Enabled = True
	TimerBonus.Interval = 250 : TimerBonus.Enabled = False
	Me.Enabled = False
End Sub

Sub TimerBonus10_Timer()
	If Tilt = True Then Exit Sub
	If DoubleBonus = True Then
		TimerDouble.Enabled = True : Me.Interval = 175
	Else 
		AddScore 1000 : Playsound"1000"
	End If
	Bonus = Bonus - 1
	AdvanceLights()
	
	If Bonus = 19 Then SetLamp 9,1 : Else SetLamp 9,0
	If Bonus = 18 Then SetLamp 8,1 : Else SetLamp 8,0
	If Bonus = 17 Then SetLamp 7,1 : Else SetLamp 7,0
	If Bonus = 16 Then SetLamp 6,1 : Else SetLamp 6,0
	If Bonus = 15 Then SetLamp 5,1 : Else SetLamp 5,0
	If Bonus = 14 Then SetLamp 4,1 : Else SetLamp 4,0
	If Bonus = 13 Then SetLamp 3,1 : Else SetLamp 3,0
	If Bonus = 12 Then SetLamp 2,1 : Else SetLamp 2,0
	If Bonus = 11 Then SetLamp 1,1 : Else SetLamp 1,0
	If Bonus < 11 Then
		TimerBonus.Enabled = True
		TimerBonus10.Interval = 250 : Me.Enabled = False
	End If
	BonusText.Text = Bonus
End Sub

Sub Advance()
	If Tilt = True Then Exit Sub
    Bonus = Bonus + 1
    If Bonus => 19 Then
        Bonus = 19
    End If
    AdvanceLights()
 	UpdateScore
	BonusText.Text = Bonus
End Sub

Sub AdvanceLights()
	If Bonus >10 Then SetLamp 10,1 : AdvanceLights10()
	If Bonus = 10 Then SetLamp 10,1 : SetLamp 9,0
	If Bonus = 9 Then SetLamp 9,1 : SetLamp 8,0
	If Bonus = 8 Then SetLamp 8,1 : SetLamp 7,0
	If Bonus = 7 Then SetLamp 7,1 : SetLamp 6,0
	If Bonus = 6 Then SetLamp 6,1 : SetLamp 5,0
	If Bonus = 5 Then SetLamp 5,1 : SetLamp 4,0
	If Bonus = 4 Then SetLamp 4,1 : SetLamp 3,0
	If Bonus = 3 Then SetLamp 3,1 : SetLamp 2,0
	If Bonus = 2 Then SetLamp 2,1 : SetLamp 1,0
	If Bonus = 1 Then SetLamp 1,1
	BonusText.Text = Bonus
End Sub

Sub AdvanceLights10()
	If Bonus = 19 Then SetLamp 10,1 : SetLamp 9,1 : SetLamp 8,0
	If Bonus = 18 Then SetLamp 10,1 : SetLamp 8,1 : SetLamp 7,0
	If Bonus = 17 Then SetLamp 10,1 : SetLamp 7,1 : SetLamp 6,0
	If Bonus = 16 Then SetLamp 10,1 : SetLamp 6,1 : SetLamp 5,0
	If Bonus = 15 Then SetLamp 10,1 : SetLamp 5,1 : SetLamp 4,0
	If Bonus = 14 Then SetLamp 10,1 : SetLamp 4,1 : SetLamp 3,0
	If Bonus = 13 Then SetLamp 10,1 : SetLamp 3,1 : SetLamp 2,0
	If Bonus = 12 Then SetLamp 10,1 : SetLamp 2,1 : SetLamp 1,0
	If Bonus = 11 Then SetLamp 10,1 : SetLamp 1,1
	BonusText.Text = Bonus
End Sub

Sub targetsR_Hit(Index)
	BRollOff : If Tilt = False Then Playsound"tdown"
	Advance()
End Sub

Sub targetsL_Hit(Index)
	BRollOff : If Tilt = False Then Playsound"tdown"
	Advance()
End Sub

Sub TL1_Hit()
	If Tilt = False Then
		TL1.IsDropped = True : TL1a.IsDropped = True : TLshad1.State = 1 : TimerCheck.Enabled = True
		If TimerScoreHdd.Enabled = False Then TimerScoreHdd.Enabled = True : Else TimerScoreHdd2.Enabled = True
	End If
End Sub

Sub TL2_Hit()
	If Tilt = False Then
		TL2.IsDropped = True : TL2a.IsDropped = True : TLshad2.State = 1 : TimerCheck.Enabled = True
		If TimerScoreHdd.Enabled = False Then TimerScoreHdd.Enabled = True : Else TimerScoreHdd2.Enabled = True
	End If
End Sub

Sub TL3_Hit()
	If Tilt = False Then
		TL3.IsDropped = True : TL3a.IsDropped = True : TLshad3.State = 1 : TimerCheck.Enabled = True
		If TimerScoreHdd.Enabled = False Then TimerScoreHdd.Enabled = True : Else TimerScoreHdd2.Enabled = True
	End If
End Sub

Sub TL4_Hit()
	If Tilt = False Then
		TL4.IsDropped = True : TL4a.IsDropped = True : TLshad4.State = 1 : TimerCheck.Enabled = True
		If TimerScoreHdd.Enabled = False Then TimerScoreHdd.Enabled = True : Else TimerScoreHdd2.Enabled = True
	End If
End Sub

Sub TL5_Hit()
	If Tilt = False Then
		TL5.IsDropped = True : TL5a.IsDropped = True : TLshad5.State = 1 : TimerCheck.Enabled = True
		If TimerScoreHdd.Enabled = False Then TimerScoreHdd.Enabled = True : Else TimerScoreHdd2.Enabled = True
	End If
End Sub

Sub TR1_Hit()
	If Tilt = False Then
		TR1.IsDropped = True : TR1a.IsDropped = True : TRshad1.State = 1 : TimerCheck.Enabled = True
		If TimerScoreHdd.Enabled = False Then TimerScoreHdd.Enabled = True : Else TimerScoreHdd2.Enabled = True
	End If
End Sub

Sub TR2_Hit()
	If Tilt = False Then
		TR2.IsDropped = True : TR2a.IsDropped = True : TRshad2.State = 1 : TimerCheck.Enabled = True
		If TimerScoreHdd.Enabled = False Then TimerScoreHdd.Enabled = True : Else TimerScoreHdd2.Enabled = True
	End If
End Sub

Sub TR3_Hit()
	If Tilt = False Then
		TR3.IsDropped = True : TR3a.IsDropped = True : TRshad3.State = 1 : TimerCheck.Enabled = True
		If TimerScoreHdd.Enabled = False Then TimerScoreHdd.Enabled = True : Else TimerScoreHdd2.Enabled = True
	End If
End Sub

Sub TR4_Hit()
	If Tilt = False Then
		TR4.IsDropped = True : TR4a.IsDropped = True : TRshad4.State = 1 : TimerCheck.Enabled = True
		If TimerScoreHdd.Enabled = False Then TimerScoreHdd.Enabled = True : Else TimerScoreHdd2.Enabled = True
	End If
End Sub

Sub TR5_Hit()
	If Tilt = False Then
		TR5.IsDropped = True : TR5a.IsDropped = True : TRshad5.State = 1 : TimerCheck.Enabled = True
		If TimerScoreHdd.Enabled = False Then TimerScoreHdd.Enabled = True : Else TimerScoreHdd2.Enabled = True
	End If
End Sub

Sub TargetReset()
	For Each x In Targets : x.IsDropped = False : Next
	For Each x In Tshad : x.State = 0 : Next
	Playsound"treset"
	DOF 109, 2
End Sub

Sub BumperReset()
	BumperR1.Force = 9 : BumperR2.Force = 9 : BumperB.Force = 9
	SetLamp 29,0 : SetLamp 26,0 : LightBR1Ref.State = 0 : LightGR1.State = 0
	SetLamp 30,0 : SetLamp 27,0 : LightBR2Ref.State = 0 : LightGR2.State = 0
	SetLamp 31,0 : SetLamp 28,0 : LightBBRef.State = 0 : LightGB.State = 0
	BR1Off.WidthBottom = 60 : BR1Off.WidthTop = 65 : BR1On.WidthBottom = 0 : BR1On.WidthTop = 0 : Abs(LightAniBR1.State -1)
	BR2Off.WidthBottom = 60 : BR2Off.WidthTop = 65 : BR2On.WidthBottom = 0 : BR2On.WidthTop = 0 : Abs(LightAniBR2.State -1)
	BBOff.WidthBottom = 60 : BBOff.WidthTop = 65 : BBOn.WidthBottom = 0 : BBOn.WidthTop = 0 : Abs(LightAniBB.State -1)
End Sub

Sub BumperOff()
	BumperR1.Force = 0 : BumperR2.Force = 0 : BumperB.Force = 0
	SetLamp 29,0 : SetLamp 26,0 : LightBR1Ref.State = 0 : LightGR1.State = 0
	SetLamp 30,0 : SetLamp 27,0 : LightBR2Ref.State = 0 : LightGR2.State = 0
	SetLamp 31,0 : SetLamp 28,0 : LightBBRef.State = 0 : LightGB.State = 0
	BR1Off.WidthBottom = 60 : BR1Off.WidthTop = 65 : BR1On.WidthBottom = 0 : BR1On.WidthTop = 0 : Abs(LightAniBR1.State -1)
	BR2Off.WidthBottom = 60 : BR2Off.WidthTop = 65 : BR2On.WidthBottom = 0 : BR2On.WidthTop = 0 : Abs(LightAniBR2.State -1)
	BBOff.WidthBottom = 60 : BBOff.WidthTop = 65 : BBOn.WidthBottom = 0 : BBOn.WidthTop = 0 : Abs(LightAniBB.State -1)
End Sub

Sub BumperReset2()
	TimerBumperReset2.Enabled = True
End Sub

Sub TimerBumperReset2_Timer
	BumperAttTimer.Enabled = False : BumperTopLightOn() : Blink = 0
	If BOn = True Then BumperBLightOn()
	Me.Enabled = False
End Sub

Sub BumperTopLightOn()
	If GameInProgress = False Or Tilt = True Then Exit Sub
	SetLamp 29,1 : SetLamp 26,1 : LightBR1Ref.State = 1 : LightGR1.State = 1
	SetLamp 30,1 : SetLamp 27,1 : LightBR2Ref.State = 1 : LightGR2.State = 1
	BR1Off.WidthBottom = 0 : BR1Off.WidthTop = 0 : BR1On.WidthBottom = 60 : BR1On.WidthTop = 65 : Abs(LightAniBR1.State -1)
	BR2Off.WidthBottom = 0 : BR2Off.WidthTop = 0 : BR2On.WidthBottom = 60 : BR2On.WidthTop = 65 : Abs(LightAniBR2.State -1)
End Sub

Sub BumperBLightOn()
	If GameInProgress = False Or Tilt = True Then Exit Sub
	BOn = True
	SetLamp 31,1 :  : SetLamp 28,1 : LightBBRef.State = 1 : LightGB.State = 1
	BBOff.WidthBottom = 0 : BBOff.WidthTop = 0 : BBOn.WidthBottom = 60 : BBOn.WidthTop = 65 : Abs(LightAniBB.State -1)
End Sub

Sub LeftInlane_Hit()
	ROWireLI.WidthBottom = 0 : ROWireLI.WidthTop = 0 : LightAniLI.State = Abs(LightAniLI.State -1)
	Me.TimerEnabled = True
	If GameInProgress = False Or Tilt = True Then Exit Sub
	AlternateEBReady
	If LampState (11) = 1 Or LampState (14) = 1 Then SetLamp 11, 0 : SetLamp 14, 0
	Advance()
	TimerCheck.Enabled = True
	TimerScoreHdd.Enabled = True
End Sub

Sub LeftInlane_Timer()
	ROWireLI.WidthBottom = 5 : ROWireLI.WidthTop = 5 : LightAniLI.State = Abs(LightAniLI.State -1)
	Me.TimerEnabled = False	
End Sub

Sub RightInlane_Hit()
	ROWireRI.WidthBottom = 0 : ROWireRI.WidthTop = 0 : LightAniRI.State = Abs(LightAniRI.State -1)
	Me.TimerEnabled = True
	If GameInProgress = False Or Tilt = True Then Exit Sub
	AlternateEBReady
	If LampState (13) = 1 Or LampState (15) = 1 Then SetLamp 13, 0 : SetLamp 15, 0
	Advance()
	TimerCheck.Enabled = True
	TimerScoreHdd.Enabled = True
End Sub

Sub RightInlane_Timer()
	ROWireRI.WidthBottom = 5 : ROWireRI.WidthTop = 5 : LightAniRI.State = Abs(LightAniRI.State -1)
	Me.TimerEnabled = False	
End Sub

Sub LeftOutlane_Hit()
	ROWireLO.WidthBottom = 0 : ROWireLO.WidthTop = 0 : LightAniLO.State = Abs(LightAniLO.State -1)
	Me.TimerEnabled = True
	If GameInProgress = False Or Tilt = True Then Exit Sub
	If LampState (20) = 1 And EB = False Then EB = True : SetLamp 32, 1 : SetLamp 18, 0 : SetLamp 19, 0 : SetLamp 20, 0 : SetLamp 21, 0 : SetLamp 22, 0 : SetLamp 23, 0 : SetLamp 24, 0 : SetLamp 25, 0 : SetB2SData 35, 1
	If LampState (24) = 1 And EB = False Then EB = True : SetLamp 32, 1 : SetLamp 18, 0 : SetLamp 19, 0 : SetLamp 20, 0 : SetLamp 21, 0 : SetLamp 22, 0 : SetLamp 23, 0 : SetLamp 24, 0 : SetLamp 25, 0 : SetB2SData 35, 1
	TimerScoreTsd.Enabled = True
End Sub

Sub LeftOutlane_Timer()
	ROWireLO.WidthBottom = 5 : ROWireLO.WidthTop = 5 : LightAniLO.State = Abs(LightAniLO.State -1)
	Me.TimerEnabled = False	
End Sub

Sub RightOutlane_Hit()
	ROWireRO.WidthBottom = 0 : ROWireRO.WidthTop = 0 : LightAniRO.State = Abs(LightAniRO.State -1)
	Me.TimerEnabled = True
	If GameInProgress = False Or Tilt = True Then Exit Sub
	If LampState (21) = 1 And EB = False Then EB = True : SetLamp 32, 1 : SetLamp 18, 0 : SetLamp 19, 0 : SetLamp 20, 0 : SetLamp 21, 0 : SetLamp 22, 0 : SetLamp 23, 0 : SetLamp 24, 0 : SetLamp 25, 0 : SetB2SData 35, 1
	If LampState (23) = 1 And EB = False Then EB = True : SetLamp 32, 1 : SetLamp 18, 0 : SetLamp 19, 0 : SetLamp 20, 0 : SetLamp 21, 0 : SetLamp 22, 0 : SetLamp 23, 0 : SetLamp 24, 0 : SetLamp 25, 0 : SetB2SData 35, 1
	TimerScoreTsd.Enabled = True
End Sub

Sub RightOutlane_Timer()
	ROWireRO.WidthBottom = 5 : ROWireRO.WidthTop = 5 : LightAniRO.State = Abs(LightAniRO.State -1)
	Me.TimerEnabled = False
End Sub

Sub RightMidLane_Hit()
	ROWireMR.WidthBottom = 0 : ROWireMR.WidthTop = 0 : LightAniMR.State = Abs(LightAniMR.State -1)
	Me.TimerEnabled = True
	If GameInProgress = False Or Tilt = True Then Exit Sub
	AlternateEBReady
	If LampState (17) = 1 Then
		TimerScoreTsd.Enabled = True
	Else
		TimerScoreHdd.Enabled = True
	End If
	TimerCheck.Enabled = True
End Sub

Sub RightMidLane_Timer()
	ROWireMR.WidthBottom = 5 : ROWireMR.WidthTop = 5 : LightAniMR.State = Abs(LightAniMR.State -1)
	Me.TimerEnabled = False
End Sub

Sub LeftMidLane_Hit()
	ROWireML.WidthBottom = 0 : ROWireML.WidthTop = 0 : LightAniML.State = Abs(LightAniML.State -1)
	Me.TimerEnabled = True
	If GameInProgress = False Or Tilt = True Then Exit Sub
	AlternateEBReady
	If LampState (16) = 1 Then
		TimerScoreTsd.Enabled = True
	Else
		TimerScoreHdd.Enabled = True
	End If
	TimerCheck.Enabled = True
End Sub

Sub LeftMidLane_Timer()
	ROWireML.WidthBottom = 5 : ROWireML.WidthTop = 5 : LightAniML.State = Abs(LightAniML.State -1)
	Me.TimerEnabled = False
End Sub

Sub TopA_Hit()
	ROWireTA.WidthBottom = 0 : ROWireTA.WidthTop = 0 : LightAniTA.State = Abs(LightAniTA.State -1)
	Me.TimerEnabled = True
	If GameInProgress = False Or Tilt = True Then Exit Sub
	If LampState (11) = 1 Or LampState (14) = 1 Then SetLamp 11, 0 : SetLamp 14, 0
	If TimerScoreHdd.Enabled = False Then TimerScoreHdd.Enabled = True : Else TimerScoreHdd2.Enabled = True
	If LampState (25) = 1 Then SetLamp 22, 1 : SetLamp 24, 1 : SetLamp 23, 0 : SetLamp 25, 0
	If LampState (24) = 1 Then SetLamp 22, 0 : SetLamp 24, 0 : SetLamp 23, 1 : SetLamp 25, 1
	TimerCheck.Enabled = True	
End Sub

Sub TopB_Hit()
	ROWireTB.WidthBottom = 0 : ROWireTB.WidthTop = 0 : LightAniTB.State = Abs(LightAniTB.State -1)
	Me.TimerEnabled = True
	If GameInProgress = False Or Tilt = True Then Exit Sub
	If LampState (12) = 1 Then SetLamp 12, 0
	BumperBLightOn()
	If TimerScoreHdd.Enabled = False Then TimerScoreHdd.Enabled = True : Else TimerScoreHdd2.Enabled = True
	If LampState (25) = 1 Then SetLamp 22, 1 : SetLamp 24, 1 : SetLamp 23, 0 : SetLamp 25, 0
	If LampState (24) = 1 Then SetLamp 22, 0 : SetLamp 24, 0 : SetLamp 23, 1 : SetLamp 25, 1
	TimerCheck.Enabled = True
End Sub

Sub TopC_Hit()
	ROWireTC.WidthBottom = 0 : ROWireTC.WidthTop = 0 : LightAniTC.State = Abs(LightAniTC.State -1)
	Me.TimerEnabled = True
	If GameInProgress = False Or Tilt = True Then Exit Sub
	If LampState (13) = 1 Or LampState (15) = 1 Then SetLamp 13, 0 : SetLamp 15, 0
	If TimerScoreHdd.Enabled = False Then TimerScoreHdd.Enabled = True : Else TimerScoreHdd2.Enabled = True
	If LampState (25) = 1 Then SetLamp 22, 1 : SetLamp 24, 1 : SetLamp 23, 0 : SetLamp 25, 0
	If LampState (24) = 1 Then SetLamp 22, 0 : SetLamp 24, 0 : SetLamp 23, 1 : SetLamp 25, 1
	TimerCheck.Enabled = True
End Sub

Sub TopA_Timer()
	ROWireTA.WidthBottom = 5 : ROWireTA.WidthTop = 5 : LightAniTA.State = Abs(LightAniTA.State -1)
	Advance()
	Me.TimerEnabled = False
End Sub

Sub TopB_Timer()
	ROWireTB.WidthBottom = 5 : ROWireTB.WidthTop = 5 : LightAniTB.State = Abs(LightAniTB.State -1)
	Advance()
	Me.TimerEnabled = False
End Sub

Sub TopC_Timer()
	ROWireTC.WidthBottom = 5 : ROWireTC.WidthTop = 5 : LightAniTC.State = Abs(LightAniTC.State -1)
	Advance()	
	Me.TimerEnabled = False
End Sub

Sub TriggersBroll_Hit(Index)
	CheckSpeed
	If FinalSpeed < 10 Then BRollOnQ
	If FinalSpeed => 10 Then BRollOnL
	TimerCheck.Enabled = True
End Sub

Sub TimerCheck_Timer
	If Tilt = False Then CheckABCBanks
	Me.Enabled = False
End Sub

Sub CheckABCBanks
	If Tilt = False Then
		If LampState (16) = 0 Then
			If TL1.IsDropped = True And TL2.IsDropped = True And TL3.IsDropped = True And TL4.IsDropped = True And TL5.IsDropped = True Then SetLamp 16, 1
		End If
		If LampState (17) = 0 Then
			If TR1.IsDropped = True And TR2.IsDropped = True And TR3.IsDropped = True And TR4.IsDropped = True And TR5.IsDropped = True Then SetLamp 17, 1
		End If
		If EBReady = True Then
			If LampState (16) = 1 And LampState (17) = 1 Then SetLamp 18,1 : SetLamp 20, 1 : EBReady = False
		End If
		If LampState (11) = 0 And LampState (12) = 0 And LampState (13) = 0 Then DoubleBonus = True : SetLamp 0, 1
		If LampState (16) = 1 And LampState (17) = 1 And LampState (11) = 0 And LampState (12) = 0 And LampState (13) = 0 And SpecialReady = True Then
			SpecialReady = False : If EB = False Then SetLamp 23,1 : SetLamp 25, 1
		End If
	End If
End Sub

Sub AlternateEBReady
	If Tilt = False Then
		If LampState (18) = 1 Then SetLamp 18, 0 : SetLamp 20, 0 : SetLamp 19, 1 : SetLamp 21, 1
		If LampState (19) = 1 Then SetLamp 18, 1 : SetLamp 20, 1 : SetLamp 19, 0 : SetLamp 21, 0

		If LampState (25) = 1 Then SetLamp 22, 1 : SetLamp 24, 1 : SetLamp 23, 0 : SetLamp 25, 0
		If LampState (24) = 1 Then SetLamp 22, 0 : SetLamp 24, 0 : SetLamp 23, 1 : SetLamp 25, 1
	End If
End Sub

Sub BumperR1_Hit() 
	DOF 105, 2
	BRollOff
	If GameInProgress = False Or Tilt = True Then Exit Sub
		RingAni(1) = 0 : For Each x In BRings : x.IsDropped = True : Next
		RingAni(2) = 0 : For Each x In BRings : x.IsDropped = True : Next
		Me.TimerEnabled = True
	AddScore 1000
	Playsound"1000a"
PlaySoundAt "fx_bumpers1", BumperR1
End Sub

Sub BumperR2_Hit()
	DOF 107, 2
	BRollOff
	If GameInProgress = False Or Tilt = True Then Exit Sub
		RingAni(1) = 0 : For Each x In BRings : x.IsDropped = True : Next
		RingAni(2) = 0 : For Each x In BRings : x.IsDropped = True : Next
		BumperR1.TimerEnabled = True
	AddScore 1000
	Playsound"1000b"
PlaySoundAt "fx_bumpers2", BumperR2
End Sub

Sub BumperB_Hit()
DOF 106, 2
PlaySoundAt "fx_bumpers3", BumperB
	BRollOff
	If GameInProgress = False Or Tilt = True Then Exit Sub
		RingAni(2) = 0 : For Each x In BRings : x.IsDropped = True : Next
		Me.TimerEnabled = True
	If LightBotB.State = 1 Then AddScore 1000 : Playsound"1000a" : Else AddScore 100 : Playsound"100a"
End Sub

Sub BumperR1_Timer()
	RingAni(1) = RingAni(1)+1
	Select Case RingAni(1)
		Case 1: BR1Ring3.IsDropped = False : BR2Ring3.IsDropped = False
		Case 2: BR1Ring3.IsDropped = True : BR1Ring2.IsDropped = False : BR2Ring3.IsDropped = True : BR2Ring2.IsDropped = False
		Case 3: BR1Ring2.IsDropped = True : BR1Ring1.IsDropped = False : BR2Ring2.IsDropped = True : BR2Ring1.IsDropped = False
		Case 4: BR1Ring1.IsDropped = True : BR1Ring2.IsDropped = False : BR2Ring1.IsDropped = True : BR2Ring2.IsDropped = False
		Case 5: BR1Ring2.IsDropped = True : BR1Ring3.IsDropped = False : BR2Ring2.IsDropped = True : BR2Ring3.IsDropped = False
		Case 6: BR1Ring3.IsDropped = True : BR2Ring3.IsDropped = True : Me.TimerEnabled = False
	End Select
End Sub

Sub BumperB_Timer()
	RingAni(2) = RingAni(2)+1
	Select Case RingAni(2)
		Case 1: BBRing3.IsDropped = False
		Case 2: BBRing3.IsDropped = True : BBRing2.IsDropped = False
		Case 3: BBRing2.IsDropped = True : BBRing1.IsDropped = False
		Case 4: BBRing1.IsDropped = True : BBRing2.IsDropped = False
		Case 5: BBRing2.IsDropped = True : BBRing3.IsDropped = False
		Case 6: BBRing3.IsDropped = True : Me.TimerEnabled = False
	End Select
End Sub

Sub BumperAttTimer_Timer()
	Blink = Blink + 1
	If Blink = 3 Then Blink = 1
	Select Case Blink
		Case 1: BumperTopLightOn()
				If BOn = True Then BumperBLightOn()
		Case 2: BumperReset()
	End Select
	BumperAttTimer.Interval = 500
'	Me.Enabled = False
End Sub

Sub BumperAttTimerB_Timer()
	Me.Enabled = False
End Sub

Sub KickerL_Hit()
	BRollOff : Stopsound"cuphit" : Playsound"kicker_enter_center"
	mKickL.MagnetOn = 0
	If Tilt = True Then KickerL.TimerInterval = 200 : Exit Sub
	If LampState (18) = 1 And EB = False Then EB = True : SetLamp 32, 1 : SetLamp 18, 0 : SetLamp 19, 0 : SetLamp 20, 0 : SetLamp 21, 0 : SetLamp 22, 0 : SetLamp 23, 0 : SetLamp 24, 0 : SetLamp 25, 0 : SetB2SData 35, 1
	If LampState (22) = 1 And EB = False Then EB = True : SetLamp 32, 1 : SetLamp 18, 0 : SetLamp 19, 0 : SetLamp 20, 0 : SetLamp 21, 0 : SetLamp 22, 0 : SetLamp 23, 0 : SetLamp 24, 0 : SetLamp 25, 0 : SetB2SData 35, 1
	KickerL.TimerInterval = 1100
	TimerScoreTsd.Enabled = True
	Me.TimerEnabled = True
End Sub

Sub KickerL_Timer()
	DOF 108, 2
	KickerL.Kick 147, 10 : LightKickerL.State = 1 : LightKickerL.TimerEnabled = True
	KVar = CInt(Rnd*2)
	Select Case KVar
		Case 0: Playsound"popper"
		Case 1: Playsound"popper"
		Case 2: Playsound"popper"
	End Select
	Playsound"Kick"
	Me.TimerEnabled = False
End Sub

Sub KickerR_Hit()
	BRollOff : Stopsound"cuphit" : Playsound"kicker_enter_center"
	mKickR.MagnetOn = 0
	If Tilt = True Then KickerR.TimerInterval = 200 : Exit Sub
	If LampState (19) = 1 And EB = False Then EB = True : SetLamp 32, 1 : SetLamp 18, 0 : SetLamp 19, 0 : SetLamp 20, 0 : SetLamp 21, 0 : SetLamp 22, 0 : SetLamp 23, 0 : SetLamp 24, 0 : SetLamp 25, 0 : SetB2SData 35, 1
	If LampState (23) = 1 And EB = False Then EB = True : SetLamp 32, 1 : SetLamp 18, 0 : SetLamp 19, 0 : SetLamp 20, 0 : SetLamp 21, 0 : SetLamp 22, 0 : SetLamp 23, 0 : SetLamp 24, 0 : SetLamp 25, 0 : SetB2SData 35, 1
	KickerR.TimerInterval = 1100
	TimerScoreTsd.Enabled = True
	Me.TimerEnabled = True
End Sub

Sub KickerR_Timer()
	DOF 109, 2
	KickerR.Kick 212, 10 : LightKickerR.State = 1 : LightKickerR.TimerEnabled = True
	KVar = CInt(Rnd*2)
	Select Case KVar
		Case 0: Playsound"popper"
		Case 1: Playsound"popper"
		Case 2: Playsound"popper"
	End Select
	Playsound"Kick"
	Me.TimerEnabled = False
End Sub

Sub LightKickerL_Timer
	LightKickerL.State = 0
	Me.TimerEnabled = False
End Sub

Sub LightKickerR_Timer
	LightKickerR.State = 0
	Me.TimerEnabled = False
End Sub

'Kickersimulation
Sub GrabLOn : KickerL.Enabled = False : mKickL.MagnetOn = 1 : End Sub
Sub GrabLOff : KickerL.Enabled = True : mKickL.MagnetOn = 0 : End Sub
Sub GrabROn : KickerR.Enabled = False : mKickR.MagnetOn = 1 : End Sub
Sub GrabROff : KickerR.Enabled = True : mKickR.MagnetOn = 0 : End Sub
Sub MCenterL_Hit : TimerGrabL.Enabled = True : BRollOff : End Sub
Sub MCenterR_Hit : TimerGrabR.Enabled = True : BRollOff : End Sub
Sub TriggerZL_Hit : BRollOff : Playsound"cuphit" : End Sub
Sub TriggerZR_Hit : BRollOff : Playsound"cuphit" : End Sub
Sub TriggerZL_Unhit : GrabLOn : End Sub
Sub TriggerZR_Unhit : GrabROn : End Sub
Sub TimerGrabL_Timer : GrabLOff : Me.Enabled = False : End Sub
Sub TimerGrabR_Timer : GrabROff : Me.Enabled = False : End Sub
'-------------------------------------------------------------

Sub TriggerLaunch_Hit() : Playsound"" : TriggerLaunch2.Enabled = False : End Sub
Sub TriggerLaunch2_Hit() : Playsound"" : End Sub
Sub TriggerGIP_Hit()
	EB = False : EBReady = True : SpecialReady = True : SetLamp 32, 0 : SetB2SData 35, 0 : TriggerLaunch2.Enabled = True
	DummyG.IsDropped = True : DummyG.TimerEnabled = True
End Sub

Sub DummyG_Timer
	DummyG.IsDropped = False
	Me.TimerEnabled = False
End Sub
'sounds------------------------------
Sub BRollOnQ
'	BRollOff
'		BRoll = CInt(Rnd*6)
'		Select Case BRoll
'			Case 0: Playsound"BrollA"
'			Case 1: Playsound"BrollB"
'			Case 2: Playsound"BrollC"
'			Case 3: Playsound"BrollD"
'			Case 4: Playsound"BrollE"
'			Case 5: Playsound"BrollF"
'			Case 6: Playsound"BrollG"
'		End Select
End Sub

Sub BRollOnL
'	BRollOff
'		BRoll = CInt(Rnd*6)
'		Select Case BRoll
'			Case 0: Playsound"BrollAL"
'			Case 1: Playsound"BrollBL"
'			Case 2: Playsound"BrollCL"
'			Case 3: Playsound"BrollDL"
'			Case 4: Playsound"BrollEL"
'			Case 5: Playsound"BrollFL"
'			Case 6: Playsound"BrollGL"
'		End Select
End Sub

Sub BRollOff
'	Stopsound"Brollback"
'	Stopsound"BrollA" : Stopsound"BrollB" : Stopsound"BrollC" : Stopsound"BrollD" : Stopsound"BrollE" : Stopsound"BrollF" : Stopsound"BrollG" : Stopsound"Launch"
'	Stopsound"BrollAL" : Stopsound"BrollBL" : Stopsound"BrollCL" : Stopsound"BrollDL" : Stopsound"BrollEL" : Stopsound"BrollFL" : Stopsound"BrollGL"
End Sub

Sub CollOff
	Stopsound"rub1" : Stopsound"rub2" : Stopsound"rub3" : Stopsound"rub4" : Stopsound"rub5"
	Stopsound"rubstring1" : Stopsound"rubstring2" : Stopsound"rubstring3" : Stopsound"rubstring4" : Stopsound"rubstring5"
	Stopsound"FLCOLL1" : Stopsound"FLCOLL12" : Stopsound"FLCOLL21" : Stopsound"FLCOLL22" : Stopsound"FLCOLL3"
	Stopsound"FLCOLL32" : Stopsound"FLCOLL4" : Stopsound"FLCOLL42" : Stopsound"FLCOLL5" : Stopsound"FLCOLL52"
	Stopsound"wall1" : Stopsound"wall2" : Stopsound"wall3" : Stopsound"wall4" : Stopsound"wall5"
	Stopsound"metal1" : Stopsound"metal2" : Stopsound"metal3" : Stopsound"metal4" : Stopsound"metal5"
	Stopsound"metal21" : Stopsound"metal22" : Stopsound"metal23" : Stopsound"metal24" : Stopsound"metal25"
	Stopsound"pla1" : Stopsound"pla2" : Stopsound"pla3" : Stopsound"pla4" : Stopsound"pla5"
	Stopsound"blech1" : Stopsound"blech2" : Stopsound"blech3" : Stopsound"blech4" : Stopsound"blech5"
End Sub

Sub SRub_Slingshot(Index)
	AddScore 10 : Playsound"10A" : DOF 129, 2
	AlternateEBReady
End Sub

Sub SRub_Hit(Index)
	BRollOff : CollOff
	CheckSpeed
	If FinalSpeed < 3 Then Playsound"rubstring1"
	If FinalSpeed > 2 And FinalSpeed <7 Then Playsound"rubstring2"
	If FinalSpeed > 6 And FinalSpeed <11 Then Playsound"rubstring3"
	If FinalSpeed > 10 And FinalSpeed <15 Then Playsound"rubstring4"
	If FinalSpeed > 14 Then Playsound"rubstring5"
End Sub

Sub walls_Hit(Index)
	BRollOff : CollOff
	CheckSpeed
	If FinalSpeed < 3 Then Playsound"wall1"
	If FinalSpeed > 2 And FinalSpeed <7 Then Playsound"wall2"
	If FinalSpeed > 6 And FinalSpeed <11 Then Playsound"wall3"
	If FinalSpeed > 10 And FinalSpeed <15 Then Playsound"wall4"
	If FinalSpeed > 14 Then Playsound"wall5"
End Sub

Sub rails_Hit(Index)
	BRollOff : CollOff
	CheckSpeed
	If FinalSpeed < 3 Then Playsound"metal21"
	If FinalSpeed > 2 And FinalSpeed <7 Then Playsound"metal22"
	If FinalSpeed > 6 And FinalSpeed <11 Then Playsound"metal23"
	If FinalSpeed > 10 And FinalSpeed <15 Then Playsound"metal24"
	If FinalSpeed > 14 Then Playsound"metal25"
End Sub

Sub metals_Hit(Index)
	BRollOff : CollOff
	CheckSpeed
	If FinalSpeed < 3 Then Playsound"metal1"
	If FinalSpeed > 2 And FinalSpeed <7 Then Playsound"metal2"
	If FinalSpeed > 6 And FinalSpeed <11 Then Playsound"metal3"
	If FinalSpeed > 10 And FinalSpeed <15 Then Playsound"metal4"
	If FinalSpeed > 14 Then Playsound"metal5"
End Sub

Sub posts_Hit(Index)
	BRollOff : CollOff
	CheckSpeed
	If FinalSpeed < 3 Then Playsound"rub1"
	If FinalSpeed > 2 And FinalSpeed <7 Then Playsound"rub2"
	If FinalSpeed > 6 And FinalSpeed <11 Then Playsound"rub3"
	If FinalSpeed > 10 And FinalSpeed <15 Then Playsound"rub4"
	If FinalSpeed > 14 Then Playsound"rub5"
End Sub

Sub plastics_Hit(Index)
	BRollOff : CollOff
	CheckSpeed
	If FinalSpeed < 3 Then Playsound"pla1"
	If FinalSpeed > 2 And FinalSpeed <7 Then Playsound"pla2"
	If FinalSpeed > 6 And FinalSpeed <11 Then Playsound"pla3"
	If FinalSpeed > 10 And FinalSpeed <15 Then Playsound"pla4"
	If FinalSpeed > 14 Then Playsound"pla5"
End Sub

Sub blech_Hit(Index)
	BRollOff : CollOff
	CheckSpeed
	If FinalSpeed < 3 Then Playsound"blech1"
	If FinalSpeed > 2 And FinalSpeed <7 Then Playsound"blech2"
	If FinalSpeed > 6 And FinalSpeed <11 Then Playsound"blech3"
	If FinalSpeed > 10 And FinalSpeed <15 Then Playsound"blech4"
	If FinalSpeed > 14 Then Playsound"blech5"
End Sub

Sub LeftFlipper_Collide(parm)
	BRollOff : CollOff
	CheckSpeed
	If FinalSpeed < 3 Then Playsound"FLCOLL1"
	If FinalSpeed > 2 And FinalSpeed <7 Then Playsound"FLCOLL22"
	If FinalSpeed > 6 And FinalSpeed <11 Then Playsound"FLCOLL3"
	If FinalSpeed > 10 And FinalSpeed <15 Then Playsound"FLCOLL4"
	If FinalSpeed > 14 Then Playsound"FLCOLL5"
End Sub

Sub RightFlipper_Collide(parm)
	BRollOff : CollOff
	CheckSpeed
	If FinalSpeed < 3 Then Playsound"FLCOLL12"
	If FinalSpeed > 2 And FinalSpeed <7 Then Playsound"FLCOLL21"
	If FinalSpeed > 6 And FinalSpeed <11 Then Playsound"FLCOLL32"
	If FinalSpeed > 10 And FinalSpeed <15 Then Playsound"FLCOLL42"
	If FinalSpeed > 14 Then Playsound"FLCOLL52"
End Sub

Sub LeftFlipper1_Collide(parm)
	BRollOff : CollOff
	CheckSpeed
	If FinalSpeed < 3 Then Playsound"FLCOLL1"
	If FinalSpeed > 2 And FinalSpeed <9 Then Playsound"FLCOLL22"
	If FinalSpeed > 6 And FinalSpeed <13 Then Playsound"FLCOLL3"
	If FinalSpeed > 10 And FinalSpeed <17 Then Playsound"FLCOLL4"
	If FinalSpeed > 14 Then Playsound"FLCOLL5"
End Sub

Sub RightFlipper1_Collide(parm)
	BRollOff : CollOff
	CheckSpeed
	If FinalSpeed < 3 Then Playsound"FLCOLL12"
	If FinalSpeed > 2 And FinalSpeed <9 Then Playsound"FLCOLL21"
	If FinalSpeed > 6 And FinalSpeed <13 Then Playsound"FLCOLL32"
	If FinalSpeed > 10 And FinalSpeed <17 Then Playsound"FLCOLL42"
	If FinalSpeed > 14 Then Playsound"FLCOLL52"
End Sub

Sub LeftOut_Hit() : CenterOut.Enabled = False : Stopsound"LostSide" : Playsound"LostSide" : End Sub
Sub RightOut_Hit() : CenterOut.Enabled = False : Stopsound"LostSide" : Playsound"LostSide" : End Sub
Sub CenterOut_Hit() : Stopsound"LostCenter" : Playsound"LostCenter" : End Sub
Sub TriggerROs_Hit(Index) :	Playsound"Switch3" : End Sub
Sub Gate1_Hit() : Playsound"Gate" : Controller.B2SSetBallInPlay 1 : BallInPlay.state=1 : End Sub
Sub Gate_Hit() : Playsound"Gate" : End Sub
Sub DummyG_Hit() : Playsound"GateL" : End Sub
'---------------------------------------------------------------------------------------------------

Sub triggerstop_Hit(Index)
	BumperAttTimer.Enabled = False : Blink = 0
	SetLamp 29,1 : SetLamp 26,1 : LightBR1Ref.State = 1 : LightGR1.State = 1
	BR1Off.WidthBottom = 0 : BR1Off.WidthTop = 0 : BR1On.WidthBottom = 60 : BR1On.WidthTop = 65 : Abs(LightAniBR1.State -1)
	SetLamp 30,1 : SetLamp 27,1 : LightBR2Ref.State = 1 : LightGR2.State = 1
	BR2Off.WidthBottom = 0 : BR2Off.WidthTop = 0 : BR2On.WidthBottom = 60 : BR2On.WidthTop = 65 : Abs(LightAniBR2.State -1)
End Sub

'----------------------------------------
'  JP's Fading Lamps v4.1 VP9 Fading only
'      Based on PD's Fading Lights
' SetLamp 0 is Off
' SetLamp 1 is On
' LampState(x) current state
'----------------------------------------

Dim LampState(33)
TimerFade.Interval = 25

Sub TimerFade_Timer() : UpdateLamps : End Sub

Sub UpdateLamps

	FadeL 0, LightDB, LightDB1
	FadeL 1, LightBonus1, LightBonus11
	FadeL 2, LightBonus2, LightBonus21
	FadeL 3, LightBonus3, LightBonus31
	FadeL 4, LightBonus4, LightBonus41
	FadeL 5, LightBonus5, LightBonus51
	FadeL 6, LightBonus6, LightBonus61
	FadeL 7, LightBonus7, LightBonus71
	FadeL 8, LightBonus8, LightBonus81
	FadeL 9, LightBonus9, LightBonus91
	FadeL 10, LightBonus10, LightBonus101
	FadeL 11, LightTopROA, LightTopROA1
	FadeL 12, LightTopROB, LightTopROB1
	FadeL 13, LightTopROC, LightTopROC1
	FadeL 14, LightBotROA, LightBotROA1
	FadeL 15, LightBotROC, LightBotROC1
	FadeL 16, LightBankL, LightBankL1
	FadeL 17, LightBankR, LightBankR1
	FadeL 18, LightTopEBL, LightTopEBL1
	FadeL 19, LightTopEBR, LightTopEBR1
	FadeL 20, LightBotEBL, LightBotEBL1
	FadeL 21, LightBotEBR, LightBotEBR1
	FadeL 22, LightTopSpecL, LightTopSpecL1
	FadeL 23, LightTopSpecR, LightTopSpecR1
	FadeL 24, LightBotSpecL, LightBotSpecL1
	FadeL 25, LightBotSpecR, LightBotSpecR1
	FadeL 26, LightTopR1, LightTopR11
	FadeL 27, LightTopR2, LightTopR21
	FadeL 28, LightTopB, LightTopB1
	FadeL 29, LightBotR1, LightBotR11
	FadeL 30, LightBotR2, LightBotR21
	FadeL 31, LightBotB, LightBotB1
	FadeL 32, LightSPSA, LightSPSA1
End Sub

Sub SetLamp(nr, value)
    If value = 0 And LampState(nr) = 0 Then Exit Sub
    If value = 1 And LampState(nr) = 1 Then Exit Sub
    LampState(nr) = Abs(value) + 4
End Sub

Sub FadeL(nr, a, b)
    Select Case LampState(nr)
        Case 2:b.state = 0:LampState(nr) = 0
        Case 3:b.state = 1:LampState(nr) = 2
        Case 4:a.state = 0:LampState(nr) = 3
        Case 5:b.state = 1:LampState(nr) = 6
        Case 6:a.state = 1:LampState(nr) = 1
    End Select
End Sub

Sub FadeR(nr, a)
     Select Case LampState(nr)
         Case 2:a.SetValue 3:LampState(nr) = 0
         Case 3:a.SetValue 2:LampState(nr) = 2
         Case 4:a.SetValue 1:LampState(nr) = 3
         Case 5:a.SetValue 1:LampState(nr) = 6
         Case 6:a.SetValue 0:LampState(nr) = 1
     End Select
End Sub

Sub AllLampsOff
Dim g
    For x = 0 to 33
        LampState(x) = 4
    Next
for each g in GILights 
g.state=0 
next
	Bumper1.State = 0 : Bumper2.State = 0 : Bumper3.State = 0 : Bumper4.State = 0
	UpdateLamps
End Sub

Sub AllLampsOn
dim g
	For x = 0 to 33
		LampState(x) = 1
	Next
	UpdateLamps
	Bumper1.State = 1 : Bumper2.State = 1 : Bumper3.State = 1 : Bumper4.State = 1
for each g in GILights 
g.state=1 
next
End Sub

Sub LightReset()
	SetLamp 16,0 : SetLamp 17,0 : SetLamp 18,0 : SetLamp 19,0 : SetLamp 20,0
	SetLamp 21,0 : SetLamp 22,0 : SetLamp 23,0 : SetLamp 24,0 : SetLamp 25,0
	SetLamp 11,1 : SetLamp 12,1 : SetLamp 13,1 : SetLamp 14,1 : SetLamp 15,1
	SetLamp 0,0
End Sub


' ************ Additional Code ***************

'***************************************************************
'             Supporting Ball & Sound Functions v4.0
'  includes random pitch in PlaySoundAt and PlaySoundAtBall
'***************************************************************

Dim TableWidth, TableHeight

TableWidth = table.width
TableHeight = table.height

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table" is the name of the table
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

Sub PlaySoundAt(soundname, tableobj) 'play sound at X and Y position of an object, mostly bumpers, flippers and other fast objects
    PlaySound soundname, 0, 1, Pan(tableobj), 0.1, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname) ' play a sound at the ball position, like rubbers, targets, metals, plastics
    PlaySound soundname, 0, Vol(ActiveBall), pan(ActiveBall), 0.2, Pitch(ActiveBall) * 10, 0, 0, AudioFade(ActiveBall)
End Sub

Function RndNbr(n) 'returns a random number between 1 and n
    Randomize timer
    RndNbr = Int((n * Rnd) + 1)
End Function

'***********************************************
'   JP's VP10 Rolling Sounds + Ballshadow v3.0
'   uses a collection of shadows, aBallShadow
'***********************************************

'***********************************
'   JP's VP10 Rolling Sounds v4.0
'   JP's Ball Shadows
'   JP's Ball Speed Control
'   Rothbauer's dropping sounds
'***********************************

Const tnob = 4   'total number of balls
Const lob = 0     'number of locked balls
Const maxvel = 25 'max ball velocity
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingTimer_timer() 'call this routine from any realtime timer you may have, running at an interval of 10 is good.

    Dim BOT, b, ballpitch, ballvol, speedfactorx, speedfactory
    BOT = GetBalls

    ' stop the sound of deleted balls and hide the shadow
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
        aBallShadow(b).Y = 2100 'under the apron 'may differ from table to table
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table

    ' draw the ball shadow
    For b = lob to UBound(BOT)
        aBallShadow(b).X = BOT(b).X
        aBallShadow(b).Y = BOT(b).Y
        aBallShadow(b).Height = BOT(b).Z - Ballsize / 2 + 1

    'play the rolling sound for each ball
        If BallVel(BOT(b)) > 1 Then
            If BOT(b).z < 30 Then
                ballpitch = Pitch(BOT(b))
                ballvol = Vol(BOT(b))
            Else
                ballpitch = Pitch(BOT(b)) + 25000 'increase the pitch on a ramp
                ballvol = Vol(BOT(b)) * 5
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
        If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_balldrop", 0, ABS(BOT(b).velz) / 17, Pan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If

        ' jps ball speed control
        If BOT(b).VelX AND BOT(b).VelY <> 0 Then
            speedfactorx = ABS(maxvel / BOT(b).VelX)
            speedfactory = ABS(maxvel / BOT(b).VelY)
            If speedfactorx < 1 Then
                BOT(b).VelX = BOT(b).VelX * speedfactorx
                BOT(b).VelY = BOT(b).VelY * speedfactorx
            End If
            If speedfactory < 1 Then
                BOT(b).VelX = BOT(b).VelX * speedfactory
                BOT(b).VelY = BOT(b).VelY * speedfactory
            End If
        End If
    Next
End Sub

' **** Hit Sounds *****
Sub aWireHit_Hit(idx)
Select Case Int(Rnd*3)+1
Case 1: PlaySoundAtBall "WireHit1"
Case 2: PlaySoundAtBall "WireHit2"
Case 3: PlaySoundAtBall "WireHit3"
End Select
End Sub


Sub SetPlayers
If Players=1 Then 
Players1.state=1
Else
Players1.state=0
End If

If Players=2 Then 
Players2.state=1
Else
Players2.state=0
End If

If Players=3 Then 
Players3.state=1
Else
Players3.state=0
End If

If Players=4 Then 
Players4.state=1
Else
Players4.state=0
End If
End Sub

Sub SetPUP
If pu=1 Then 
playerUP1.state=1
Else
playerUP1.state=0
End If

If pu=2 Then 
playerUP2.state=1
Else
playerUP2.state=0
End If

If pu=3 Then 
playerUP3.state=1
Else
playerUP3.state=0
End If

If pu=4 Then 
playerUP4.state=1
Else
playerUP4.state=0
End If
End Sub

Sub table_exit
controller.stop
end sub