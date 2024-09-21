'############ Centigrade 37 by Gottlieb (1977) ############

'               Original Design by:	Allen Edwall
'				Original Art by:	Gordon Morison
'
'		   
'           VPX version by bord
'           Script by Borgdog/Scottacus
'           Based on VP9 Code by Pinuck
'           Menu by Scottacus
'           VR implemented by leojreimroc
'			VR "Bar Room" by Rawd
'			VR Reel Script by Arconovum
'           7.1 sound by Thalamus
'           Arngrim for the DOF 

Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table."
On Error Goto 0

Const cGameName = "centigrade"
Const hsFileName = "centigrade 37 (Gottlieb 1977)"

'  DECLARATIONS
Const FreeLevel1=130000
Const FreeLevel2=170000
Const FreeLevel3=190000

Const SMFadeResetValue = 18
Const maxCredits = 9
Const TiltMax = 3 'nudges till tilt
Const BallSize = 50

Const VolDiv = 2000    ' Lower number, louder ballrolling/collition sound
Const VolCol = 10    ' Ball collition divider ( voldiv/volcol )

' The rest of the values are multipliers
'
'  .5 = lower volume
' 1.5 = higher volume

Const VolBump   = 2    ' Bumpers volume.
Const VolGates  = 1    ' Gates volume.
Const VolMetal  = 1    ' Metals volume.
Const VolRH     = 1    ' Rubber hits volume.
Const VolPo     = 1    ' Rubber posts volume.
Const VolPi     = 1    ' Rubber pins volume.
Const VolTarg   = 1    ' Targets volume.
Const VolKick   = 1    ' Kicker volume.
Const VolSpin   = 1  ' Spinners volume.
Const VolFlip   = 1    ' Flipper volume.

'**********************************************************
'********   	OPTIONS		*******************************
'**********************************************************

VRRoom = 0   '********************************************* Set to 1 to turn on VR Room, Set to 0 to turn off VR Room
VRChooseRoom = 1  '**************************************** Set to 1 for Bar room, Set to 0 for Minimal room

Dim BallShadows: Ballshadows = 1   '*********************** Set to 1 to turn on Ball shadows
Dim FlipperShadows: FlipperShadows = 1   '***************** Set to 1 to turn on Flipper shadows

Dim Score(5), Credits, hisc
Dim BallsRemaining, GameStarted, Match, CurrentBall, SpecialLit, BonusValue
'Dim BallsPerGame
Dim Dropped(4)
Dim DropDone
Dim Drop(4)
Dim x, i, j, k, obj
Dim MazeState
Dim MotorRunning
Dim Temperature
Dim Score100k, Score10k, ScoreK, Score100, Score10, Score1, Scorex	'Define 5 different score values for each reel to use
Dim Free1, Free2, Free3 'free game levels reached
Dim Tilt, tiltsens
Dim Tilted
Dim CurrBall
Dim mhole
Dim BallObj, BallinPlay
Dim LastChime10:Dim LastChime100:Dim LastChime1000:
Dim AllRoRewarded:Dim SpecialRewarded
Dim light, objekt
Dim AttractTick
Dim Chime
Dim Balls
Dim ShowBallShadow
Dim FreePlay
Dim Options
Dim StartedUp
Dim bumpersareon
Dim giison
Dim pfOption
Dim Object
Dim VRChooseRoom
Dim VRRoom




Sub C37_Init
	LoadEM
	Boottable.enabled=1
	DelayTimer.Enabled = 1:DelayTimer.Interval=40

	loadHighScore
	If Score(1) = "" Then Score(1) = 2340
	If highScore(0) = "" Then highScore(0)=5000
	If highScore(1) = "" Then highScore(1)=4500
	If highScore(2) = "" Then highScore(2)=4000
	If highScore(3) = "" Then highScore(3)=3500
	If HighScore(4) = "" Then highScore(4)=3000	
	If match = "" Then match = 4
	If initial(0,1) = "" Then
		initial(0,1) = 19: initial(0,2) = 5: initial(0,3) = 13
		initial(1,1) = 1: initial(1,2) = 1: initial(1,3) = 1
		initial(2,1) = 2: initial(2,2) = 2: initial(2,3) = 2
		initial(3,1) = 3: initial(3,2) = 3: initial(3,3) = 3
		initial(4,1) = 4: initial(4,2) = 4: initial(4,3) = 4
	End If
	If credits = "" Then credits = 0
	If freePlay = "" Then freePlay = 1
	If balls = "" Then balls = 5
	If chime = "" Then chime = 0
	If Temperature = "" Then Temperature = 7
	If ShowDT = True Then pfOption = 1
	If pfOption = "" Then pfOption = 1


	StartedUp = 0

	Matchtxt.text= match
	BIPReel.setValue(0)
	tiltReel.setValue(0)
	GameOverReel.setValue(1)

	Drain.CreateBall

	ShowBallShadow = 1
	UpdatePostIt
	dynamicUpdatePostIt.enabled = 1
	CreditReel.setvalue Credits
	ThermoReel.setvalue Temperature
	if match=0 then
		matchtxt.text="00"
	  else
		matchtxt.text=match*10
	end if
	ScoreReel.setvalue Score(1) MOD 100000
'	If Score>99999 then Reel100k.setvalue Int(Score/100000)
	if score(1)>99999 then 
		p1100k.text="100,000"
	  else
		p1100k.text=" "
	end if
	BallinPlay=0
	MotorRunning=0
	MazeState=0
	MatchPause=0
	CheckLights
	BumpersOff
	ThermRewindTimer.interval = 20	
	ThermRewindTimer.Enabled = 0
	AttractTimer.interval = 75
	AttractTimer.Enabled = 0

	CheckLights

	BGTemperatureFlasher.enabled = 0

	If B2SOn Then
		Controller.B2sSetScorePlayer 1, Score(1)
		Controller.B2SSetData 150, 1
		Controller.B2SSetGameover 4
		Controller.B2SSetCredits Credits
		Controller.B2ssetMatch 34, Match
	End If

	If ShowDT = True Then
		For each objekt in backdropstuff 
		Objekt.visible = 1 	
		Next
	End If
	
	If ShowDt = False Then
		For each objekt in backdropstuff 
		Objekt.visible = 0 	
		Next
	End If

	If VRRoom = 0 Then
		for each Object in VRRoomStuff : object.visible = 0 : next
		ClockTimer.enabled = False
		FanTimer.enabled = False
		TimerTV.enabled = False
		PoolLightTimer1.enabled = False
		PoolLightTimer2.enabled = False
	End If

	If VRRoom = 1 Then
		SetBackglass
		center_objects_em
		VRRoomChoice
		UpdateVRReels 1,1 ,score(0), 0, 0,0,0,0,0
		Lockdown.visible = 0
		Ramp15.visible = 0
		Ramp16.visible = 0
		
		For each objekt in backdropstuff 
		Objekt.visible = 0 	
		Next

		cred =reels(4, 0)
		reels(4, 0) = 0
		SetDrum -1,0,  0
		SetReel 0,-1,  Credits
		reels(4, 0) = Credits	
		If VRChooseRoom = 1 Then
			ClockTimer.enabled = True
			FanTimer.enabled = True	
			TimerTV.enabled = True
			PoolLightTimer1.enabled = True
			PoolLightTimer2.enabled = True
			VRFrontRightLeg.z = 62
			VRLeftFrontLeg.z = 62
		End If
		If VRChooseRoom = 0 Then
			ClockTimer.enabled = False
			FanTimer.enabled = False
			TimerTV.enabled = False
			PoolLightTimer1.enabled = False
			PoolLightTimer2.enabled = False
			VRFrontRightLeg.z = -60
			VRLeftFrontLeg.z = -60
		End If
	End If

	if ShowBallShadow = 1 then
		BallShadowUpdate.enabled=1
	  else
		BallShadowUpdate.enabled=0
	end if

	if flippershadows=1 then 
		FlipperLSh.visible=1
		FlipperRSh.visible=1
	  else
		FlipperLSh.visible=0
		FlipperRSh.visible=0
	end if

	If Balls = 3 Then 
		cards.image = "cards3"
	Else
		cards.image = "cards5"
	End If
End Sub

Sub BootTable_Timer()
		me.interval = 100

		playFieldSound "poweron", 0, Plunger,1
		GION
		If VRRoom = 1 Then
			FlasherMatch
			for each Object in ColFlGameOver : object.visible = 1 : next
			for each Object in ColFlTilt : object.visible = 1 : next
		End If
		dropplate1.visible = 1
		dropplate2.visible = 1
		dropplate3.visible = 1
		dropplate4.visible = 1
	boottable.enabled=0
End Sub

'*****************************************
'			BALL SHADOW
'*****************************************
Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5)

Sub BallShadowUpdate_timer()
    Dim BOT, b
    BOT = GetBalls
    ' hide shadow of deleted balls
    If UBound(BOT)<(tnob-1) Then
        For b = (UBound(BOT) + 1) to (tnob-1)
            BallShadow(b).visible = 0
        Next
    End If
    ' exit the Sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub
    ' render the shadow for each ball
	tb.text = UBound(BOT)
    For b = 0 to UBound(BOT)
        If BOT(b).X < C37.Width/2 Then
            BallShadow(b).X = ((BOT(b).X) + ((BOT(b).X - (C37.Width/2))/12))
        Else
            BallShadow(b).X = ((BOT(b).X) + ((BOT(b).X - (C37.Width/2))/12))
        End If
        ballShadow(b).Y = BOT(b).Y + 10
        If BOT(b).Z > 20 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
    Next
End Sub


Sub StartGame()
	dynamicUpdatePostIt.enabled = 0
	updatePostIt
	AllLampsOff
	ResetDrops
	NextBallPause = 60
	ZeroTherm
	BumpersOn
	MazeState = 1:CheckLights
	AllRoRewarded=0:SpecialRewarded=0
	SetLamp 1,1:SetLamp 2,1:SetLamp 3,1:SetLamp 4,1 'turn on rollover lights
	l9.state=0: lampstate(9)=0: l10.state=0: lampstate(10)=0: l11.state=0: lampstate(11)=0:  l12.state=0: lampstate(12)=0 
	BallsRemaining = Balls ' first ball
	TiltDelay = 0
	Tilted = False
	Tilt = 0
	TurnOn
	GameStarted = 1
	ResetDrops
'	Credits = Credits - 1
	If Credits < 1 Then DOF 138, DOFOff
	CurrBall = 0
	CreditReel.setvalue Credits
	ResetReel.enabled = 1
'	ScoreReel.resettozero
'	Reel100k.setvalue 0
	p1100k.text=" "
	GameOverReel.setValue(0)
	Matchtxt.text=""

	If VRRoom = 1 Then
		for each Object in ColFlTilt : object.visible = 0 : next
		for each Object in ColFlGameOver : object.visible = 0 : next
		for each Object in ColFlMatch : object.visible = 0 : next
		for each Object in ColFl100k : object.visible = 0 : next
		ExtraBallDARK.visible = true
		ExtraBallLIT.visible = false
		WallExtraBallLight.visible = false
	End If

	If B2SOn Then
		Controller.B2SSetCredits Credits
		Controller.B2SSetScoreRollover 25,1
		Controller.B2SSetMatch 34,0
	End If
End Sub

Sub GameOver()
	MatchPause = 20
	GameStarted = 0
	BIPReel.setValue(0)
	GameOverReel.setValue(1)
	BumpersOff
	AllLampsOff	
	StartedUp = 0
	DynamicUpdatePostIt.enabled = 1
	LeftFlipper.RotateToStart:StopSound "PNK_MH_Flip_L_up":DOF 101, DOFOff
	RightFlipper.RotateToStart:StopSound "PNK_MH_Flip_R_up":DOF 102, DOFOff
'	Bumper1.Disabled = 1
'	Bumper2.Disabled = 1
'	Bumper3.Disabled = 1

	If VRRoom = 1 Then
		FlasherMatch
		for each Object in ColFlGameOver : object.visible = 1 : next
		for each Object in ColFlBalls : object.visible = 0 : next
		
	End If

	If B2SOn Then
		Controller.B2SSetGameover 4
	End If
	sortScores
	checkHighScores
End Sub

Sub ResetMachine
	AttractTimer.Enabled = 0
'	GIoff
	AllLampsOff
	RestartDelay=12

	If VRRoom = 1 Then
		for each Object in ColFlGameOver : object.visible = 0 : next
		BGTemperatureFlasher.enabled = 0
	End If

	If B2SOn Then
		Controller.B2SSetScorePlayer 1, 0
		Controller.B2SSetData 6, 0
		Controller.B2SSetData 1, 4
		Controller.B2SSetData 2, 0
		Controller.B2SSetData 3, 0
		Controller.B2SSetData 4, 0
		Controller.B2SSetData 5, 0
		Controller.B2SSetGameover 0
	End If
End Sub

'************Kickers

Function RndNum(min, max)
  RndNum = Int(Rnd() * (max-min + 1) ) + min
End Function

'*** PI returns the value for PI
Function PI()
	PI = 4*Atn(1)
End Function

Sub kickBall(kball, kangle, kvel, kvelz, kzlift)
	dim rangle
	rangle = 3.14 * (kangle - 90) / 180
	kball.z = kball.z + kzlift
	kball.velz = kvelz
	kball.velx = cos(rangle)*kvel
	kball.vely = sin(rangle)*kvel
End Sub

Dim kickerBall, inSaucer, kickstep

Sub kicker1_Hit
	set kickerBall = activeBall	
   	If Tilted = true then
		playFieldSoundVol "kicker_enter_center", 0, kicker1, VolKick
		Pkickarm.rotz=10
		kickstep = 0
		kicker1.timerenabled=1
		exit sub
    else
		playFieldSound "kicker_enter_center", 0, kicker1, VolKick
		ScoreKOH
		Pkickarm.rotz=10
		kickstep = 0
		kickpause=24
	end if
End Sub

Sub kicker1_timer
		Select Case kickStep
        Case 3
			if kicker1.ballcntover > 0 then
				kickBall kickerBall, 120+RndNum(1,4), 8+RndNum(1,4), 5, 30
				playFieldSound SoundFXDOF("popper_ball",119,DOFPulse,DOFContactors), 0, kicker1, VolKick
				DOF 117, DOFPulse
				Pkickarm.rotz=15
			End if
        Case 4:pKickArm.rotz=15
        Case 5:pKickArm.rotz=15
        Case 6:pKickArm.rotz=15
        Case 7:pKickArm.rotz=8
        Case 8:pKickArm.rotz=3
		Case 9:pKickArm.rotz=0:kicker1.timerenabled = 0
    End Select
   kickstep = kickstep + 1
End Sub



Sub ScoreKOH
	dim KOHBonus
	KOHBonus=1000
	If LampState(13)=1 Then KOHBonus=KOHBonus+1000
	If LampState(14)=1 Then KOHBonus=KOHBonus+1000
	If LampState(15)=1 Then KOHBonus=KOHBonus+1000
	If LampState(16)=1 Then KOHBonus=KOHBonus+1000
	SetMotor(KOHBonus)
	If LampState(17)=1 And SpecialRewarded=0 Then 
		GetSpecial
'		SpecialRewarded=1 'PF special
'		SetLamp 17,0
	End If
End Sub

Sub GetSpecial
	If B2Son then 
		DOF 106, DOFPulse 
		Else
		playsound "Knocker"
	End If
	DOF 117, DOFPulse
	AddCredits(1)
	If VRRoom = 1 Then
		cred =reels(4, 0)
		reels(4, 0) = 0
		SetDrum -1,0,  0

		SetReel 0,-1,  Credits
		reels(4, 0) = Credits
	End If
End Sub

Sub AddCredits(x)
	Credits=Credits + x
	DOF 138, DOFOn
	If Credits>maxCredits then Credits=maxCredits
	CreditReel.setvalue Credits
	If VRRoom = 1 Then
		cred =reels(4, 0)
		reels(4, 0) = 0
		SetDrum -1,0,  0

		SetReel 0,-1,  Credits
		reels(4, 0) = Credits
	End If
	If B2SOn Then Controller.B2SSetCredits Credits
End Sub
	
Sub AttractTimer_Timer()
	AttractTick=AttractTick+1
	Select Case AttractTick
		Case 1:
			AllLampsOff:SetLamp 28,1
		Case 2
			SetLamp 25,1:SetLamp 26,1:SetLamp 24,1:SetLamp 27,1
		Case 3
			if lampstate(28)= 0 then l9.state=1:lampstate(9)=1 'SetLamp 9,1
			SetLamp 10,1
			
		Case 4
			SetLamp 5,1	
			SetLamp 28,0
			
		Case 5
			SetLamp 4,1:SetLamp 6,1	
			SetLamp 25,0:SetLamp 26,0:SetLamp 24,0:SetLamp 27,0
			
		Case 6
			SetLamp 7,1:SetLamp 22,1:SetLamp 23,1	
			SetLamp 10,0:l9.state=0:lampstate(9)=0 'SetLamp 9,0:
			
		Case 7
			SetLamp 8,1:
			if lampstate(28)= 0 then l11.state=1: lampstate(11)=1'SetLamp 11,1
			SetLamp 12,1
			SetLamp 5,0
				
		Case 8
			SetLamp 20,1:SetLamp 21,1:SetLamp 31,1
			SetLamp 4,0:SetLamp 6,0
		Case 9
			SetLamp 13,1
			SetLamp 7,0:SetLamp 22,0:SetLamp 23,0
		Case 10
			SetLamp 14,1:SetLamp 29,1:SetLamp 30,1	
			SetLamp 8,0:SetLamp 12,0: l11.state=0: lampstate(11)=0 'SetLamp 11,0:
		Case 11
			SetLamp 15,1:SetLamp 18,1:SetLamp 19,1	
			SetLamp 20,0:SetLamp 21,0:SetLamp 31,0
		Case 12
			SetLamp 16,1	
			SetLamp 13,0
		Case 13
			SetLamp 17,1	
			SetLamp 14,0:SetLamp 29,0:SetLamp 30,0
		Case 14
			SetLamp 1,1:SetLamp 2,1:SetLamp 3,1
			SetLamp 15,0:SetLamp 18,0:SetLamp 19,0
		Case 15 
			SetLamp 16,0	
		Case 16
			SetLamp 17,0
		Case 17
		'	SetLamp 1,0:SetLamp 2,0:SetLamp 3,0
		'rev back
		Case 18 
			SetLamp 17,1:GIon
		Case 19 
			SetLamp 16,1	
		Case 20
			SetLamp 15,1:SetLamp 18,1:SetLamp 19,1
		Case 21
			SetLamp 14,1:SetLamp 29,1:SetLamp 30,1	
		Case 22
			SetLamp 13,1
		Case 23
			SetLamp 20,1:SetLamp 21,1:SetLamp 31,1
		Case 24
			SetLamp 8,1
			if lampstate(28)= 0 then l11.state=1:lampstate(11)=1 'SetLamp 11,1
			SetLamp 12,1
		Case 25
			SetLamp 7,1:SetLamp 22,1:SetLamp 23,1
		Case 26
			SetLamp 4,1:SetLamp 6,1	
		Case 27
			SetLamp 5,1	
		Case 28
			if lampstate(28)= 0 then l9.state=1:lampstate(9)=1 'SetLamp 9,1
			SetLamp 10,1
		Case 29
			SetLamp 25,1:SetLamp 26,1:SetLamp 24,1:SetLamp 27,1
		Case 30
			SetLamp 28,1
		Case 33
			AllLampsOff
		Case 45
			GIOn:For x = 1 to 31:SetLamp x, 1:Next:
		Case 55
			AllLampsOff
		Case 65
			GIOn:For x = 1 to 31:SetLamp x, 1:Next:
		Case 75
			AllLampsOff
		Case 90
			AttractTick=0
	End Select
End Sub



'*******************************************
' KEYS
'******************************************
Dim enableInitialEntry, firstBallOut
Sub C37_KeyDown(ByVal keycode)

	If enableInitialEntry = True Then enterIntitals(keycode)

	If keycode = PlungerKey Then
		Plunger.PullBack
		playFieldSound "plungerpull", 0, Plunger, 1
	End If

	If keycode = PlungerKey Then  
		If VRRoom = 1 Then
			TimerVRPlunger.Enabled = True
			TimerVRPlunger2.Enabled = False
		End If
	End If

	If keycode = StartGameKey Then
        If VRRoom = 1 Then
			Primary_startbutton.y = -32.37037 - 4
        End If
	End If

	If keycode = LeftFlipperKey Then 
		If VRRoom = 1 Then
			Primary_flipper_button_left.X = 2096.804 + 4
		End If
	End If
	
	If keycode = RightFlipperKey Then
		If VRRoom = 1 Then
			Primary_flipper_button_right.X = 2110.841 - 4
		End If
	End If

'.................................
  If Gamestarted = 1 AND NOT Tilted Then
	If keycode = LeftFlipperKey and contball = 0 Then
		lf.fire
		playFieldSound SoundFXDOF("fx_flipperup",101,DOFOn,DOFFlippers), 0, LeftFlipper, VolFlip
		PlaySound "Buzz", -1
	End If

	If keycode = RightFlipperKey and contball = 0 Then
		rf.fire
		playFieldSound SoundFXDOF("fx_flipperup",102,DOFOn,DOFFlippers), 0, RightFlipper, VolFlip
		PlaySound "Buzz1", -1
	End If
    
	If keycode = LeftTiltKey Then
		Nudge 90, 2
		checktilt
	End If
    
	If keycode = RightTiltKey Then
		Nudge 270, 2
		checktilt
	End If
    
	If keycode = CenterTiltKey Then
		Nudge 0, 2
		checktilt
	End If

	If keycode = MechanicalTilt Then
		gametilted
	End If
	
	If Keycode = 30 then SetLamp 17,1

  End If

	If keycode = AddCreditKey Then AddCredits(1):playFieldSound "Coin", 0, Drain, 1

	If keycode = StartGameKey AND GameStarted = 0 And HighScoreDelay.enabled = 0 and enableInitialEntry = False Then 
		If FreePlay = 1 Then ResetMachine
		If FreePlay = 0 and Credits > 0 and StartedUp = 0Then 
			ResetMachine
			StartedUp = 1
			Credits = Credits - 1
			If VRRoom = 1 Then
				cred =reels(4, 0)
				reels(4, 0) = 0
				SetDrum -1,0,  0

				SetReel 0,-1,  Credits
				reels(4, 0) = Credits
			End If
			If B2SOn Then Controller.B2SSetCredits Credits
			CreditReel.setvalue Credits
		End If
	End If

    If keycode = LeftFlipperKey and GameStarted = 0 and OperatorMenu = 0 and enableInitialEntry = False Then
		OperatorMenuTimer.Enabled = true
    End If

	If keycode = LeftFlipperKey and GameStarted = 0 and OperatorMenu = 1 Then
		Options = Options + 1
		If showDt = True Then If options = 3 Then options = 4 'skips non DT options
        If Options = 5 then Options = 0
		OptionMenu.visible = True
        playsound "target"
        Select Case (Options)
            Case 0:
                OptionMenu.image = "FreeCoin" & FreePlay
            Case 1:
                OptionMenu.image = Balls & "Balls" 
            Case 2:
                OptionMenu.image = "Chimes" & Chime
			Case 3:
				optionMenu.image = "UnderCab"
				optionMenu1.visible = 1
				optionMenu1.image = "Sound" & pfOption
				optionMenu2.visible = 1
				optionMenu2.image = "SoundChange"
				Select Case (pfOption)
					Case 1: speaker1.visible = 1: speaker2.visible = 1: speaker3.visible = 0: speaker4.visible = 0
					Case 2: speaker5.visible = 1: speaker6.visible = 1: speaker1.visible = 0: speaker2.visible = 0
					Case 3: speaker1.visible = 1: speaker2.visible = 1: speaker3.visible = 1: speaker4.visible = 1: speaker5.visible = 0: speaker6.visible = 0
				End Select
			Case 4:
				For x = 1 to 6
					EVAL("Speaker" & x).visible = 0
				Next
				optionMenu1.visible = 0
				optionMenu2.visible = 0
				OptionMenu.image = "SaveExit"  
        End Select
    End If

If keycode = RightFlipperKey and GameStarted = 0 and OperatorMenu = 1 Then
      PlaySound "metalhit2"
      Select Case (Options)
		Case 0:
            If FreePlay = 0 Then
                FreePlay = 1
              Else
                FreePlay = 0
            End If
            OptionMenu.image= "FreeCoin" & FreePlay
	'		InstructCard.image = "InstructionCard" & FreePlay
	'		CoinCard.image = "CoinCard" & ReplayEB & Balls 
        Case 1:
            If Balls = 3 Then
                Balls = 5
              Else
                Balls = 3
            End If
            OptionMenu.image = Balls & "Balls" 
			cards.image = "cards" & Balls 
        Case 2:
            If Chime = 0 Then
                Chime= 1
				If B2SOn Then DOF 142,DOFPulse
              Else
                Chime = 0
				PlaySound "SJ_Chime_1000a"
            End If
			OptionMenu.image = "Chimes" & Chime
		Case 3:
			optionMenu1.visible = 1
			pfOption = pfOption + 1
			If pfOption = 4 Then pfOption = 1
			optionMenu1.image = "Sound" & pfOption

			Select Case (pfOption)
				Case 1: speaker1.visible = 1: speaker2.visible = 1: speaker3.visible = 0: speaker4.visible = 0
				Case 2: speaker5.visible = 1: speaker6.visible = 1: speaker1.visible = 0: speaker2.visible = 0
				Case 3: speaker1.visible = 1: speaker2.visible = 1: speaker3.visible = 1: speaker4.visible = 1: speaker5.visible = 0: speaker6.visible = 0
			End Select		
        Case 4:
            OperatorMenu = 0
            saveHighScore
			OptionMenu.image = "FreeCoin" & FreePlay
            OptionMenu.visible = 0
			optionMenu1.visible = 0
			OptionsMenu.visible = 0
		End Select
    End If

    If keycode = 46 Then' C Key
        If contball = 1 Then
            contball = 0
          Else
            contball = 1
        End If
    End If
 
    If keycode = 48 Then 'B Key
        If bcboost = 1 Then
            bcboost = bcboostmulti
          Else
            bcboost = 1
        End If
    End If
 
    If keycode = 203 Then cLeft = 1' Left Arrow
 
    If keycode = 200 Then cUp = 1' Up Arrow
 
    If keycode = 208 Then cDown = 1' Down Arrow
 
    If keycode = 205 Then cRight = 1' Right Arrow

    If keycode = 52 Then Zup = 1' Period

	If keycode = 30 Then 
		ResetDrops
	End If

End Sub

Sub C37_KeyUp(ByVal keycode)

	If keycode = PlungerKey Then
		Plunger.Fire
		playFieldSound "plunger", 0, plunger, 1
	End If

	If keycode = PlungerKey Then  
		If VRRoom = 1 Then
			TimerVRPlunger.Enabled = False
			TimerVRPlunger2.Enabled = True
		End If
	End If

	If keycode = StartGameKey Then
        If VRRoom = 1 Then
			Primary_startbutton.y = -32.37037
        End If
	End If

	If keycode = LeftFlipperKey Then 
		If VRRoom = 1 Then
			Primary_flipper_button_left.X = 2096.804
		End If
	End If
	
	If keycode = RightFlipperKey Then
		If VRRoom = 1 Then
			Primary_flipper_button_right.X = 2110.841
		End If
	End If

	If GameStarted = 1 And Tilted = False Then
		If keycode = LeftFlipperKey Then
			LeftFlipper.RotateToStart
			playFieldSound SoundFXDOF("fx_flipperdown",101,DOFOff,DOFFlippers), 0, LeftFlipper, VolFlip
			StopSound "Buzz"
		End If
    
If keycode = RightFlipperKey Then
		RightFlipper.RotateToStart
		playFieldSound SoundFXDOF("fx_flipperdown",102,DOFOff,DOFFlippers), 0, RightFlipper, VolFlip
		StopSound "Buzz1"
		End If
	End If

    If keycode = LeftFlipperKey Then
        OperatorMenuTimer.Enabled = False
    End If

   If keycode = 203 then Cleft = 0' Left Arrow
 
   If keycode = 200 then Cup = 0' Up Arrow
 
   If keycode = 208 then Cdown = 0' Down Arrow
 
   If keycode = 205 then Cright = 0' Right Arrow

End Sub


Sub FeedBall()
	playFieldSound SoundFXDOF("ballrel",107,DOFPulse,DOFContactors), 0, Pscore0, 1

	BallinPlay=1
	Drain.kick 70,15

'	If VRRoom = 1 Then
'		FlasherBalls
'	End If

	If B2SOn Then
		Controller.B2SSetData 1,0
		Controller.B2SSetData 2,0
		Controller.B2SSetData 3,0
		Controller.B2SSetData 4,0
		Controller.B2SSetData 5,0
		Controller.B2SSetData CurrBall,4
	End If
End Sub

Sub Drain_Hit()
	BumpersOff
	playFieldSound "drain", 0, Drain, 1
	DOF 139, DOFPulse
	If GameStarted = 1 then NextBallPause = 27
	If B2SOn Then Controller.B2SSetTilt 0
	Tilted=False
	tiltReel.setValue(0)
	turnon
End Sub

'*************Ball in Launch Lane
Sub BallHome_hit
	Set ControlBall = ActiveBall   
    contballinplay = True
End Sub

Sub BallHome_unhit

End Sub

Sub Kicker_Hit()
	KickOut
End Sub

'## DROP TARGETS
dim t1step,t2step,t3step,t4step,tupstep

Sub CheckDrop(f)
	Dropped(f)=1
	DropDone=DropDone+1
	PlaySound SoundFX("DTDrop",DOFContactors)
	DOF 111, DOFPulse
	If Tilted=0 then
		SetMotor(500) 'score 500
		If LampState(f+4)=1 Then AddTemperature(1)
	End If
	CheckLights
End Sub

'## STANDUPS

dim step19, step20

Sub standup1a_Hit 'lower standup
  if not tilted then	
	If LampState(9)=0 And LampState(10)=0 Then SetMotor(500)
	If LampState(9)=1 Then SetMotor(5000):SetLamp 28,1:l9.state=0:lampstate(9)=0 'double advance - is it 5000pts too?
	If LampState(10)=1 Then SetMotor(5000): l10.state=0: lampstate(10)=0: ResetDrops 'reset drops
  end if
	playFieldSound SoundFXDOF("target",108,DOFPulse,DOFContactors), 0, standup1a, VolTarg
	layer4target1.transy=-5		
	standup1a.timerenabled = 1
	Step19 = 0
end sub

Sub standup1a_timer
	Select Case Step19
		Case 3:	layer4target1.transy=3
		Case 4: layer4target1.transy=-4
		Case 5: layer4target1.transy=2
		Case 6: layer4target1.transy=-3
		Case 7: layer4target1.transy=1
		Case 8: layer4target1.transy=-2
		Case 9: layer4target1.transy=-1
		Case 10: layer4target1.transy=0: standup1a.timerenabled = 0
	End Select
	Step19=Step19 + 1
End Sub

Sub standup2a_Hit 'upper standup
  if not tilted then	
	If LampState(11)=0 And LampState(12)=0 Then SetMotor(500)
	If LampState(11)=1 Then SetMotor(5000):SetLamp 28,1:l11.state=0: lampstate(11)=0 'double advance - is it 5000pts too?
	If LampState(12)=1 Then SetMotor(5000): l12.state=0: lampstate(12)=0: ResetDrops 'reset drops
  end if
	playFieldSound SoundFXDOF("target",108,DOFPulse,DOFContactors), 0, standup2a, VolTarg
	layer4target2.transy=-5		
	standup2a.timerenabled = 1
	Step20 = 0
End Sub

Sub standup2a_timer
	Select Case Step20
		Case 3:	layer4target2.transy=3
		Case 4: layer4target2.transy=-4
		Case 5: layer4target2.transy=2
		Case 6: layer4target2.transy=-3
		Case 7: layer4target2.transy=1
		Case 8: layer4target2.transy=-2
		Case 9: layer4target2.transy=-1
		Case 10: layer4target2.transy=0: standup2a.timerenabled = 0
	End Select
	Step20=Step20 + 1
End Sub

'## POPS

Dim bump1, bump2, bump3

''bumpers
Sub Bumper1_Hit
	playFieldSound SoundFXDOF("PNK_MH_Pop_L",103,DOFPulse,DOFContactors), 0, Bumper1, VolBump
	  if not tilted then
		AddScore(100)
	  end if
End Sub
'
Sub Bumper2_Hit
	playFieldSound SoundFXDOF("PNK_MH_Pop_R",104,DOFPulse,DOFContactors), 0, Bumper2, VolBump
	  if not tilted then
		AddScore (100)
	  end if
End Sub

Sub Bumper3_Hit
	playFieldSound SoundFXDOF("PNK_MH_Pop_C",105,DOFPulse,DOFContactors), 0, Bumper3, VolBump
	  if not tilted then
		AddScore (1000)
	  end if
End Sub

Sub Wall10_hit
  if not tilted then
	AddScore (10)
  end if
End Sub

Sub Wall26_hit
  if not tilted then
	AddScore (10)
  end if
End Sub

Dim RStep, RRStep, Rrrstep

Sub Wall25_hit
  if not tilted then
	AddScore (10)
  end if
    lrubbersa_prim.Visible = 0
    lrubbersa_prim1.Visible = 1
    wall25timer.Enabled = 1
    RStep = 0
End Sub

Sub Wall25timer_Timer
    Select Case RStep
        Case 3:lrubbersa_prim1.Visible = 0:lrubbersa_prim2.Visible = 1
        Case 4:lrubbersa_prim2.Visible = 0:lrubbersa_prim.Visible = 1:wall25timer.Enabled = 0 
    End Select
    RStep = RStep + 1
End Sub

Sub Wall27_hit
  if not tilted then
	AddScore (10)
  end if
    lrubbers_prim.Visible = 0
    lrubbers_prim3.Visible = 1
    Wall27timer.Enabled = 1
    RRStep = 0
End Sub

Sub Wall27timer_Timer
    Select Case RRStep
        Case 3:lrubbers_prim3.Visible = 0:lrubbers_prim4.Visible = 1
        Case 4:lrubbers_prim4.Visible = 0:lrubbers_prim.Visible = 1:Wall27timer.Enabled = 0 
    End Select
    RRStep = RRStep + 1
End Sub

Sub Wall28_hit
  if not tilted then
	AddScore (10)
  end if
    rrubbersa_prim.Visible = 0
    rrubbersa_prim1.Visible = 1
    Wall28Timer.Enabled = 1
    RrrStep = 0
End Sub

Sub Wall28timer_Timer
    Select Case RrrStep
        Case 3:rrubbersa_prim1.Visible = 0:rrubbersa_prim2.Visible = 1
        Case 4:rrubbersa_prim2.Visible = 0:rrubbersa_prim.Visible = 1:Wall28timer.Enabled = 0 
    End Select
    RrrStep = RrrStep + 1
End Sub

'****************************************
'  DELAY TIMER LOOP
'****************************************
Dim NextBallPause, MatchPause, TiltDelay, RestartDelay, kickpause

Sub DelayTimer_Timer

	If RestartDelay> 0 Then
		RestartDelay = RestartDelay-1
		If RestartDelay=1 Then StartGame
	End If

	If kickpause> 0 Then
		kickpause = kickpause-1
		If kickpause=1 Then kicker1.timerenabled = 1
	End If

	If MatchPause> 0 Then
		MatchPause = MatchPause-1
		If MatchPause=0 Then 
			if match=0 then
				matchtxt.text="00"
				If B2SOn then Controller.B2SSetMatch 100
			  else
				matchtxt.text=match*10
				If B2SOn then Controller.B2SSetMatch 34,Match*10
			end if
			if (match*10)=(score(1) mod 100) then 
				GetSpecial
			end if
		End if
	End If

	If TiltDelay> 0 Then
		TiltDelay = TiltDelay - 1
		If TiltDelay = 60 Then Tilt = Tilt - 1:End If

		If TiltDelay = 30 Then Tilt = Tilt - 1:End If
		If TiltDelay = 1 Then Tilt = Tilt - 1:End If
	End If

	If NextBallPause > 0 Then
		NextBallPause = NextBallPause - 1
		If NextBallPause = 0 Then
			BumpersOn':CheckLights
			CurrBall=CurrBall+1
			BIPReel.setValue(CurrBall)
			If CurrBall > Balls then
				GameOver
			Else
				'new ball
				If VRRoom = 1 Then
					FlasherBalls
					for each Object in ColFlTilt : object.visible = 0 : next
				End If
				SetLamp 28,0
				If currball > 1 Then FeedBall
			End If
		End If
	End If
	
End Sub


'## CUSTOM LAMP ROUTINES

Sub GIon
	If B2SOn then
		Controller.B2SSetScoreRollover 25,1
	End if
	For each Light in GILights:light.state=1:next
	playfield_off.visible=0
	For each obj in layercol1: obj.image = "layer1": Next
	For each obj in layercol2: obj.image = "layer2": Next
	For each obj in layercol3: obj.image = "layer3": Next
	For each obj in layercol4: obj.image = "layer4": Next
	If VRRoom = 1 Then
		For each obj in ColBackglassGI: obj.visible = 1: Next
	End If
	bumperson
	brackets001.image = "brackets"
	brackets002.image = "brackets2"
	ramp16.image = "cent leftrailON"
	ramp15.image = "cent rightrailON"
	metalback.image="metalback"
	rubberbumper.image="rubber"
	giison=true
End Sub


Sub GIoff
	If B2SOn then
		Controller.B2SSetScoreRollover 25,0
	End if
	For each Light in GILights:light.state=0:next
	playfield_off.visible=1
	For each obj in layercol1: obj.image = "layer1_off": Next
	For each obj in layercol2: obj.image = "layer2_off": Next
	For each obj in layercol3: obj.image = "layer3_off": Next
	For each obj in layercol4: obj.image = "layer4_off": Next
	If VRRoom = 1 Then
		For each obj in ColBackglassGI: obj.visible = 0: Next
		For each obj in ColFl100k: obj.visible = 0: Next
		For each obj in ColFlBalls: obj.visible = 0: Next
		BGTemperatureFlasher.enabled = 0
		ExtraBallDARK.visible = True
		ExtraBallLIT.visible = False
		WallExtraBallLight.visible = False
	End If		
	bumpersoff
	brackets001.image = "bracketsoff"
	brackets002.image = "brackets2off"
	ramp16.image = "cent leftrailOff"
	ramp15.image = "cent rightrailOff"
	metalback.image="metalbackoff"
	rubberbumper.image="rubberoff"
	dropplate1.visible = 0
	dropplate2.visible = 0
	dropplate3.visible = 0
	dropplate4.visible = 0
	giison=0
End Sub

Sub ToggleMaze
	Select Case MazeState
		Case 0

		Case 1
			MazeState=2
		Case 2
			MazeState=1
	End Select
	CheckLights
End Sub

Sub FlashAdvance
	For each light in AdvanceLights
		if light.state=1 then light.duration 0, 500, 1
	next
end sub

Sub CheckLights
    Select Case MazeState
		Case 0:'all off
			SetLamp 18,0:SetLamp 21,0:SetLamp 22,0
			SetLamp 19,0:SetLamp 20,0:SetLamp 23,0
			SetLamp 24,0:SetLamp 27,0:SetLamp 25,0:SetLamp 26,0
			SetLamp 10,0:SetLamp 12,0: l11.state=0:lampstate(11)=0:l9.state=0:lampstate(9)=0 'SetLamp 11,0:SetLamp 9,0:
		Case 1:'maze state1
			SetLamp 18,1:SetLamp 21,1:SetLamp 22,1
			SetLamp 19,0:SetLamp 20,0:SetLamp 23,0
			SetLamp 24,1:SetLamp 27,1:SetLamp 25,0:SetLamp 26,0
			If DropDone=4 then
				if lampstate(28)= 0 then l9.state=1:lampstate(9)=1 'SetLamp 9,1
				SetLamp 10,0:SetLamp 12,1: l11.state=0:lampstate(11)=0 'SetLamp 11,0:
			End If
		Case 2:'maze state2
			SetLamp 18,0:SetLamp 21,0:SetLamp 22,0
			SetLamp 19,1:SetLamp 20,1:SetLamp 23,1
			SetLamp 24,0:SetLamp 27,0:SetLamp 25,1:SetLamp 26,1
			If DropDone=4 then
				if lampstate(28)= 0 then l11.state=1:lampstate(11)=1 'SetLamp 11,1
				SetLamp 10,1:SetLamp 12,0:l9.state=0:lampstate(9)=0 'SetLamp 9,0:
			End If
	End Select

End Sub 

Sub BumpersOn()
	SetLamp 29,1:SetLamp 30,1:SetLamp 31,1
	bumpersareon = 1
End Sub

Sub BumpersOff()
	SetLamp 29,0:SetLamp 30,0:SetLamp 31,0
	bumpersareon = 0
End Sub


'****************************************
'  SCORE MOTOR
'****************************************

ScoreMotorTimer.Enabled = 1
ScoreMotorTimer.Interval = 145 '135
Dim MotorMode
Dim MotorPosition

Sub SetMotor(x)
	If MotorRunning<>1 And GameStarted = 1 then
		MotorRunning=1
		BumpersOff
		Select Case x
			Case 500:
				MotorMode=100
				MotorPosition=5
			Case 1000:
				AddScore(1000)
				MotorRunning=0
				BumpersOn
			Case 2000:
				MotorMode=1000
				MotorPosition=2
			Case 3000:
				MotorMode=1000
				MotorPosition=3
			Case 4000:
				MotorMode=1000
				MotorPosition=4		
			Case 5000:
				MotorMode=1000
				MotorPosition=5
		End Select
	End If
End Sub

Sub ScoreMotorTimer_Timer
	If MotorPosition > 0 Then
		Select Case MotorPosition
			Case 5,4,3,2:
				If MotorMode=1000 Then 
					AddScore(1000)
				Else
					AddScore(100):ToggleMaze
				End If
				MotorPosition=MotorPosition-1
			Case 1:
				If MotorMode=1000 Then 
					AddScore(1000)
				Else 
					AddScore(100):ToggleMaze
				End If
				MotorPosition=0:MotorRunning=0:BumpersOn
		End Select
	End If
End Sub

Sub AddScore(x)
  if not Tilted then
'	debugtext.text=score
	Select Case x
		Case 10:
			match=match+1
			if match>9 then match=0
			PlayChime(10)
			Score(1)=Score(1)+10:b2sSetScore: ScoreReel.addvalue 10
		Case 100:
			PlayChime(100)
			Score(1)=Score(1)+100:b2sSetScore: ScoreReel.addvalue 100
		Case 1000:
			PlayChime(1000)
			Score(1)=Score(1)+1000:b2sSetScore: ScoreReel.addvalue 1000
	End Select
	'check for digit rollover and fire second chime
	Scorex = Score(1)
	Score100K=Int (Scorex/100000)'Calculate the value for the 100,000's digit
	Score10K=Int ((Scorex-(Score100k*100000))/10000) 'Calculate the value for the 10,000's digit
	ScoreK=Int((Scorex-(Score100k*100000)-(Score10K*10000))/1000) 'Calculate the value for the 1000's digit
	Score100=Int((Scorex-(Score100k*100000)-(Score10K*10000)-(ScoreK*1000))/100) 'Calculate the value for the 100's digit
	Score10=Int((Scorex-(Score100k*100000)-(Score10K*10000)-(ScoreK*1000)-(Score100*100))/10) 'Calculate the value for the 10's digit
	Select Case x
		Case 10:
			If Score10=0 Then PlayChime(100)
		Case 100:
			If Score100=0 Then PlayChime(1000)
	End Select

	If VRRoom = 1 Then
		EMMODE = 1
		UpdateVRReels 1,1 ,score(1), 0, -1,-1,-1,-1,-1
		EMMODE = 0 ' restore EM mode
	End If

'	If Score100k>0 then Reel100k.setvalue Score100k
	if score(1)>99999 then 
		If VRRoom = 1 Then	
			for each Object in ColFl100k : object.visible = 1 : next
		End If
		p1100k.text="100,000"
	  else
		p1100k.text=" "
	end if
	
	

	If B2SOn Then Controller.B2SSetScorePlayer 1, Score(1)
  end if
End Sub

Sub PlayChime(x)
	Select Case x
		Case 10
			If LastChime10=1 Then
				If Chime = 0 Then
					playSound "SJ_Chime_10a"
				Else
					If B2SOn Then DOF 140,DOFPulse
				End If
				LastChime10=0
			Else
				If Chime = 0 Then
					playSound "SJ_Chime_10b"
				Else
					If B2SOn Then DOF 140,DOFPulse
				End If
				LastChime10=1
			End If
		Case 100
			If LastChime100=1 Then
				If Chime = 0 Then
					playSound "SJ_Chime_100a"
				Else
					If B2SOn Then DOF 141,DOFPulse
				End If
				LastChime100=0
			Else
				If Chime = 0 Then
					playSound "SJ_Chime_100b"
				Else
					If B2SOn Then DOF 141,DOFPulse
				End If
				LastChime100=1
			End If
		Case 1000
			If LastChime1000=1 Then
				If Chime = 0 Then
					playSound "SJ_Chime_1000a"
				Else
					If B2SOn Then DOF 142,DOFPulse
				End If
				LastChime1000=0
			Else
				If Chime = 0 Then
					playSound "SJ_Chime_1000b"
				Else
					If B2SOn Then DOF 142,DOFPulse
				End If
				LastChime1000=1
			End If
	End Select

	If VRRoom = 1 Then
		EMMODE = 1
		UpdateVRReels 1,1 ,score(1), 0, -1,-1,-1,-1,-1
		EMMODE = 0 ' restore EM mode
	End If
End Sub

'****************************************
'  THERMOMETER
'****************************************

Sub ZeroTherm
	ThermRewindTimer.enabled = 1
End Sub

Sub ThermRewindTimer_Timer
	If Temperature > 0 Then
		Temperature=Temperature-1
		ThermoReel.setvalue Temperature
		If VRRoom = 1 Then
			fnMoveLevel Temperature
		End If
		If B2SOn Then TemperaturB2S(Temperature)
	Else
		Me.enabled = 0
	End If
End Sub

Sub AddTemperature(x)
	If GameStarted = 0 Then Exit Sub
	Select Case x
		Case 1
			If LampState(28)=1 Then x=2
		Case 5
			x=5
	End Select
	Temperature=Temperature+x
	If Temperature>=24 Then
		Temperature=24
		'light special
		If SpecialRewarded=0 then SetLamp 17, 1
		If VRRoom = 1 Then
			BGTemperatureFlasher.enabled = 1
			If VRChooseRoom = 1 Then
				ExtraBallDARK.visible = false
				ExtraBallLIT.visible = true
				WallExtraBallLight.visible = true
			End If
		End If
	End If
	ThermoReel.setvalue Temperature
	If VRRoom = 1 Then
		fnMoveLevel Temperature
	End If
	If B2SOn Then TemperaturB2S(Temperature)
End Sub

Sub TemperaturB2S(ttt)
	for x = 150 to 174
		Controller.B2SSetData x,0
	next
	Controller.B2SSetData 150+ttt, 1
End Sub

'****************************************
'  ROLL-OVER SWITCHES
'****************************************

Sub checkRollovers
  if not tilted then
	If GameStarted = 0 Then Exit Sub
	If LampState(1)=0 Then 'A done
		SetLamp 8,1 ' light drop
		SetLamp 16,1 ' light KOH spot
	End If
	If LampState(2)=0 Then 'B done
		SetLamp 7,1 ' light drop
		SetLamp 15,1 ' light KOH spot
	End If
	If LampState(3)=0 Then 'C done
		SetLamp 6,1 ' light drop
		SetLamp 14,1 ' light KOH spot
	End If
	If LampState(4)=0 Then 'D done
		SetLamp 5,1 ' light drop
		SetLamp 13,1 ' light KOH spot
	End If
	If LampState(1)=0 And LampState(2)=0 And LampState(3)=0 And LampState(4)=0 Then
		'only redeem this once
		If AllRoRewarded=0 Then
			AllRoRewarded=1:AddTemperature(5)
		End If
	End If
  end if
End Sub

Sub SwTr1_Hit() 'lane A
	layer2wire014.transz=-10
  if not tilted then
	SetLamp 1,0:checkRollovers
	SetMotor(500)
	DOF 121, DOFPulse
  end if
	playFieldSoundAB "rollover", 0, 1
End Sub

Sub SwTr1_Unhit
	layer2wire014.transz=0
End Sub

Sub SwTr2_Hit() 'lane B
	layer2wire015.transz=-10
  if not tilted then
	SetLamp 2,0:checkRollovers
	SetMotor(500)
	DOF 122, DOFPulse
  end if
	playFieldSoundAB "rollover", 0, 1
End Sub

Sub SwTr2_Unhit
	layer2wire015.transz=0
End Sub

Sub SwTr3_Hit() 'lane C
	layer2wire016.transz=-10
  if not tilted then
	SetLamp 3,0:checkRollovers
	SetMotor(500)
	DOF 123, DOFPulse
  end if
	playFieldSoundAB "rollover", 0, 1
End Sub

Sub SwTr3_Unhit
	layer2wire016.transz=0
End Sub

Sub SwTr4_Hit() 'lane D
	layer2wire007.transz=-10
  if not tilted then
	SetLamp 4,0:checkRollovers
	SetMotor(500)
	DOF 124, DOFPulse
  end if
	playFieldSoundAB "rollover", 0, 1
End Sub

Sub SwTr4_Unhit
	layer2wire007.transz=0
End Sub

Sub SwTr5_Hit() 'maze 1
	layer2wire012.transz=-10
  if not tilted then
	If LampState(18)=1 then SetMotor(5000):AddTemperature(1):FlashAdvance: else SetMotor(500):end if
	DOF 125, DOFPulse
  end if
	playFieldSoundAB "rollover", 0, 1
End Sub

Sub SwTr5_Unhit
	layer2wire012.transz=0
End Sub

Sub SwTr6_Hit() 'maze 2
	layer2wire013.transz=-10
  if not tilted then
	If LampState(19)=1 then SetMotor(5000):AddTemperature(1):FlashAdvance:else SetMotor(500):end if
	DOF 126, DOFPulse
  end if
	playFieldSoundAB "rollover", 0, 1
End Sub

Sub SwTr6_Unhit
	layer2wire013.transz=0
End Sub

Sub SwTr7_Hit() 'maze 3
	layer2wire010.transz=-10
  if not tilted then
	If LampState(20)=1 then SetMotor(5000):AddTemperature(1):FlashAdvance:else SetMotor(500):end if
	DOF 127, DOFPulse
  end if
	playFieldSoundAB "rollover", 0, 1
End Sub

Sub SwTr7_Unhit
	layer2wire010.transz=0
End Sub

Sub SwTr8_Hit() 'maze 4
	layer2wire011.transz=-10
  if not tilted then
	If LampState(21)=1 then SetMotor(5000):AddTemperature(1):FlashAdvance:else SetMotor(500):end if
	DOF 128, DOFPulse
  end if
	playFieldSoundAB "rollover", 0, 1
End Sub

Sub SwTr8_Unhit
	layer2wire011.transz=0
End Sub

Sub SwTr9_Hit() 'maze 5
	layer2wire008.transz=-10
  if not tilted then
	If LampState(22)=1 then SetMotor(5000):AddTemperature(1):FlashAdvance:else SetMotor(500):end if
	DOF 129, DOFPulse
  end if
	playFieldSoundAB "rollover", 0, 1
End Sub

Sub SwTr9_Unhit
	layer2wire008.transz=0
End Sub

Sub SwTr10_Hit() 'maze 6
	layer2wire009.transz=-10
  if not tilted then
	If LampState(23)=1 then SetMotor(5000):AddTemperature(1):FlashAdvance:else SetMotor(500):end if
	DOF 130, DOFPulse
  end if
	playFieldSoundAB "rollover", 0, 1
End Sub

Sub SwTr10_Unhit
	layer2wire009.transz=0
End Sub

Sub SwTr11_Hit() 'left outlane
	layer2wire001.transz=-10
  if not tilted then
	SetMotor(500)
	AddTemperature(1)
	DOF 132, DOFPulse
  end if
	playFieldSoundAB "rollover", 0, 1
End Sub

Sub SwTr11_Unhit
	layer2wire001.transz=0
End Sub

Sub SwTr12_Hit() 'left inlane 1
	layer2wire002.transz=-10
  if not tilted then
	If LampState(24)=1 then SetMotor(5000):AddTemperature(1):FlashAdvance:else SetMotor(500):end if
	DOF 133, DOFPulse
  end if
	playFieldSoundAB "rollover", 0, 1
End Sub

Sub SwTr12_Unhit
	layer2wire002.transz=0
End Sub

Sub SwTr13_Hit() 'left inlane 2
	layer2wire003.transz=-10
  if not tilted then
	If LampState(25)=1 then SetMotor(5000):AddTemperature(1):FlashAdvance:else SetMotor(500):end if
	DOF 134, DOFPulse
  end if
	playFieldSoundAB "rollover", 0, 1
End Sub

Sub SwTr13_Unhit
	layer2wire003.transz=0
End Sub

Sub SwTr14_Hit() 'right inlane 1
	layer2wire004.transz=-10
  if not tilted then
	If LampState(26)=1 then SetMotor(5000):AddTemperature(1):FlashAdvance:else SetMotor(500):end if
	DOF 135, DOFPulse
  end if
	playFieldSoundAB "rollover", 0, 1
End Sub

Sub SwTr14_Unhit
	layer2wire004.transz=0
End Sub

Sub SwTr15_Hit() 'right inlane 2
	layer2wire005.transz=-10
  if not tilted then
	If LampState(27)=1 then SetMotor(5000):AddTemperature(1):FlashAdvance:else SetMotor(500):end if
	DOF 136, DOFPulse
  end if
	playFieldSoundAB "rollover", 0, 1
End Sub

Sub SwTr15_Unhit
	layer2wire005.transz=0
End Sub

Sub SwTr16_Hit() 'right outlane
	layer2wire006.transz=-10
  if not tilted then
	SetMotor(500)
	AddTemperature(1)
	DOF 137, DOFPulse
  end if
	playFieldSoundAB "rollover", 0, 1
End Sub

Sub SwTr16_Unhit
	layer2wire006.transz=0
End Sub

'*****************************************
'  JP's Fadings Lamps 3.0 VP9 Fading only
'        for originals only
'     (based on PD's fading lights)
' SetLamp 0 is Off
' SetLamp 1 is On
' LampState(x) current state
' FadingState(x) current fading state
'*****************************************


Const NumberOfLamps = 33
Redim LampState(NumberOfLamps), FadingState(NumberOfLamps)
dim FadingLevel(200)
 
InitLamps()
LampTimer.Interval = 52  '45 reco
LampTimer.Enabled = 1
 
Sub LampTimer_Timer()
	UpdateLamps
End Sub
 
Sub UpdateLamps()
	FadeL 1, l1, l1 'A Top Rollovers
	FadeL 2, l2, l2 'B
	FadeL 3, l3, l3 'C
	FadeL 4, l4, l4 'D
	FadeL 5, l5, l5 '1 D Drop Target /note reversed mapped 
	FadeL 6, l6, l6 '2 C
	FadeL 7, l7, l7 '3 B
	FadeL 8, l8, l8 '4 A
	FadeL 9, l9, l9 'Lower Standup Pink (DA)
	FadeL 10, l10, l10 'Lower Standup Green (5k/reset)
	FadeL 11, l11, l11 'Upper Standup Pink (DA)
	FadeL 12, l12, l12 'Upper Standup Green (5k/reset)
	FadeL 13, l13, l13 'KOH 1 D
	FadeL 14, l14, l14 'KOH 2 C
	FadeL 15, l15, l15 'KOH 3 B
	FadeL 16, l16, l16 'KOH 4 A
	FadeL 17, l17, l17 'KOH Special
	FadeL 18, l18, l18 'Maze 1
	FadeL 19, l19, l19 'Maze 2
	FadeL 20, l20, l20 'Maze 3
	FadeL 21, l21, l21 'Maze 4
	FadeL 22, l22, l22 'Maze 5
	FadeL 23, l23, l23 'Maze 6
	FadeL 24, l24, l24 'Inlane 1
	FadeL 25, l25, l25 'Inlane 2
	FadeL 26, l26, l26 'Inlane 3
	FadeL 27, l27, l27 'Inlane 4
	FadeL 28, l28, l28 'Double Advance
''	FadeLm 29, bumperlight1, bumperlight1 'Bumper 1
'	FadeLm 30, bumperlight2, bumperlight2 'Bumper 2
''	FadeLm 31, bumperlight3, bumperlight3 'Bumper 3
	FadeLm 29, bumperlightA1, bumperlightA1 'Bumper 1
	FadeLm 29, bumperlightB1, bumperlightB1 'Bumper 1
	FadeLm 30, bumperlightA2, bumperlightA2 'Bumper 2
	FadeLm 30, bumperlightB2, bumperlightB2 'Bumper 2
	FadeLm 31, bumperlightA3, bumperlightA3 'Bumper 3	
	FadeLm 31, bumperlightB3, bumperlightB3 'Bumper 3	
	'GI
'	FadeLm 32, gip1, gip1a 'lane cover lowerleft	FIX
'	FadeLm 32, gip2, gip2a 'lane cover lower right
'	FadeLm 32, gip3, gip3a 'left triangle
'	FadeLm 32, gip4, gip4a 'DT plastic
'	FadeLM 32, gip5, gip5a 'top left plastic
'	FadeLM 32, gip6, gip6a 'top right plastic
'	FadeLM 32, gip7, gip7a 'upper right maze plastic
'	FadeLM 32, gip8, gip8a 'lower right maze plastic
'	FadeLM 32, gip9, gip9a '2 top rollover cover
'	FadeLM 32, gip10, gip10a '3 top rollover cover
'	FadeLM 32, gip11, gip11a 'maze lane cover
'	FadeLM 32, gip12, gip12a '1 top rollover cover
'	FadeL 32, gipf1, gipf1a ' GI playfield lights
	
End Sub


Sub InitLamps():For x = 1 to NumberOfLamps:LampState(x) = 0:FadingState(x) = 4:Next:End Sub 'turn off all the lamps and init arrays

Sub AllLampsOff():For x = 1 to NumberOfLamps:SetLamp x, 0:Next:End Sub

Sub SetLamp(nr, value)
		LampState(nr) = value
		FadingState(nr) = abs(value) + 4
End Sub


'Sub SetBonus(x)
'	Select Case x
'		Case 1:SetLamp 19,1:SetLamp 20,0:SetLamp 21,0:SetLamp 22,0:SetLamp 23,0:SetLamp 24,0
'		Case 2:SetLamp 19,0:SetLamp 20,1:SetLamp 21,0:SetLamp 22,0:SetLamp 23,0:SetLamp 24,0
'		Case 3:SetLamp 19,0:SetLamp 20,0:SetLamp 21,1:SetLamp 22,0:SetLamp 23,0:SetLamp 24,0
'		Case 4:SetLamp 19,0:SetLamp 20,0:SetLamp 21,0:SetLamp 22,1:SetLamp 23,0:SetLamp 24,0
'		Case 5:SetLamp 19,0:SetLamp 20,0:SetLamp 21,0:SetLamp 22,0:SetLamp 23,1:SetLamp 24,0
'		Case 6:SetLamp 19,0:SetLamp 20,0:SetLamp 21,0:SetLamp 22,0:SetLamp 23,0:SetLamp 24,1
'	End Select
'End Sub

Sub FadeW(nr, a, b, c)
	Select Case FadingState(nr)
		Case 2:a.IsDropped = 1:b.IsDropped = 1:c.IsDropped = 1:FadingState(nr) = 0 'Off
		Case 3:b.IsDropped = 1:c.IsDropped = 1:a.IsDropped = 0:FadingState(nr) = 2 'fading 33
		Case 4:a.IsDropped = 1:c.IsDropped = 1:b.IsDropped = 0:FadingState(nr) = 3 'fading 66
		Case 5:b.IsDropped = 1:c.IsDropped = 1:a.IsDropped = 0:FadingState(nr) = 6 'turning ON 33
		Case 6:a.IsDropped = 1:c.IsDropped = 1:b.IsDropped = 0:FadingState(nr) = 7 'turning ON 66
		Case 7:a.IsDropped = 1:b.IsDropped = 1:c.IsDropped = 0:FadingState(nr) = 1 'ON
	End Select
End Sub

'Sub FadeW(nr, a, b, c)
'	Select Case FadingState(nr)
'		Case 2:c.IsDropped = 1:FadingState(nr) = 0                 'Off
'		Case 3:b.IsDropped = 1:c.IsDropped = 0:FadingState(nr) = 2 'fading...
'		Case 4:a.IsDropped = 1:b.IsDropped = 0:FadingState(nr) = 3 'fading...
'		Case 5:b.IsDropped = 1:c.IsDropped = 0:FadingState(nr) = 6 'turning ON
'		Case 6:c.IsDropped = 1:a.IsDropped = 0:FadingState(nr) = 1 'ON
'	End Select
'End Sub

Sub FadeWm(nr, a, b, c)
	Select Case FadingState(nr)
		Case 2:c.IsDropped = 1
		Case 3:b.IsDropped = 1:c.IsDropped = 0
		Case 4:a.IsDropped = 1:b.IsDropped = 0
		Case 5:b.IsDropped = 1:c.IsDropped = 0
	Case 6:c.IsDropped = 1:a.IsDropped = 0
	End Select
End Sub

Sub NFadeW(nr, a)
	Select Case FadingState(nr)
		Case 4:a.IsDropped = 1:FadingState(nr) = 0
		Case 5:a.IsDropped = 0:FadingState(nr) = 1
	End Select
End Sub
 
Sub NFadeWm(nr, a)
	Select Case FadingState(nr)
		Case 4:a.IsDropped = 1
		Case 5:a.IsDropped = 0
	End Select
End Sub
 
Sub NFadeWi(nr, a)
	Select Case FadingState(nr)
		Case 5:a.IsDropped = 1:FadingState(nr) = 0
		Case 4:a.IsDropped = 0:FadingState(nr) = 1
	End Select
End Sub

Sub FadeL(nr, a, b)
	Select Case FadingState(nr)
		Case 2:b.state = 1:b.state = 0:FadingState(nr) = 0 'OFF
		Case 3:b.state = 1:FadingState(nr) = 2 'fading 33
		Case 4:a.state = 1:a.state = 0:FadingState(nr) = 3 'fading 66
		Case 5:b.state = 1:FadingState(nr) = 6 'turning ON 33
		Case 6:a.state = 1:a.state = 0:FadingState(nr) = 7 'turning ON 66
		Case 7:a.state = 1:FadingState(nr) = 1 'ON
	End Select
End Sub

Sub FadeL(nr, a, b)
	Select Case FadingState(nr)
		Case 2:b.state = 0:FadingState(nr) = 0
		Case 3:b.state = 1:FadingState(nr) = 2
		Case 4:a.state = 0:FadingState(nr) = 3
		Case 5:b.state = 1:FadingState(nr) = 6
		Case 6:a.state = 1:FadingState(nr) = 1
	End Select
End Sub
 
Sub FadeLm(nr, a, b)
	Select Case FadingState(nr)
		Case 2:b.state = 0
		Case 3:b.state = 1
		Case 4:a.state = 0
		Case 5:b.state = 1
		Case 6:a.state = 1
	End Select
End Sub
 
Sub NFadeL(nr, a)
	Select Case FadingState(nr)
		Case 4:a.state = 0:FadingState(nr) = 0
		Case 5:a.State = 1:FadingState(nr) = 1
	End Select
End Sub
 
Sub NFadeLm(nr, a)
	Select Case FadingState(nr)
		Case 4:a.state = 0
		Case 5:a.State = 1
	End Select
End Sub
 
Sub FadeR(nr, a)
	Select Case FadingState(nr)
		Case 2:a.SetValue 3:FadingState(nr) = 0
		Case 3:a.SetValue 2:FadingState(nr) = 2
		Case 4:a.SetValue 1:FadingState(nr) = 3
		Case 5:a.SetValue 1:FadingState(nr) = 6
		Case 6:a.SetValue 0:FadingState(nr) = 1
	End Select
End Sub

Sub FadeRm(nr, a)
	Select Case FadingState(nr)
		Case 2:a.SetValue 3
		Case 3:a.SetValue 2
		Case 4:a.SetValue 1
		Case 5:a.SetValue 1
		Case 6:a.SetValue 0
	End Select
End Sub

Sub NFadeT(nr, a, b)
	Select Case FadingState(nr)
		Case 4:a.Text = "":FadingState(nr) = 0
		Case 5:a.Text = b:FadingState(nr) = 1
	End Select
End Sub

Sub NFadeTm(nr, a, b)
	Select Case FadingState(nr)
		Case 4:a.Text = ""
		Case 5:a.Text = b
	End Select
End Sub

Sub NFadeWi(nr, a)
	Select Case FadingState(nr)
		Case 4:a.IsDropped = 0:FadingState(nr) = 0
		Case 5:a.IsDropped = 1:FadingState(nr) = 1
	End Select
End Sub

Sub NFadeWim(nr, a)
	Select Case FadingState(nr)
		Case 4:a.IsDropped = 0
		Case 5:a.IsDropped = 1
	End Select
End Sub

Sub FadeLCo(nr, a, b) 'fading collection of lights
	Dim obj
	Select Case FadingState(nr)
		Case 2:vpmSolToggleObj b, Nothing, 0, 0:FadingState(nr) = 0
		Case 3:vpmSolToggleObj b, Nothing, 0, 1:FadingState(nr) = 2
		Case 4:vpmSolToggleObj a, Nothing, 0, 0:FadingState(nr) = 3
		Case 5:vpmSolToggleObj b, Nothing, 0, 1:FadingState(nr) = 6
		Case 6:vpmSolToggleObj a, Nothing, 0, 1:FadingState(nr) = 1
	End Select
End Sub

Sub FlashL(nr, a, b) ' simple light flash, not controlled by the rom
	Select Case FadingState(nr)
		Case 2:b.state = 0:FadingState(nr) = 0
		Case 3:b.state = 1:FadingState(nr) = 2
		Case 4:a.state = 0:FadingState(nr) = 3
		Case 5:a.state = 1:FadingState(nr) = 4
	End Select
End Sub

Sub MFadeL(nr, a, b, c) 'Light acting as a flash. C is the light number to be restored
	Select Case FadingState(nr)
		Case 2:b.state = 0:FadingState(nr) = 0
			If FadingState(c) = 1 Then SetLamp c, 1
		Case 3:b.state = 1:FadingState(nr) = 2
		Case 4:a.state = 0:FadingState(nr) = 3
		Case 5:a.state = 1:FadingState(nr) = 1
	End Select
End Sub

Sub NFadeB(nr, a, b, c, d, e) 'New Bally Bumpers: a and b are the off state, c and d and on state, no fading. e only for Kiss
	Select Case FadingState(nr)
		Case 4:a.IsDropped = 0:b.IsDropped = 0:c.IsDropped = 1:d.IsDropped = 1:e.IsDropped = 1:FadingState(nr) = 0
		Case 5:a.IsDropped = 1:b.IsDropped = 1:c.IsDropped = 0:d.IsDropped = 0:e.IsDropped = 0:FadingState(nr) = 1
	End Select
End Sub

Sub NFadeBm(nr, a, b, c, d, e)
	Select Case FadingState(nr)
		Case 4:a.IsDropped = 0:b.IsDropped = 0:c.IsDropped = 1:d.IsDropped = 1:e.IsDropped = 1
		Case 5:a.IsDropped = 1:b.IsDropped = 1:c.IsDropped = 0:d.IsDropped = 0:e.IsDropped = 0
	End Select
End Sub

'Lights, Ramps & Primitives used as 4 step fading lights
'a,b,c,d are the images used from on to off

Sub FadeObj(nr, object, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 4:object.image = b:FadingLevel(nr) = 6                   'fading to off...
        Case 5:object.image = a:FadingLevel(nr) = 1                   'ON
        Case 6, 7, 8:FadingLevel(nr) = FadingLevel(nr) + 1             'wait
        Case 9:object.image = c:FadingLevel(nr) = FadingLevel(nr) + 1 'fading...
        Case 10, 11, 12:FadingLevel(nr) = FadingLevel(nr) + 1         'wait
        Case 13:object.image = d:FadingLevel(nr) = 0                  'Off
    End Select
End Sub

Sub FadeObjm(nr, object, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 4:object.image = b
        Case 5:object.image = a
        Case 9:object.image = c
        Case 13:object.image = d
    End Select
End Sub

Sub NFadeObj(nr, object, a, b)
    Select Case FadingLevel(nr)
        Case 4:object.image = b:FadingLevel(nr) = 0 'off
        Case 5:object.image = a:FadingLevel(nr) = 1 'on
    End Select
End Sub

Sub NFadeObjm(nr, object, a, b)
    Select Case FadingLevel(nr)
        Case 4:object.image = b
        Case 5:object.image = a
    End Select
End Sub

'************Reset Reels

'This Sub looks at each individual digit in each players score and sets them in an array RScore.  If the value is >0 and <9
'then the players score is increased by one times the position value of that digit (ie 1 * 1000 for the 1000's digit)
'If the value of the digit is 9 then it subtracts 9 times the postion value of that digit (ie 9*100 for the 100's digit)
'so that the score is not rolled over and the next digit in line gets incremented as well (ie 9 in the 10's positon gets 
'incremented so the 100's position rolls up by one as well since 90 -> 100).  Lastly the RScore array values get incremented
'by one to get ready for the next pass.

Dim rScore(4,5), resetLoop, test, playerTest, resetFlag, reelFlag, reelStop
Sub countUp
	For test = 0 to 4
		rScore(1,Test) = Int(score(1)/10^test) mod 10
	Next

	For x = 1 to 4
		If rScore(1, x) > 0 And rScore(1, x) < 9 Then score(1) = score(1) + 10^x
		If rScore(1, x) = 9 Then score(1) = score(1) - (9 * 10^x)	
		If rScore(1, x) > 0 Then rScore(1, x) = rScore(1, x) + 1
		If rScore(1, x) = 10 Then rScore(1, x) = 0
	Next 

End Sub

'This Sub sets each B2S reel or Desdktop reels to their new values and then plays the score motor sound each time and the
'reel sounds only if the reels are being stepped

Sub updateReels
	If B2SOn and ShowDT = False Then Controller.B2SSetScorePlayer 1, score(1)
	If ShowDT = True Then ScoreReel.setvalue (score(1))
	playFieldSound "scoreMotorSingleFire", 0, l5, 0.2
	If reelStop = 0 Then playsound "reel"
	If reelFlag = 1 Then reelStop = 1

End Sub

'This Timer runs a loop that calls the CountUp and UpdateReels routines to step the reels up five times and Then
'check to see if they are all at zero during a two loop pause and then step them the rest of the way to zero

Dim allZeros, testFlag
Sub resetReel_Timer
	score(1) = (score(1) Mod 100000)
	resetLoop = resetLoop + 1
	If resetLoop = 1 and score(1) = 0 Then
		resetLoop = 0
		If testFlag = 0 Then FeedBall
		testFlag = 0
		allZeros = 1
		resetReel.enabled = 0
		Exit Sub
	End If
	Select Case resetLoop
		Case 1: countUp: updateReels
		Case 2: countUp: updateReels
		Case 3: countUp: updateReels
		Case 4: countUp: updateReels
		Case 5: countUp: updateReels
		Case 6: If reelStop = 1 Then 
					resetLoop = 0
					reelFlag = 0
					reelStop = 0
					If testFlag = 0 Then FeedBall
					testFlag = 0
					resetReel.enabled = 0
					Exit Sub
				End If

		Case 7:
		Case 8: countUp: updateReels
		Case 9: countUp: updateReels
		Case 10: countUp: updateReels
		Case 11: countUp: updateReels
		Case 12: countUp: updateReels: 
			resetLoop = 0
			reelFlag = 0
			reelStop = 0
			If testFlag = 0 Then FeedBall
			testFlag = 0
			resetReel.enabled = 0
			Exit Sub 	
	End Select
	If VRRoom = 1 Then
		EMMODE = 1
		UpdateVRReels 1,1 ,score(1), 0, -1,-1,-1,-1,-1
		EMMODE = 0 ' restore EM mode
	End If

End Sub

'***********************************************
'  END OF JP's Fadings Lamps 3.0 VP9 Fading only
'***********************************************

'*****************
'      Tilt
'*****************

Sub CheckTilt
	If Tilttimer.Enabled = True Then 
	 TiltSens = TiltSens + 1
	 if TiltSens = 3 Then GameTilted
	Else
	 TiltSens = 0
	 Tilttimer.Enabled = True
	End If
End Sub

Sub Tilttimer_Timer()
	Tilttimer.Enabled = False
End Sub

Sub GameTilted
	Tilted = True
	tiltReel.setValue(1)
	If VRRoom = 1 Then
		for each Object in ColFlTilt : object.visible = 1 : next
	End If
    If B2SOn Then Controller.B2SSetTilt 33,1
    If B2SOn Then Controller.B2ssetdata 1, 0
	If balls = 3 Then
		if currball = 3 Then
			GIOFF
			playFieldSound "tilt", 0, plunger, 1
			BumpersOff
		end If
	end if
	If balls = 5 Then
		if currball = 5 Then
			If B2SOn then
				Controller.B2SSetScoreRollover 25,0
			End if
			GIOFF
			BumpersOff
			playFieldSound "tilt", 0, plunger, 1
		end If
	end if
	turnoff
End Sub

sub turnoff
	for i=1 to 3
		EVAL("Bumper"&i).hashitevent = 0
	Next
	if Balls = 3 and currball=3 then
		GIOFF
	end if
	if Balls = 5 and currball=5 Then
		GIOFF
	end if
  	LeftFlipper.RotateToStart
	StopSound "Buzz"
	DOF 101, DOFOff
	RightFlipper.RotateToStart
	StopSound "Buzz1"
	DOF 102, DOFOff
end sub    

sub turnon
	for i=1 to 3
		EVAL("Bumper"&i).hashitevent = 1
	Next
	GION
end sub 

'****************************************
'  B2S Updating 
'**************************************** 

Sub b2sSetScore
'	debugtext.text=score
	Scorex = Score(1)
	Score100K=Int (Scorex/100000)'Calculate the value for the 100,000's digit
	Score10K=Int ((Scorex-(Score100k*100000))/10000) 'Calculate the value for the 10,000's digit
	ScoreK=Int((Scorex-(Score100k*100000)-(Score10K*10000))/1000) 'Calculate the value for the 1000's digit
	Score100=Int((Scorex-(Score100k*100000)-(Score10K*10000)-(ScoreK*1000))/100) 'Calculate the value for the 100's digit
	Score10=Int((Scorex-(Score100k*100000)-(Score10K*10000)-(ScoreK*1000)-(Score100*100))/10) 'Calculate the value for the 10's digit
	Score1=Int(Scorex-(Score100k*100000)-(Score10K*10000)-(ScoreK*1000)-(Score100*100)-(Score10*10)) 'Calculate the value for the 1's digit

	If B2SOn and Score100k=1 Then Controller.B2SSetData 6, 4

	If Score(1) >= FreeLevel1 AND Free1 <1 then
		GetSpecial
		Free1 = 1
	End If
	If Score(1) >= FreeLevel2 AND Free2 <1 then
		GetSpecial
		Free2 = 1
	End If
	If Score(1) >= FreeLevel3 AND Free3 <1 then
		GetSpecial
		Free3 = 1
	End If
End Sub

'***********Operator Menu
Dim OperatorMenu

Sub OperatorMenuTimer_Timer
	Options = 0
    OperatorMenu = 1
	dynamicUpdatePostIt.enabled = 0
	Options = 0
    OptionsMenu.visible = True
    OptionMenu.visible = True
	OptionMenu.image = "FreeCoin" & FreePlay
End Sub

'******* For Ball Control Script
Sub EndControl_Hit()                
    contballinplay = False
End Sub


'************************************************************************
'                         Ball Control
'************************************************************************

Dim Cup, Cdown, Cleft, Cright, Zup, contball, contballinplay, ControlBall, bcboost
Dim bcvel, bcyveloffset, bcboostmulti
 
bcboost = 1 'Do Not Change - default setting
bcvel = 4 'Controls the speed of the ball movement
bcyveloffset = -0.1 'Offsets the force of gravity to keep the ball from drifting vertically on the table, should be negative
bcboostmulti = 3 'Boost multiplier to ball veloctiy (toggled with the B key)

Sub BallControl_Timer()
    If Contball and ContBallInPlay then
        If Cright = 1 Then
            ControlBall.velx = bcvel*bcboost
          ElseIf Cleft = 1 Then
            ControlBall.velx = -bcvel*bcboost
          Else
            ControlBall.velx=0
        End If
        If Cup = 1 Then
            ControlBall.vely = -bcvel*bcboost
          ElseIf Cdown = 1 Then
            ControlBall.vely = bcvel*bcboost
          Else
            ControlBall.vely = bcyveloffset
        End If
        If Zup = 1 Then
            ControlBall.velz = bcvel*bcboost
		Else
			ControlBall.velz = -bcvel*bcboost
        End If
    End If
End Sub

Sub C37_Exit()
	saveHighScore
	If B2SOn Then Controller.stop
End Sub

'**************************************************File Writing Section******************************************************

'*************Load Scores
Sub loadHighScore
	Dim fileObj
	Dim scoreFile
	Dim temp(40)
	Dim textStr

	dim hiInitTemp(3)
	dim hiInit(5)

    Set fileObj = CreateObject("Scripting.FileSystemObject")
	If Not fileObj.FolderExists(UserDirectory) Then 
		Exit Sub
	End If
	If Not fileObj.FileExists(UserDirectory & "Centigrade37.txt") Then
		Exit Sub
	End If
	Set scoreFile = fileObj.GetFile(UserDirectory & "Centigrade37.txt")
	Set textStr = scoreFile.OpenAsTextStream(1,0)
		If (textStr.AtEndOfStream = True) Then
			Exit Sub
		End If

		For x = 1 to 33
			temp(x) = textStr.readLine
		Next
		TextStr.Close	
		For x = 0 to 4
			highScore(x) = cdbl (temp(x+1))
		Next
		For x = 0 to 4
			hiInit(x) = (temp(x + 6))
		Next
		i = 10
		For x = 0 to 4
			For y = 1 to 3
				i = i + 1
				initial(x,y) = cdbl (temp(i))
			Next
		Next
		credits = cdbl (temp(26))
		freePlay = cdbl (temp(27))
		balls = cdbl (temp(28))
		Temperature = cdbl (temp(29))
		match = cdbl (temp(30))
		chime = cdbl (temp(31))
		score(1) = cdbl (temp(32))
		pfOption = cdbl (temp(33))
		Set scoreFile = Nothing
	    Set fileObj = Nothing
End Sub

'************Save Scores
Dim y
Sub saveHighScore
Dim hiInit(5)
Dim hiInitTemp(5)
	For x = 0 to 4
		For y = 1 to 3
			hiInitTemp(y) = chr(initial(x,y) + 64)
		Next
		hiInit(x) = hiInitTemp(1) + hiInitTemp(2) + hiInitTemp(3)
	Next
	Dim fileObj
	Dim scoreFile
	Set fileObj = createObject("Scripting.FileSystemObject")
	If Not fileObj.folderExists(userDirectory) Then 
		Exit Sub
	End If
	Set scoreFile = fileObj.createTextFile(userDirectory & "Centigrade37.txt",True)

		For x = 0 to 4
			scoreFile.writeLine highScore(x)
		Next
		For x = 0 to 4
			scoreFile.writeLine hiInit(x)
		Next
		For x = 0 to 4
			For y = 1 to 3
				scoreFile.writeLine initial(x,y)
			Next
		Next
		scoreFile.WriteLine credits
		scorefile.writeline freePlay
		scoreFile.WriteLine balls
		scoreFile.WriteLine Temperature
		scoreFile.WriteLine match
		scoreFile.WriteLine chime
		scoreFile.WriteLine score(1)
		scoreFile.WriteLine pfOption
		scoreFile.Close
	Set scoreFile = Nothing
	Set fileObj = Nothing

'This section of code writes a file in the User Folder of VisualPinball that contains the High Score data for PinballY.
'PinballY can read this data and display the high scores on the DMD during game selection mode in PinballY.

	Set FileObj = CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) Then 
		Exit Sub
	End If
	Set ScoreFile=FileObj.CreateTextFile(UserDirectory & hsFileName & ".PinballYHighScores",True)

	For x = 0 to 4 
		ScoreFile.WriteLine HighScore(x) 
		ScoreFile.WriteLine HiInit(x)
	Next
	ScoreFile.Close
	Set ScoreFile=Nothing
	Set FileObj=Nothing

End Sub

'************************************************Post It Note Section**************************************************************************
'***************Static Post It Note Update
Dim  hsY, shift, scoreMil, scoreUnit
Dim hsInitial0, hsInitial1, hsInitial2
Dim hsArray: hsArray = Array("HS_0","HS_1","HS_2","HS_3","HS_4","HS_5","HS_6","HS_7","HS_8","HS_9","HS_Space","HS_Comma")
Dim hsiArray: hsIArray = Array("HSi_0","HSi_1","HSi_2","HSi_3","HSi_4","HSi_5","HSi_6","HSi_7","HSi_8","HSi_9","HSi_10","HSi_11","HSi_12","HSi_13","HSi_14","HSi_15","HSi_16","HSi_17","HSi_18","HSi_19","HSi_20","HSi_21","HSi_22","HSi_23","HSi_24","HSi_25","HSi_26")

Sub updatePostIt
	scoreMil = Int(highScore(0)/1000000)
	score100K = Int( (highScore(0) - (scoreMil*1000000) ) / 100000)
	score10K = Int( (highScore(0) - (scoreMil*1000000) - (score100K*100000) ) / 10000)														
	scoreK = Int( (highScore(0) - (scoreMil*1000000) - (score100K*100000) - (score10K*10000) ) / 1000)										
	score100 = Int( (highScore(0) - (scoreMil*1000000) - (score100K*100000) - (score10K*10000) - (scoreK*1000) ) / 100)						
	score10 = Int( (highScore(0) - (scoreMil*1000000) - (score100K*100000) - (score10K*10000) - (scoreK*1000) - (score100*100) ) / 10)			
	scoreUnit = (highScore(0) - (scoreMil*1000000) - (score100K*100000) - (score10K*10000) - (scoreK*1000) - (score100*100) - (score10*10) ) 

	pScore6.image = hsArray(scoreMil):If highScore(0) < 1000000 Then pScore6.image = hsArray(10)
	pScore5.image = hsArray(score100K):If highScore(0) < 100000 Then pScore5.image = hsArray(10)
	pScore4.image = hsArray(score10K):If highScore(0) < 10000 Then pScore4.image = hsArray(10)
	pScore3.image = hsArray(scoreK):If highScore(0) < 1000 Then pScore3.image = hsArray(10)
	pScore2.image = hsArray(score100):If highScore(0) < 100 Then pScore2.image = hsArray(10)
	pScore1.image = hsArray(score10):If highScore(0) < 10 Then pScore1.image = hsArray(10)
	pScore0.image = hsArray(scoreUnit):If highScore(0) < 1 Then pScore0.image = hsArray(10)
	If highScore(0) < 1000 Then
		PComma.image = hsArray(10)
	Else
		pComma.image = hsArray(11)
	End If
	If highScore(0) < 1000000 Then
		pComma1.image = hsArray(10)
	Else
		pComma1.image = hsArray(11)
	End If
	If highScore(0) > 999999 Then shift = 0 :pComma.transx = 0
	If highScore(0) < 1000000 Then shift = 1:pComma.transx = -10
	If highScore(0) < 100000 Then shift = 2:pComma.transx = -20
	If highScore(0) < 10000 Then shift = 3:pComma.transx = -30
	For hsY = 0 to 6
		EVAL("Pscore" & hsY).transx = (-10 * shift)
	Next
	initial1.image = hsIArray(initial(0,1))
	initial2.image = hsIArray(initial(0,2))
	initial3.image = hsIArray(initial(0,3))
End Sub

'***************Show Current Score
Sub showScore
	scoreMil = Int(highScore(activeScore(flag))/1000000)
	score100K = Int( (highScore(activeScore(flag)) - (scoreMil*1000000) ) / 100000)
	score10K = Int( (highScore(activeScore(flag)) - (scoreMil*1000000) - (score100K*100000) ) / 10000)														
	scoreK = Int( (highScore(activeScore(flag)) - (scoreMil*1000000) - (score100K*100000) - (score10K*10000) ) / 1000)										
	score100 = Int( (highScore(activeScore(flag)) - (scoreMil*1000000) - (score100K*100000) - (score10K*10000) - (scoreK*1000) ) / 100)						
	score10 = Int( (highScore(activeScore(flag)) - (scoreMil*1000000) - (score100K*100000) - (score10K*10000) - (scoreK*1000) - (score100*100) ) / 10)			
	scoreUnit = (highScore(activeScore(flag)) - (scoreMil*1000000) - (score100K*100000) - (score10K*10000) - (scoreK*1000) - (score100*100) - (score10*10) ) 

	pScore6.image = hsArray(scoreMil):If highScore(activeScore(flag)) < 1000000 Then pScore6.image = hsArray(10)
	pScore5.image = hsArray(score100K):If highScore(activeScore(flag)) < 100000 Then pScore5.image = hsArray(10)
	pScore4.image = hsArray(score10K):If highScore(activeScore(flag)) < 10000 Then pScore4.image = hsArray(10)
	pScore3.image = hsArray(scoreK):If highScore(activeScore(flag)) < 1000 Then pScore3.image = hsArray(10)
	pScore2.image = hsArray(score100):If highScore(activeScore(flag)) < 100 Then pScore2.image = hsArray(10)
	pScore1.image = hsArray(score10):If highScore(activeScore(flag)) < 10 Then pScore1.image = hsArray(10)
	pScore0.image = hsArray(scoreUnit):If highScore(activeScore(flag)) < 1 Then pScore0.image = hsArray(10)
	If highScore(activeScore(flag)) < 1000 Then
		pComma.image = hsArray(10)
	Else
		pComma.image = hsArray(11)
	End If
	If highScore(activeScore(flag)) < 1000000 Then
		pComma1.image = hsArray(10)
	Else
		pComma1.image = hsArray(11)
	End If
	If highScore(flag) > 999999 Then shift = 0 :pComma.transx = 0
	If highScore(activeScore(flag)) < 1000000 Then shift = 1:pComma.transx = -10
	If highScore(activeScore(flag)) < 100000 Then shift = 2:pComma.transx = -20
	If highScore(activeScore(flag)) < 10000 Then shift = 3:pComma.transx = -30
	For HSy = 0 to 6
		EVAL("Pscore" & hsY).transx = (-10 * shift)
	Next
	initial1.image = hsIArray(initial(activeScore(flag),1))
	initial2.image = hsIArray(initial(activeScore(flag),2))
	initial3.image = hsIArray(initial(activeScore(flag),3))
End Sub

'***************Dynamic Post It Note Update
Dim scoreUpdate, dHSx
Sub dynamicUpdatePostIt_Timer
	scoreMil = Int(highScore(scoreUpdate)/1000000)
	score100K = Int( (highScore(ScoreUpdate) - (scoreMil*1000000) ) / 100000)
	score10K = Int( (highScore(scoreUpdate) - (ScoreMil*1000000) - (Score100K*100000) ) / 10000)														
	scoreK = Int( (highScore(scoreUpdate) - (scoreMil*1000000) - (score100K*100000) - (score10K*10000) ) / 1000)										
	score100 = Int( (highScore(ScoreUpdate) - (scoreMil*1000000) - (score100K*100000) - (score10K*10000) - (scoreK*1000) ) / 100)						
	score10 = Int( (highScore(ScoreUpdate) - (scoreMil*1000000) - (score100K*100000) - (score10K*10000) - (scoreK*1000) - (score100*100) ) / 10)			
	scoreUnit = (highScore(ScoreUpdate) - (scoreMil*1000000) - (score100K*100000) - (score10K*10000) - (scoreK*1000) - (score100*100) - (score10*10) ) 

	pScore6.image = hsArray(ScoreMil):If highScore(scoreUpdate) < 1000000 Then pScore6.image = hsArray(10)
	pScore5.image = hsArray(Score100K):If highScore(scoreUpdate) < 100000 Then pScore5.image = hsArray(10)
	pScore4.image = hsArray(Score10K):If highScore(scoreUpdate) < 10000 Then pScore4.image = hsArray(10)
	pScore3.image = hsArray(ScoreK):If highScore(scoreUpdate) < 1000 Then pScore3.image = hsArray(10)
	pScore2.image = hsArray(Score100):If highScore(scoreUpdate) < 100 Then pScore2.image = hsArray(10)
	pScore1.image = hsArray(Score10):If highScore(scoreUpdate) < 10 Then pScore1.image = hsArray(10)
	pScore0.image = hsArray(ScoreUnit):If highScore(scoreUpdate) < 1 Then pScore0.image = hsArray(10)
	If highScore(scoreUpdate) < 1000 Then
		pComma.image = hsArray(10)
	Else
		pComma.image = hsArray(11)
	End If
	If highScore(scoreUpdate) < 1000000 Then
		pComma1.image = hsArray(10)
	Else
		pComma1.image = hsArray(11)
	End If
	If highScore(scoreUpdate) > 999999 Then shift = 0 :pComma.transx = 0
	If highScore(scoreUpdate) < 1000000 Then shift = 1:pComma.transx = -10
	If highScore(scoreUpdate) < 100000 Then shift = 2:pComma.transx = -20
	If highScore(scoreUpdate) < 10000 Then shift = 3:pComma.transx = -30
	For dHSx = 0 to 6
		EVAL("Pscore" & dHSx).transx = (-10 * shift)
	Next
	initial1.image = hsIArray(initial(scoreUpdate,1))
	initial2.image = hsIArray(initial(scoreUpdate,2))
	initial3.image = hsIArray(initial(scoreUpdate,3))
	scoreUpdate = scoreUpdate + 1
	If scoreUpdate = 5 then scoreUpdate = 0
End Sub

'***************Bubble Sort
Dim tempScore(2), tempPos(3), position(5)
Dim bSx, bSy
'Scores are sorted high to low with Position being the player's number
Sub sortScores
	For bSx = 1 to 4
		position(bSx) = bSx
	Next
	For bSx = 1 to 4
		For bSy = 1 to 3
			If score(bSy) < score(bSy+1) Then	
				tempScore(1) = score(bSy+1)
				tempPos(1) = position(bSy+1)
				score(bSy+1) = score(bSy)
				score(bSy) = tempScore(1)
				position(bSy+1) = position(BSy)
				position(bSy) = tempPos(1)
			End If
		Next
	Next
End Sub

'*************Check for High Scores

Dim highScore(5), activeScore(5), hs, chX, chY, chZ, chIX, tempI(4), tempI2(4), flag, hsI, hsX
'goes through the 5 high scores one at a time and compares them to the player's scores high to 
'if a player's score is higher it marks that postion with ActiveScore(x) and moves all of the other
'	high scores down by one along with the high score's player initials
'	also clears the new high score's initials for entry later
Sub checkHighScores
	flag = 0
	For hs = 1 to 4     							'look at all 5 saved high scores
		For chY = 0 to 4   					    	'look at 4 player scores
			If score(hs) > highScore(chY) Then
				flag = flag + 1						'flag to show how many high scores needs replacing 
				tempScore(1) = highScore(chY)
				highScore(chY) = score(hs)
				activeScore(hs) = chY				'ActiveScore(x) is the high score being modified with x=1 the largest and x=4 the smallest
				For chIX = 1 to 3					'set initals to blank and make temporary initials = to intials being modifed so they can move down one high score
					tempI(chIX) = initial(chY,chIX)
					initial(chY,chIX) = 0
				Next
				
				If chY < 4 Then						'check if not on lowest high score for overflow error prevention
					For chZ = chY+1 to 4			'set as high score one more than score being modifed (CHy+1)
						tempScore(2) = highScore(chZ)	'set a temporaray high score for the high score one higher than the one being modified 
						highScore(chZ) = tempScore(1)	'set this score to the one being moved
						tempScore(1) = tempScore(2)		'reassign TempScore(1) to the next higher high score for the next go around
						For chIX = 1 to 3
							tempI2(chIX) = initial(chZ,chIX)	'make a new set of temporary initials
						Next
						For chIX = 1 to 3
							initial(chZ,chIX) = tempI(chIX)		'set the initials to the set being moved
							tempI(chIX) = tempI2(chIX)			'reassign the initials for the next go around
						Next
					Next
				End If
				chY = 4								'if this loop was accessed set CHy to 4 to get out of the loop
			End If
		Next
	Next
'	Goto Initial Entry
		hsI = 1			'go to the first initial for entry
		hsX = 1			'make the displayed inital be "A"
		If flag > 0 Then	'Flag 0 when all scores are updated so leave subroutine and reset variables
			showScore
			playerEntry.visible = 1
			playerEntry.image = "Player" & position(Flag)
			initial(activeScore(flag),1) = 1	'make first inital "A"
			For chY = 2 to 3
				initial(activeScore(flag),chY) = 0	'set other two to " "
			Next
			For chY = 1 to 3
				EVAL("Initial" & chY).image = hsIArray(initial(activeScore(flag),chY))		'display the initals on the tape
			Next
			initialTimer1.enabled = 1		'flash the first initial
			dynamicUpdatePostIt.enabled = 0		'stop the scrolling intials timer
			If B2Son then 
				DOF 106, DOFPulse 
			Else
				playsound "Knocker"
			End If
			enableInitialEntry = True
		End If
End Sub


'************Enter Initials Keycode Subroutine
Dim initial(6,5), players
Sub enterIntitals(keycode)
		If keyCode = leftFlipperKey Then 
			hsX = hsX - 1						'HSx is the inital to be displayed A-Z plus " "
			If hsX < 0 Then hsX = 26
			If hsI < 4 Then EVAL("Initial" & hsI).image = hsIArray(hsX)		'HSi is which of the three intials is being modified
			playSound "metalhit_thin"
		End If
		If keycode = rightFlipperKey Then
			hsX = hsX + 1
			If hsX > 26 Then hsX = 0
			If hsI < 4 Then EVAL("Initial"& hsI).image = hsIArray(hsX)
			playSound "metalhit_thin"
		End If
		If keycode = startGameKey Then
			If hsI < 3 Then									'if not on the last initial move on to the next intial
				EVAL("Initial" & hsI).image = hsIArray(hsX)	'display the initial
				initial(activeScore(flag), hsI) = hsX		'save the inital
				playSound "metalhit_medium"
				EVAL("InitialTimer" & hsI).enabled = 0		'turn that inital's timer off
				EVAL("Initial" & hsI).visible = 1			'make the initial not flash but be turn on
				initial(activeScore(flag),hsI + 1) = hsX	'move to the next initial and make it the same as the last initial
				EVAL("Initial" & hsI +1).image = hsIArray(hsX)	'display this intial
'				y = 1
				EVAL("InitialTimer" & hsI + 1).enabled = 1	'make the new intial flash
				hsI = hsI + 1								'increment HSi
			Else
				enableInitialEntry = False
				highScoreDelay.enabled = 1										'if on the last initial then get ready to exit the subroutine 
				initial3.visible = 1					'make the intial visible
				playSound "metalhit_medium"
				initialTimer3.enabled = 0				'shut off the flashing
				initial(activeScore(flag),3) = hsX		'set last initial
				initialEntry							'exit subroutine
			End If
		End If
End Sub

'************Update Initials and see if more scores need to be updated
Dim eIX
Sub initialEntry
	playsound "10Pts"
	hsI = 1
					'exit high score entry mode and reset variables
		players = 0
		For eIX = 1 to 4
			activeScore(eIX) = 0
			position(eIX) = 0
		Next
		playerEntry.visible = 0
		scoreUpdate = 0						'go to the highest score
		updatePostIt						'display that score
		highScoreDelay.enabled = 1
End Sub

'************Delay to prevent start button push for last initial from starting game Update
Dim hsCount
Sub highScoreDelay_timer
	hsCount = hsCount + 1
	If hsCount > 1 Then
		saveHighScore
		dynamicUpdatePostIt.enabled = 1		'turn scrolling high score back on
		hsCount = 0
		highScoreDelay.enabled = 0
	End If
End Sub
'************Flash Initials Timers
Sub initialTimer1_Timer
	y = y + 1
	If y > 1 Then y = 0
	If y = 0 Then 
		initial1.visible = 1
	Else
		initial1.visible = 0	
	End If
End Sub

Sub initialTimer2_Timer
	y = y + 1
	If y > 1 Then y = 0
	If y = 0 Then 
		initial2.visible = 1
	Else
		initial2.visible = 0	
	End If
End Sub

Sub initialTimer3_Timer
	y = y + 1
	If y > 1 Then y = 0
	If y = 0 Then 
		initial3.visible = 1
	Else
		initial3.visible = 0	
	End If
End Sub

' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Function AudioPan(TableObj)	'Calculates the pan for a TableObj based on the X position on the table. "C37" is the name of the table.  New AudioPan algorithm for accurate stereo pan positioning.
    Dim tmp
    If PFOption=1 Then tmp = TableObj.x * 2 / C37.width-1
	If PFOption=2 Then tmp = TableObj.y * 2 / C37.height-1
	If tmp < 0 Then
		AudioPan = -((0.8745898957*(ABS(tmp)^12.78313661)) + (0.1264569796*(ABS(tmp)^1.000771219)))
	Else
		AudioPan = (0.8745898957*(ABS(tmp)^12.78313661)) + (0.1264569796*(ABS(tmp)^1.000771219))
	End If
End Function

Function xGain(TableObj)
'xGain algorithm calculates a PlaySound Volume parameter multiplier to provide a Constant Power "pan".
'PFOption=1:  xGain = 1 at PF Left, xGain = 0.32931 (-3dB for PlaySound's volume parameter) at PF Center and xGain = 1 at PF Right.  Used for Left & Right stereo PF Speakers.
'PFOption=2:  xGain = 1 at PF Top, xGain = 0.32931 (-3dB for PlaySound's volume parameter) at PF Center and xGain = 1 at PF Bottom.  Used for Top & Bottom stereo PF Speakers.
	Dim tmp, PI
    If PFOption=1 Then tmp = TableObj.x * 2 / C37.width-1
	If PFOption=2 Then tmp = TableObj.y * 2 / C37.height-1
	PI = 4 * ATN(1)
	If tmp < 0 Then
	xGain = 0.3293074856*EXP(-0.9652695455*tmp^3 - 2.452909811*tmp^2 - 2.597701999*tmp)
	Else
	xGain = 0.3293074856*EXP(-0.9652695455*-tmp^3 - 2.452909811*-tmp^2 - 2.597701999*-tmp)
	End If
'	TB1.text = "xGain=" & Round(xGain,4)
End Function

Function XVol(tableobj)
'XVol algorithm calculates a PlaySound Volume parameter multiplier for a tableobj based on its X table position to provide a Constant Power "pan".
'XVol = 1 at PF Left, XVol = 0.32931 (-3dB for PlaySound's volume parameter) at PF Center and XVol = 0 at PF Right
Dim tmpx
	If PFOption = 3 Then
		tmpx = tableobj.x * 2 / C37.width-1
		XVol = 0.3293074856*EXP(-0.9652695455*tmpx^3 - 2.452909811*tmpx^2 - 2.597701999*tmpx)
	End If
'	TB1.text = "xVol=" & Round(xVol,4)
End Function

Function YVol(tableobj)
'YVol algorithm calculates a PlaySound Volume parameter multiplier for a tableobj based on its Y table position to provide a Constant Power "fade".
'YVol = 1 at PF Top, YVol = 0.32931 (-3dB for PlaySound's volume parameter) at PF Center and YVol = 0 at PF Bottom
Dim tmpy
	If PFOption = 3 Then
		tmpy = tableobj.y * 2 / C37.height-1
		YVol = 0.3293074856*EXP(-0.9652695455*tmpy^3 - 2.452909811*tmpy^2 - 2.597701999*tmpy)
	End If
'	TB2.text = "yVol=" & Round(yVol,4)
End Function

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

'******************************************************
'      JP's VP10 Rolling Sounds - Modified by Whirlwind
'******************************************************

'******************************************
' Explanation of the rolling sound routine
'******************************************

' ball rolling sounds are played based on the ball speed and position
' the routine checks first for deleted balls and stops the rolling sound.
' The For loop goes through all the balls on the table and checks for the ball speed and 
' if the ball is on the table (height lower than 30) then then it plays the sound
' otherwise the sound is stopped.

'New algorithms added to make sounds for TopArch Hits, Arch Rolls, ball bounces and glass hits. 
'For stereo, xGain is a Playsound volume multiplier that provides a Constant Power pan.
'For quad, multiple PlaySound commands are launched together that are panned and faded to their maximum extents where PlaySound's PAN and FADE have the least error.
'XVol and YVol are Playsound volume multipliers that provide a Constant Power "pan" and "fade".
'Subtracting XVol or YVol from 1 yields an inverse response.

Const tnob = 5 ' total number of balls

'Change GHT, GHB and PFL values based upon the real pinball table dimensions.  Values are used by the GlassHit code.
Const GHT = 2	'Glass height in inches at top of real playfield
Const GHB = 2	'Glass height in inches at bottom of real playfield
Const PFL = 40	'Real playfield length in inches

ReDim rolling(tnob)
InitRolling

ReDim ArchRolling(tnob)
InitArchRolling

Dim ArchHit
Sub TopArch_Hit
	ArchHit = 1
	ArchTimer.Enabled = True
End Sub


Dim archCount
'The ArchHit sound is played and.the ArchTimer is enabled upon the first hit of the arch by the ball 
'ArchTimer is enabled for 15ms after which ArchTimer2 is enabled for 2 seconds
'While ArchTimer2 is enabled it "locks-out" the ArchHit sound from being played again until after 2 seconds have elapsed
Sub ArchTimer_Timer
	archCount = archCount + 1
	If archCount = 1 Then
		archCount = 0
		ArchTimer.enabled = False
		If ArchTimer2.enabled = False Then ArchTimer2.enabled = True
	End If
End Sub

Dim archCount2
Sub ArchTimer2_Timer
	archCount2 = archCount2 + 1
	If archCount2 = 1 Then
		archCount2 = 0
		ArchTimer2.enabled = False
	End If
End Sub

Sub NotOnArch_Hit
	ArchHit = 0
End Sub

Sub NotOnArch2_Hit
	ArchHit = 0
End Sub

Sub InitRolling
	Dim i
	For i = 0 to tnob
		rolling(i) = False
	Next
End Sub

Sub InitArchRolling
	Dim i
	For i = 0 to tnob
		ArchRolling(i) = False
	Next
End Sub

Sub RollingTimer_Timer()
	Dim BOT, b, paSub
	BOT = GetBalls
	paSub=35000	'Playsound pitch adder for subway/ramp rolling ball sound

'TB.text="BOT(b).Z  " & formatnumber(BOT(b).Z,1)
'TB.text="BOT(b).VelZ  " & formatnumber(BOT(b).VelZ,1)
'TB.text="GLASS  " & formatnumber((BOT(b).Y * ((GHT-GHB)/PFL)) + (GHB*50/1.0625) - (BallSize/2),1)
'TB.text = "ArchTimer.enabled=" & ArchTimer.enabled & "    ArchTimer2.enabled=" & ArchTimer2.enabled & "    ArchHit=" & ArchHit

	' stop the sound of deleted balls
	For b = UBound(BOT) + 1 to tnob
		rolling(b) = False
		StopSound("BallrollingA" & b)
		StopSound("BallrollingB" & b)
		StopSound("BallrollingC" & b)
		StopSound("BallrollingD" & b)
	Next

	' exit the sub if no balls on the table
	If UBound(BOT) = -1 Then Exit Sub

	' play the rolling sound for each ball
	For b = 0 to UBound(BOT)
'	TB.text = "Ball0.z=" & (BOT(0).z)	'Use to verify ball.z (currently -3) when in Adrian's new saucer primitives.


'	Ball Rolling sounds
'**********************
'	Ball=50 units=1.0625".  One unit = 0.02125"  Ball.z is ball center.
'	A ball in Adrian's saucer has a Z of -3.  Use <-5 for subway sounds.
	If PFOption = 1 or PFOption = 2 Then
		If BallVel(BOT(b)) > 1 AND BOT(b).z > 10 and BOT(b).z <26 Then	'Ball on playfield
			rolling(b) = True
			PlaySound("BallrollingA" & b), -1, Vol(BOT(b)) * 0.2 * xGain(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.
		ElseIf BallVel(BOT(b)) > 1 AND BOT(b).z < -5 Then	'Ball on subway
			PlaySound("BallrollingA" & b), -1, Vol(BOT(b)) * 0.2 * xGain(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b))+paSub, 1, 0, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.			
		ElseIf rolling(b) = True Then
			StopSound("BallrollingA" & b)
			rolling(b) = False
		End If
	End If
	
	If PFOption = 3 Then
		If BallVel(BOT(b)) > 1 AND BOT(b).z > 10 and BOT(b).z < 26 Then	'Ball on playfield
			rolling(b) = True
			PlaySound("BallrollingA" & b), -1, Vol(BOT(b)) * 0.2 *    XVol(BOT(b))  *     YVol(BOT(b)), -1, 0, Pitch(BOT(b)), 1, 0, -1	'Top Left PF Speaker
			PlaySound("BallrollingB" & b), -1, Vol(BOT(b)) * 0.2 * (1-XVol(BOT(b))) *     YVol(BOT(b)),  1, 0, Pitch(BOT(b)), 1, 0, -1	'Top Right PF Speaker
			PlaySound("BallrollingC" & b), -1, Vol(BOT(b)) * 0.2 *    XVol(BOT(b))  * (1-YVol(BOT(b))), -1, 0, Pitch(BOT(b)), 1, 0,  1	'Bottom Left PF Speaker
			PlaySound("BallrollingD" & b), -1, Vol(BOT(b)) * 0.2 * (1-XVol(BOT(b))) * (1-YVol(BOT(b))),  1, 0, Pitch(BOT(b)), 1, 0,  1	'Bottom Right PF Speaker
		ElseIf BallVel(BOT(b)) > 1 AND BOT(b).z < -5 Then	'Ball on subway
			PlaySound("BallrollingA" & b), -1, Vol(BOT(b)) * 0.2 *    XVol(BOT(b))  *     YVol(BOT(b)), -1, 0, Pitch(BOT(b))+paSub, 1, 0, -1	'Top Left PF Speaker
			PlaySound("BallrollingB" & b), -1, Vol(BOT(b)) * 0.2 * (1-XVol(BOT(b))) *     YVol(BOT(b)),  1, 0, Pitch(BOT(b))+paSub, 1, 0, -1	'Top Right PF Speaker
			PlaySound("BallrollingC" & b), -1, Vol(BOT(b)) * 0.2 *    XVol(BOT(b))  * (1-YVol(BOT(b))), -1, 0, Pitch(BOT(b))+paSub, 1, 0,  1	'Bottom Left PF Speaker
			PlaySound("BallrollingD" & b), -1, Vol(BOT(b)) * 0.2 * (1-XVol(BOT(b))) * (1-YVol(BOT(b))),  1, 0, Pitch(BOT(b))+paSub, 1, 0,  1	'Bottom Right PF Speaker
		ElseIf rolling(b) = True Then
			StopSound("BallrollingA" & b)		'Top Left PF Speaker
			StopSound("BallrollingB" & b)		'Top Right PF Speaker
			StopSound("BallrollingC" & b)		'Bottom Left PF Speaker
			StopSound("BallrollingD" & b)		'Bottom Right PF Speaker
			rolling(b) = False
		End If
	End If

'	Arch Hit and Arch Rolling sounds
'***********************************
	If PFOption = 1 or PFOption = 2 Then
		If BallVel(BOT(b)) > 1 And ArchHit =1 Then
			If ArchTimer2.enabled = 0 Then
				PlaySound("ArchHit" & b), 0, (BallVel(BOT(b))/32)^5 * xGain(BOT(b)), AudioPan(BOT(b)), 0, (BallVel(BOT(b))/40)^7, 0, 0, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.
			End If	
			ArchRolling(b) = True
			PlaySound("ArchRollA" & b), -1, (BallVel(BOT(b))/40)^5 * xGain(BOT(b)), AudioPan(BOT(b)), 0, (BallVel(BOT(b))/40)^7, 1, 0, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.
		Else
			If ArchRolling(b) = True Then
			StopSound("ArchRollA" & b)
			ArchRolling(b) = False
			End If
		End If
'	If ArchTimer2.enabled = 0 And ArchHit = 1 Then TB.text = "ArchHit vol=" & round((BallVel(BOT(b))/32)^5 * 1 * xGain(BOT(b)),4)	'Keep below 1.
	End If
	
	If PFOption = 3 Then
		If BallVel(BOT(b)) > 1 And ArchHit =1 Then
			If ArchTimer2.enabled = 0 Then
				PlaySound("ArchHit" & b),   0, (BallVel(BOT(b))/32)^5 *    XVol(BOT(b))  *     YVol(BOT(b)),  -1, 0, (BallVel(BOT(b))/40)^5, 0, 0, -1	'Top Left PF Speaker
				PlaySound("ArchHit" & b),   0, (BallVel(BOT(b))/32)^5 * (1-XVol(BOT(b))) *     YVol(BOT(b)),   1, 0, (BallVel(BOT(b))/40)^5, 0, 0, -1	'Top Right PF Speaker
				PlaySound("ArchHit" & b),   0, (BallVel(BOT(b))/32)^5 *    XVol(BOT(b))  *  (1-YVol(BOT(b))), -1, 0, (BallVel(BOT(b))/40)^5, 0, 0,  1	'Bottom Left PF Speaker
				PlaySound("ArchHit" & b),   0, (BallVel(BOT(b))/32)^5 * (1-XVol(BOT(b))) *  (1-YVol(BOT(b))),  1, 0, (BallVel(BOT(b))/40)^5, 0, 0,  1	'Bottom Right PF Speaker
			End If
			ArchRolling(b) = True
			PlaySound("ArchRollA" & b), -1, (BallVel(BOT(b))/40)^5 *    XVol(BOT(b))  *     YVol(BOT(b)),  -1, 0, (BallVel(BOT(b))/40)^5, 1, 0, -1	'Top Left PF Speaker
			PlaySound("ArchRollB" & b), -1, (BallVel(BOT(b))/40)^5 * (1-XVol(BOT(b))) *     YVol(BOT(b)),   1, 0, (BallVel(BOT(b))/40)^5, 1, 0, -1	'Top Right PF Speaker
			PlaySound("ArchRollC" & b), -1, (BallVel(BOT(b))/40)^5 *    XVol(BOT(b))  *  (1-YVol(BOT(b))), -1, 0, (BallVel(BOT(b))/40)^5, 1, 0,  1	'Bottom Left PF Speaker
			PlaySound("ArchRollD" & b), -1, (BallVel(BOT(b))/40)^5 * (1-XVol(BOT(b))) *  (1-YVol(BOT(b))),  1, 0, (BallVel(BOT(b))/40)^5, 1, 0,  1	'Bottom Right PF Speaker
		Else
			If ArchRolling(b) = True Then
			StopSound("ArchRollA" & b)	'Top Left PF Speaker
			StopSound("ArchRollB" & b)	'Top Right PF Speaker
			StopSound("ArchRollC" & b)	'Bottom Left PF Speaker
			StopSound("ArchRollD" & b)	'Bottom Right PF Speaker
			ArchRolling(b) = False
			End If
		End If
	End If

'	Ball drop sounds
'*******************
'Four intensities of ball bounce sound files ranging from 1 to 4 bounces.  The number of bounces increases as the ball's downward Z velocity increases.
'A BOT(b).VelZ < -2 eliminates nuisance ball bounce sounds.

	If PFOption = 1 or PFOption = 2 Then
		If BOT(b).VelZ > -4 And BOT(b).VelZ < -2 And BOT(b).Z > 24 And BallinPlay => 1 Then
			PlaySound "BallDrop1" & b, 0, ABS(BOT(b).VelZ)/600 * xGain(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 0, 0, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.
		ElseIf BOT(b).VelZ > -8 And BOT(b).VelZ < -4 And BOT(b).Z > 24 And BallinPlay => 1 Then
			PlaySound "BallDrop2" & b, 0, ABS(BOT(b).VelZ)/600 * xGain(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 0, 0, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.
		ElseIf BOT(b).VelZ > -12 And BOT(b).VelZ < -8 And BOT(b).Z > 24 And BallinPlay => 1 Then
			PlaySound "BallDrop3" & b, 0, ABS(BOT(b).VelZ)/600 * xGain(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 0, 0, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.
		ElseIf BOT(b).VelZ < -12 And BOT(b).Z > 24 And BallinPlay => 1 Then
			PlaySound "BallDrop4" & b, 0, ABS(BOT(b).VelZ)/600 * xGain(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 0, 0, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.
		End If
	End If

	If PFOption = 3 Then
		If BOT(b).VelZ > -4 And BOT(b).VelZ < -2 And BOT(b).Z > 24 And BallinPlay => 1 Then
			PlaySound "BallDrop1" & b, 0, ABS(BOT(b).VelZ)/600 *    XVol(BOT(b))  *     YVol(BOT(b)), -1, 0, Pitch(BOT(b)), 1, 0, -1	'Top Left PF Speaker
			PlaySound "BallDrop1" & b, 0, ABS(BOT(b).VelZ)/600 * (1-XVol(BOT(b))) *     YVol(BOT(b)),  1, 0, Pitch(BOT(b)), 1, 0, -1	'Top Right PF Speaker
			PlaySound "BallDrop1" & b, 0, ABS(BOT(b).VelZ)/600 *    XVol(BOT(b))  * (1-YVol(BOT(b))), -1, 0, Pitch(BOT(b)), 1, 0,  1	'Bottom Left PF Speaker
			PlaySound "BallDrop1" & b, 0, ABS(BOT(b).VelZ)/600 * (1-XVol(BOT(b))) * (1-YVol(BOT(b))),  1, 0, Pitch(BOT(b)), 1, 0,  1	'Bottom Right PF Speaker
		ElseIf BOT(b).VelZ > -8 And BOT(b).VelZ < -4 And BOT(b).Z > 24 And BallinPlay => 1 Then
			PlaySound "BallDrop2" & b, 0, ABS(BOT(b).VelZ)/600 *    XVol(BOT(b))  *     YVol(BOT(b)), -1, 0, Pitch(BOT(b)), 1, 0, -1	'Top Left PF Speaker
			PlaySound "BallDrop2" & b, 0, ABS(BOT(b).VelZ)/600 * (1-XVol(BOT(b))) *     YVol(BOT(b)),  1, 0, Pitch(BOT(b)), 1, 0, -1	'Top Right PF Speaker
			PlaySound "BallDrop2" & b, 0, ABS(BOT(b).VelZ)/600 *    XVol(BOT(b))  * (1-YVol(BOT(b))), -1, 0, Pitch(BOT(b)), 1, 0,  1	'Bottom Left PF Speaker
			PlaySound "BallDrop2" & b, 0, ABS(BOT(b).VelZ)/600 * (1-XVol(BOT(b))) * (1-YVol(BOT(b))),  1, 0, Pitch(BOT(b)), 1, 0,  1	'Bottom Right PF Speaker
		ElseIf BOT(b).VelZ > -12 And BOT(b).VelZ < -8 And BOT(b).Z > 24 And BallinPlay => 1 Then
			PlaySound "BallDrop3" & b, 0, ABS(BOT(b).VelZ)/600 *    XVol(BOT(b))  *     YVol(BOT(b)), -1, 0, Pitch(BOT(b)), 1, 0, -1	'Top Left PF Speaker
			PlaySound "BallDrop3" & b, 0, ABS(BOT(b).VelZ)/600 * (1-XVol(BOT(b))) *     YVol(BOT(b)),  1, 0, Pitch(BOT(b)), 1, 0, -1	'Top Right PF Speaker
			PlaySound "BallDrop3" & b, 0, ABS(BOT(b).VelZ)/600 *    XVol(BOT(b))  * (1-YVol(BOT(b))), -1, 0, Pitch(BOT(b)), 1, 0,  1	'Bottom Left PF Speaker
			PlaySound "BallDrop3" & b, 0, ABS(BOT(b).VelZ)/600 * (1-XVol(BOT(b))) * (1-YVol(BOT(b))),  1, 0, Pitch(BOT(b)), 1, 0,  1	'Bottom Right PF Speaker
		ElseIf BOT(b).VelZ < -12 And BOT(b).Z > 24 And BallinPlay => 1 Then
			PlaySound "BallDrop4" & b, 0, ABS(BOT(b).VelZ)/600 *    XVol(BOT(b))  *     YVol(BOT(b)), -1, 0, Pitch(BOT(b)), 1, 0, -1	'Top Left PF Speaker
			PlaySound "BallDrop4" & b, 0, ABS(BOT(b).VelZ)/600 * (1-XVol(BOT(b))) *     YVol(BOT(b)),  1, 0, Pitch(BOT(b)), 1, 0, -1	'Top Right PF Speaker
			PlaySound "BallDrop4" & b, 0, ABS(BOT(b).VelZ)/600 *    XVol(BOT(b))  * (1-YVol(BOT(b))), -1, 0, Pitch(BOT(b)), 1, 0,  1	'Bottom Left PF Speaker
			PlaySound "BallDrop4" & b, 0, ABS(BOT(b).VelZ)/600 * (1-XVol(BOT(b))) * (1-YVol(BOT(b))),  1, 0, Pitch(BOT(b)), 1, 0,  1	'Bottom Right PF Speaker
		End If
	End If
'TB.text="BOT(b).VelZ  " & round(BOT(b).VelZ,1)

'	Glass hit sounds
'*******************
'	Ball=50 units=1.0625".  Ball.z is ball center.  Balls are physically limited by Top Glass Height.  Max ball.z is 25 units below Top Glass Height.
'	To ensure ball can go high enough to trigger glass hit, make Table Options/Dimensions & Slope/Top Glass Height equal to (GHT*50/1.0625) + 5

	If PFOption = 1 or PFOption = 2 Then
		If BOT(b).Z > (BOT(b).Y * ((GHT-GHB)/PFL)) + (GHB*50/1.0625) - BallSize/2 And BallinPlay => 1 Then
			PlaySound "GlassHit" & b, 0, ABS(BOT(b).VelZ)/30 * xGain(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 0, 0, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.
		End If
	End If

	If PFOption = 3 Then
		If BOT(b).Z > (BOT(b).Y * ((GHT-GHB)/PFL)) + (GHB*50/1.0625) - Ballsize/2 And BallinPlay => 1 Then
			PlaySound "GlassHit" & b, 0, ABS(BOT(b).VelZ)/30 *    XVol(BOT(b))  *     YVol(BOT(b)), -1, 0, Pitch(BOT(b)), 0, 0, -1	'Top Left PF Speaker
			PlaySound "GlassHit" & b, 0, ABS(BOT(b).VelZ)/30 * (1-XVol(BOT(b))) *     YVol(BOT(b)),  1, 0, Pitch(BOT(b)), 0, 0, -1	'Top Right PF Speaker
			PlaySound "GlassHit" & b, 0, ABS(BOT(b).VelZ)/30 *    XVol(BOT(b))  * (1-YVol(BOT(b))), -1, 0, Pitch(BOT(b)), 0, 0,  1	'Bottom Left PF Speaker
			PlaySound "GlassHit" & b, 0, ABS(BOT(b).VelZ)/30 * (1-XVol(BOT(b))) * (1-YVol(BOT(b))),  1, 0, Pitch(BOT(b)), 0, 0,  1	'Bottom Right PF Speaker
		End If
	End If
	Next
End Sub

'*************Hit Sound Routines
'Eliminated the Hit Subs extra velocity criteria since the PlayFieldSoundAB command already incorporates the balls velocity.

Sub aRubberPins_Hit(idx)
	PlayFieldSoundAB "pinhit_low", 0, 1
End Sub

Sub aTargets_Hit(idx)
	PlayFieldSoundAB "target", 0, 1
End Sub

Sub aMetalsThin_Hit(idx)
	PlayFieldSoundAB "metalhit_thin", 0, 1
End Sub

Sub aMetalsMedium_Hit(idx)
	PlayFieldSoundAB "metalhit_medium", 0, 1
End Sub

Sub aMetals2_Hit(idx)
	PlayFieldSoundAB "metalhit2", 0, 1
End Sub

Sub aGates_Hit(idx)
	PlayFieldSoundAB "gate4", 0, 1
End Sub

Sub aRubberBands_Hit(idx)
	If BallinPlay > 0 Then	'Eliminates the thump of Trough Ball Creation balls hitting walls 9 and 14 during table initiation
	PlayFieldSoundAB "rubber2", 0, 1
	End If
End Sub

Sub RubberWheel_hit
	PlayFieldSoundAB "rubber_hit_2", 0, 1
End Sub

Sub aPosts_Hit(idx)
	PlayFieldSoundAB "rubber2", 0, 1
End Sub

Sub aWoods_Hit(idx)
	PlayFieldSoundAB "wood", 0, 1
End Sub

Sub LeftFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RightFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlayFieldSoundAB "flip_hit_1", 0, 1
		Case 2 : PlayFieldSoundAB "flip_hit_2", 0, 1
		Case 3 : PlayFieldSoundAB "flip_hit_3", 0, 1
	End Select
End Sub

Sub ApronWall_Hit
	Dim Volume
	If ActiveBall.vely < 0 Then	Volume = abs(ActiveBall.vely) / 1 Else Volume = ActiveBall.vely / 30	'The first bounce is -vely subsequent bounces are +vely
'		TextBox1.text = "Volume = " & Volume
		If ActiveBall.z > 24 Then
			If PFOption = 1 Or PFOption = 2 Then
				PlaySound "ApronHit", 0, Volume * xGain(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.
			End If
			If PFOption = 3 Then
				PlaySound "ApronHit", 0, Volume *    XVol(ActiveBall)  *    YVol(ActiveBall),  -1, 0, Pitch(ActiveBall), 0, 0, -1	'Top Left PF Speaker
				PlaySound "ApronHit", 0, Volume * (1-XVol(ActiveBall)) *    YVol(ActiveBall),   1, 0, Pitch(ActiveBall), 0, 0, -1	'Top Right PF Speaker
				PlaySound "ApronHit", 0, Volume *    XVol(ActiveBall)  * (1-YVol(ActiveBall)), -1, 0, Pitch(ActiveBall), 0, 0,  1	'Bottom Left PF Speaker
				PlaySound "ApronHit", 0, Volume * (1-XVol(ActiveBall)) * (1-YVol(ActiveBall)),  1, 0, Pitch(ActiveBall), 0, 0,  1	'Bottom Right PF Speaker
			End If
		End If
End Sub

'**********************
' Ball Collision Sound
'**********************

'**************************************
' Explanation of the collision routine
'**************************************

' The collision is built in VP.
' You only need to add a Sub OnBallBallCollision(ball1, ball2, velocity) and when two balls collide they 
' will call this routine.

'New algorithm for BallBallCollision
'For stereo, xGain is a Playsound volume multiplier that provides a Constant Power pan.
'For quad, multiple PlaySound commands are launched together that are panned and faded to their maximum extents where PlaySound's PAN and FADE have the least error.
'XVol and YVol are Playsound volume multipliers that provide a Constant Power "pan" and "fade".
'Subtracting XVol or YVol from 1 yields an inverse response.

Sub OnBallBallCollision(ball1, ball2, velocity)
	If PFOption = 1 or PFOption = 2 Then
		PlaySound "BBcollide", 0, (Csng(velocity) ^2 / 2000) * xGain(ball1), AudioPan(ball1), 0, Pitch(ball1), 0, 0, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.
	End If
	If PFOption = 3 Then
		PlaySound "BBcollide", 0, (Csng(velocity) ^2 / 2000) *    XVol(ball1)  *    YVol(ball1),  -1, 0, Pitch(ball1), 0, 0, -1	'Top Left Playfield Speaker
		PlaySound "BBcollide", 0, (Csng(velocity) ^2 / 2000) * (1-XVol(ball1)) *    YVol(ball1),   1, 0, Pitch(ball1), 0, 0, -1	'Top Right Playfield Speaker
		PlaySound "BBcollide", 0, (Csng(velocity) ^2 / 2000) *    XVol(ball1)  * (1-YVol(ball1)), -1, 0, Pitch(ball1), 0, 0,  1	'Bottom Left Playfield Speaker
		PlaySound "BBcollide", 0, (Csng(velocity) ^2 / 2000) * (1-XVol(ball1)) * (1-YVol(ball1)),  1, 0, Pitch(ball1), 0, 0,  1	'Bottom Right Playfield Speaker
	End If
End Sub

Sub PlayFieldSound (SoundName, Looper, TableObject, VolMult)
'Plays the sound of a table object at the table object's coordinates.
'For stereo, xGain is a Playsound volume multiplier that provides a Constant Power pan.
'For quad, multiple PlaySound commands are launched together that are panned and faded to their maximum extents where PlaySound's PAN and FADE have the least error.
'XVol and YVol are Playsound volume multipliers that provide a Constant Power "pan" and "fade".
'Subtracting XVol or YVol from 1 yields an inverse response.

	If PFOption = 1 Or PFOption = 2 Then
		If Looper = -1 Then
			PlaySound SoundName&"A", Looper, VolMult * xGain(TableObject), AudioPan(TableObject), 0, 0, 0, 0, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.
		End If
		If Looper = 0 Then
			PlaySound SoundName, Looper, VolMult * xGain(TableObject), AudioPan(TableObject), 0, 0, 0, 0, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.
		End If
	End If
	If PFOption = 3 Then
		If Looper = -1 Then
			PlaySound SoundName&"A", Looper, VolMult *    XVol(TableObject)  *    YVol(TableObject),  -1, 0, 0, 0, 0, -1	'Top Left PF Speaker
			PlaySound SoundName&"B", Looper, VolMult * (1-XVol(TableObject)) *    YVol(TableObject),   1, 0, 0, 0, 0, -1	'Top Right PF Speaker
			PlaySound SoundName&"C", Looper, VolMult *    XVol(TableObject)  * (1-YVol(TableObject)), -1, 0, 0, 0, 0,  1	'Bottom Left PF Speaker
			PlaySound SoundName&"D", Looper, VolMult * (1-XVol(TableObject)) * (1-YVol(TableObject)),  1, 0, 0, 0, 0,  1	'Bottom Right PF Speaker
		End If
		If Looper = 0 Then
			PlaySound SoundName, Looper, VolMult *    XVol(TableObject)  *    YVol(TableObject),  -1, 0, 0, 0, 0, -1	'Top Left PF Speaker
			PlaySound SoundName, Looper, VolMult * (1-XVol(TableObject)) *    YVol(TableObject),   1, 0, 0, 0, 0, -1	'Top Right PF Speaker
			PlaySound SoundName, Looper, VolMult *    XVol(TableObject)  * (1-YVol(TableObject)), -1, 0, 0, 0, 0,  1	'Bottom Left PF Speaker
			PlaySound SoundName, Looper, VolMult * (1-XVol(TableObject)) * (1-YVol(TableObject)),  1, 0, 0, 0, 0,  1	'Bottom Right PF Speaker
		End If
	End If
End Sub

Sub PlayFieldSoundAB (SoundName, Looper, VolMult)
'Plays the sound of a table object at the Active Ball's location.
'For stereo, xGain is a Playsound volume multiplier that provides a Constant Power pan.
'For quad, multiple PlaySound commands are launched together that are panned and faded to their maximum extents where PlaySound's PAN and FADE have the least error.
'XVol and YVol are Playsound volume multipliers that provide a Constant Power "pan" and "fade".
'Subtracting XVol or YVol from 1 yields an inverse response.

	If PFOption = 1 Or PFOption = 2 Then
		PlaySound SoundName, Looper, VolMult * Vol(ActiveBall) * xGain(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.
	End If
	If PFOption = 3 Then
		PlaySound SoundName, Looper, VolMult * Vol(ActiveBall) *    XVol(ActiveBall)  *    YVol(ActiveBall),  -1, 0, Pitch(ActiveBall), 0, 0, -1	'Top Left PF Speaker
		PlaySound SoundName, Looper, VolMult * Vol(ActiveBall) * (1-XVol(ActiveBall)) *    YVol(ActiveBall),   1, 0, Pitch(ActiveBall), 0, 0, -1	'Top Right PF Speaker
		PlaySound SoundName, Looper, VolMult * Vol(ActiveBall) *    XVol(ActiveBall)  * (1-YVol(ActiveBall)), -1, 0, Pitch(ActiveBall), 0, 0,  1	'Bottom Left PF Speaker
		PlaySound SoundName, Looper, VolMult * Vol(ActiveBall) * (1-XVol(ActiveBall)) * (1-YVol(ActiveBall)),  1, 0, Pitch(ActiveBall), 0, 0,  1	'Bottom Right PF Speaker
	End If
End Sub

Sub PlayReelSound (SoundName, Pan)
Dim ReelVolAdj
ReelVolAdj = 0.2
'Provides a Constant Power Pan for the backglass reel sound volume to match the playfield's Constant Power Pan response
	If showDT = False Then	'-3dB for desktop mode
		If Pan = "Lpan" Then PlaySound SoundName, 0, ReelVolAdj * 1.00, -0.12, 0, 0, 0, 1, 0	'Panned 3/4 Left at 0dB * ReelVolAdj
		If Pan = "Mpan" Then PlaySound SoundName, 0, ReelVolAdj * 0.33,  0.00, 0, 0, 0, 1, 0	'Panned Middle at -3dB * ReelVolAdj
		If Pan = "Rpan" Then PlaySound SoundName, 0, ReelVolAdj * 1.00,  0.12, 0, 0, 0, 1, 0	'Panned 3/4 Right at 0dB * ReelVolAdj
	Else
		If Pan = "Lpan" Then PlaySound SoundName, 0, ReelVolAdj * 0.33, -0.12, 0, 0, 0, 1, 0	'Panned 3/4 Left at -3dB * ReelVolAdj
		If Pan = "Mpan" Then PlaySound SoundName, 0, ReelVolAdj * 0.11,  0.00, 0, 0, 0, 1, 0	'Panned Middle at -6dB * ReelVolAdj
		If Pan = "Rpan" Then PlaySound SoundName, 0, ReelVolAdj * 0.33,  0.12, 0, 0, 0, 1, 0	'Panned 3/4 Right at -3dB * ReelVolAdj
	End If
End Sub

' Roth's drop target routine
'************************************************************************************************
'These are the hit subs for the DT walls that start things off and send the number of the wall hit to the DTHit sub
Sub DW001_Hit : DTHit 01 : dropplate1.visible=0: CheckDrop(1): End Sub
Sub DW002_Hit : DTHit 02 : dropplate2.visible=0: CheckDrop(2): End Sub
Sub DW003_Hit : DTHit 03 : dropplate3.visible=0: CheckDrop(3): End Sub
Sub DW004_Hit : DTHit 04 : dropplate4.visible=0: CheckDrop(4): End Sub

Sub ResetDrops
	DTRaise 1: DTRaise 2: DTRaise 3: DTRaise 4: dropplate1.visible=1: dropplate2.visible=1: dropplate3.visible=1: dropplate4.visible=1	
	dropdone = 0
End Sub
		
'Configure the behavior of Drop Targets.
Const DTDropSpeed = 110             'in milliseconds
Const DTDropUpSpeed = 40            'in milliseconds
Const DTDropUnits = 44              'VP units primitive drops
Const DTDropUpUnits = 10            'VP units primitive raises above the up position on drops up
Const DTMaxBend = 8                 'max degrees primitive rotates when hit
Const DTDropDelay = 20              'time in milliseconds before target drops (due to friction/impact of the ball)
Const DTRaiseDelay = 40             'time in milliseconds before target drops back to normal up position after the solendoid fires to raise the target
Const DTBrickVel = 30               'velocity at which the target will brick, set to '0' to disable brick
Const DTEnableBrick = 1             'Set to 0 to disable bricking, 1 to enable bricking
Const DTDropSound = "DTDrop"        'Drop Target Drop sound
Const DTResetSound = "DTReset"      'Drop Target reset sound
Const DTHitSound = "Target_Hit_1"   'Drop Target Hit sound
Const DTMass = 0.2                  'Mass of the Drop Target (between 0 and 1), higher values provide more resistance

'******************************************************
'                                DROP TARGETS FUNCTIONS
'******************************************************
'An array of objects for each DT of (primary wall, secondary wall, primitive, switch, animate variable)
Dim DT001, DT002, DT003, DT004
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

Set DT001 = (new DropTarget)(DW001, DOW001, drop001, 01, 0, false)
Set DT002 = (new DropTarget)(DW002, DOW002, drop002, 02, 0, false)
Set DT003 = (new DropTarget)(DW003, DOW003, drop003, 03, 0, false)
Set DT004 = (new DropTarget)(DW004, DOW004, drop004, 04, 0, false)

'An array of DT arrays
Dim DTArray
DTArray = Array(DT001, DT002, DT003, DT004)

' This function looks over the DTArray and polls the ID the target hit (ie 06) and returns its position in the array (ie 0)
Function DTArrayID(switch)
    Dim i
    For i = 0 to uBound(DTArray) 
		If DTArray(i).sw = switch Then DTArrayID = i: Exit Function 
    Next
End Function' This function looks over the DTArray and pulls the ID the target hit (ie 06))

Sub DTRaise(switch)
    Dim i
    i = DTArrayID(switch)
    DTArray(i).animate = -1 'this sets the last variable in the DT array to -1 from 0 to raise DT
    DoDTAnim
End Sub

Sub DTDrop(switch)
    Dim i
    i = DTArrayID(switch)
    DTArray(i).animate = 1 'this sets the last variable in the DT array to 1 from 0
    DoDTAnim
End Sub

Sub DTHit(switch)
    Dim i
    i = DTArrayID(switch) ' this sets i to be the position of the DT in the array DTArray

    DTArray(i).animate =  DTCheckBrick(Activeball, DTArray(i).prim) ' this sets the animate value (-1 raise, 1&4 drop, 0 do nothing, 3 bend backwards, 2 BRICK

    If DTArray(i).animate = 1 or DTArray(i).animate = 3 or DTArray(i).animate = 4 Then ' if the value from brick checking is not 2 then apply ball physics
	DTBallPhysics Activeball, DTArray(i).prim.rotz, DTMass
    End If
    DoDTAnim
End Sub

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

Sub DTAnim_Timer()  ' 10 ms timer
    DoDTAnim
End Sub

'Check if target is hit on it's face or sides and whether a 'brick' occurred
Function DTCheckBrick(aBall, dtprim) 
    dim bangle, bangleafter, rangle, rangle2, Xintersect, Yintersect, cdist, perpvel, perpvelafter, paravel, paravelafter
    rangle = (dtprim.rotz - 90) * 3.1416 / 180
    rangle2 = dtprim.rotz * 3.1416 / 180
    bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
    bangleafter = Atn2(aBall.vely,aball.velx)

    Xintersect = (aBall.y - dtprim.y - tan(bangle) * aball.x + tan(rangle2) * dtprim.x) / (tan(rangle2) - tan(bangle))
    Yintersect = tan(rangle2) * Xintersect + (dtprim.y - tan(rangle2) * dtprim.x)

    cdist = Distance(dtprim.x, dtprim.y, Xintersect, Yintersect)

    perpvel = cor.BallVel(aball.id) * cos(bangle-rangle)
    paravel = cor.BallVel(aball.id) * sin(bangle-rangle)

    perpvelafter = BallSpeed(aBall) * cos(bangleafter - rangle) 
    paravelafter = BallSpeed(aBall) * sin(bangleafter - rangle)
    'debug.print "brick " & perpvel & " : " & paravel & " : " & perpvelafter & " : " & paravelafter

    If perpvel > 0 and perpvelafter <= 0 Then
	If DTEnableBrick = 1 and  perpvel > DTBrickVel and DTBrickVel <> 0 and cdist < 8 Then
	    DTCheckBrick = 3
        Else
            DTCheckBrick = 1
        End If
    ElseIf perpvel > 0 and ((paravel > 0 and paravelafter > 0) or (paravel < 0 and paravelafter < 0)) Then
        DTCheckBrick = 4
    Else 
        DTCheckBrick = 0
    End If

	DTCheckBrick = 1
End Function

'**************************************How this all works********************************************
'This uses walls to register the hits and primitives that get dropped as well as an offsetwall which is a wall that is set behind the main wall.
'1) The wallTarget, offsetWallTarget, primitive, target number and animation value of 0 are put into arrays called DT006 - DT010
'2) A master array called DTArray contains all of the DT0xx arrays
'3) If a wall is hit then the position in the array of the wall hit is determined
'4) The animation value is calculated by DTCheckBrick
'5) If the animation value is 1,3 or 4 then DTBallPhysics applies velocity corrections to the active ball
'6) the animation sub is called and it passes each element of the DT array to the DTAnimate function
'  - if the animate value is 0 do nothing
'  - if the animate value is 1 or 4 then bend the dt back make the front wall not collidable and turn on the offset wall's collide and 
'    when the elapsed drop target delay time has passed change the animate value to 2
'  - if the animate value is two then drop the target and once it is dropped turn off the collide for the offset wall
'  - if the animate value is 3 then BRICK  bend the prim back and the forth but no drop 
'  - if the anumate value is -1 then raise the drop target past its resting point and drop back down and if a ball is over it kick the ball up in the air
'****************************************************************************************************

Sub DoDTAnim()
        Dim i
        For i = 0 to Ubound(DTArray)
	    DTArray(i).animate = DTAnimate(DTArray(i).primary, DTArray(i).secondary, DTArray(i).prim, DTArray(i).sw, DTArray(i).animate)
        Next
End Sub

' This is the function that animates the DT drop and raise
Function DTAnimate(primary, secondary, prim, switch,  animate)
        dim transz
        Dim animtime, rangle
        rangle = prim.rotz * 3.1416 / 180 ' number of radians

        DTAnimate = animate

        if animate = 0  Then  ' no action to be taken
                primary.uservalue = 0  ' primary.uservalue is used to keep track of gameTime
                DTAnimate = 0
                Exit Function
        Elseif primary.uservalue = 0 then 
                primary.uservalue = gametime ' sets primary.uservalue to game time for calculating how much time has elapsed
        end if

        animtime = gametime - primary.uservalue 'variable for elapsed time

        If (animate = 1 or animate = 4) and animtime < DTDropDelay Then 'if the time elapse is less than time for the dt to start to drop after impact
                primary.collidable = 0 'primary wall is not collidable
                If animate = 1 then secondary.collidable = 1 else secondary.collidable = 0 'animate 1 turns on offset wall collide and 4 turns offest collide off
                prim.rotx = DTMaxBend * cos(rangle) ' bend the primitive back the max value
                prim.roty = DTMaxBend * sin(rangle)
                DTAnimate = animate
                Exit Function
        elseif (animate = 1 or animate = 4) and animtime > DTDropDelay Then ' if the drop time has passed then
                primary.collidable = 0 ' primary wall is not collidable
                If animate = 1 then secondary.collidable = 1 else secondary.collidable = 0 'animate 1 turns on offset wall collide and 4 turns offest collide off
                prim.rotx = DTMaxBend * cos(rangle) ' bend the primitive back the max value
                prim.roty = DTMaxBend * sin(rangle)
                animate = 2 '**** sets animate to 2 for dropping the DT
                playFieldSound "DTDrop", 0, prim, 1  
        End If

        if animate = 2 Then ' DT drop time
                transz = (animtime - DTDropDelay)/DTDropSpeed *  DTDropUnits * -1
                if prim.transz > -DTDropUnits  Then ' if not fully dropped then transz
                        prim.transz = transz
                end if

                prim.rotx = DTMaxBend * cos(rangle)/2
                prim.roty = DTMaxBend * sin(rangle)/2

                if prim.transz <= -DTDropUnits Then  ' if fully dropped then 
                        prim.transz = -DTDropUnits
                        secondary.collidable = 0 ' turn off collide for secondary wall now the rubber behind can be hit
                        'controller.Switch(Switch) = 1
                        primary.uservalue = 0 ' reset the time keeping value
                        DTAnimate = 0 ' turn off animation
                        Exit Function
                Else
                        DTAnimate = 2
                        Exit Function
                end If 
        End If

		'*** animate 3 is a brick!
        If animate = 3 and animtime < DTDropDelay Then ' if elapsed time is less than the drop time
                primary.collidable = 0 'turn off primary collide
                secondary.collidable = 1 'turn on secondary collide
                prim.rotx = DTMaxBend * cos(rangle) 'rotate back
                prim.roty = DTMaxBend * sin(rangle)
        elseif animate = 3 and animtime > DTDropDelay Then
                primary.collidable = 1 'turn on the primary collide
                secondary.collidable = 0 'turn off secondary collide
                prim.rotx = 0 'rotate back to start
                prim.roty = 0
                primary.uservalue = 0
                DTAnimate = 0
                Exit Function
        End If

        if animate = -1 Then ' If the value is -1 raise the DT past its resting point
                transz = (1 - (animtime)/DTDropUpSpeed) *  DTDropUnits * -1
                If prim.transz = -DTDropUnits Then
                        Dim BOT, b
                        BOT = GetBalls

                        For b = 0 to UBound(BOT) ' if a ball is over a DT that is rising, pop it up in the air with a vel of 20
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

                if prim.transz > DTDropUpUnits then 'If the dt is at the top of its rise
                        DTAnimate = -2  ' set the dt animate to -2
                        prim.rotx = 0  'remove the rotation
                        prim.roty = 0
                        primary.uservalue = gametime
                end if
                primary.collidable = 0
                secondary.collidable = 1
                'controller.Switch(Switch) = 0

        End If

        if animate = -2 and animtime > DTRaiseDelay Then ' if the value is -2 then drop back down to resting height
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

'******************************************************
'                DROP TARGET
'                SUPPORTING FUNCTIONS 
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



'******************************************************
'				FLIPPER AND RUBBER CORRECTION
'******************************************************

dim LFPress, RFPress, EOST, EOSA, EOSTnew, EOSAnew
dim FStrength, Frampup, FElasticity, EOSRampup, SOSRampup
dim RFEndAngle, LFEndAngle, LFCount, RFCount, LiveCatch
	LFEndAngle = 70: RFEndAngle = -70

EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
FStrength = LeftFlipper.strength
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
EOSTnew = 1.0 'FEOST
EOSAnew = 0.2
EOSRampup = 1.5 
SOSRampup = 8.5 
LiveCatch = 8

'********Need to have a flipper timer to check for these values
Sub flipperTimer_Timer

	FlipperLB.rotz = LeftFlipper.currentangle 
	FlipperLR.rotz = LeftFlipper.currentangle 
	FlipperRB.rotz = RightFlipper.currentangle
	FlipperRR.rotz = RightFlipper.currentangle
	if FlipperShadows=1 then
		FlipperLsh.rotz= LeftFlipper.currentangle
		FlipperRsh.rotz= RightFlipper.currentangle
	end if
	Pgate.rotz = Gate3.currentangle+25
	if bumperlightA2.state=1 then
		bumprims_prim.image = "bumperrimONGION"
		bumpcaps_prim.image = "bumpercapONGION"
	Else
		if light4.state=1 Then
			bumprims_prim.image = "bumperrimOffGION"
			bumpcaps_prim.image = "bumpercapOffGION"
		Else
			bumprims_prim.image = "bumperrimOffGIOff"
			bumpcaps_prim.image = "bumpercapOffGIOff"
		end If
	End if
	If leftflipper.currentangle = leftflipper.endangle and LFPress = 1 then 
		leftflipper.eostorqueangle = EOSAnew
		leftflipper.eostorque = EOSTnew
		LeftFlipper.rampup = EOSRampup
		if LFCount = 0 Then LFCount = GameTime
		if GameTime - LFCount < LiveCatch Then
			leftflipper.Elasticity = 0.1
			If LeftFlipper.endangle <> LFEndAngle Then leftflipper.endangle = LFEndAngle
		Else	
			leftflipper.Elasticity = FElasticity
		end if
	elseif leftflipper.currentangle > leftflipper.startangle - 0.05  Then
		leftflipper.rampup = SOSRampup
		leftflipper.endangle = LFEndAngle - 3
		leftflipper.Elasticity = FElasticity
		LFCount = 0
	elseif leftflipper.currentangle > leftflipper.endangle + 0.01 Then 
		leftflipper.eostorque = EOST
		leftflipper.eostorqueangle = EOSA
		LeftFlipper.rampup = Frampup
		leftflipper.Elasticity = FElasticity
	end if

	If rightflipper.currentangle = rightflipper.endangle and RFPress = 1 then
		rightflipper.eostorqueangle = EOSAnew
		rightflipper.eostorque = EOSTnew
		RightFlipper.rampup = EOSRampup
		if RFCount = 0 Then RFCount = GameTime
		if GameTime - RFCount < LiveCatch Then
			rightflipper.Elasticity = 0.1
			If RightFlipper.endangle <> RFEndAngle Then rightflipper.endangle = RFEndAngle
		Else
			rightflipper.Elasticity = FElasticity
		end if
	elseif rightflipper.currentangle < rightflipper.startangle + 0.05 Then
		rightflipper.rampup = SOSRampup 
		rightflipper.endangle = RFEndAngle + 3
		rightflipper.Elasticity = FElasticity
		RFCount = 0 
	elseif rightflipper.currentangle < rightflipper.endangle - 0.01 Then 
		rightflipper.eostorque = EOST
		rightflipper.eostorqueangle = EOSA
		RightFlipper.rampup = Frampup
		rightflipper.Elasticity = FElasticity
	end if
End Sub

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity

InitPolarity

Sub InitPolarity()
	dim x, a : a = Array(LF, RF)
	for each x in a
		'safety coefficient (diminishes polarity correction only)
		x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1	'disabled
		x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1

		x.enabled = True
		x.TimeDelay = 69    '*****Important, this variable is an offset for the speed that the ball travels down the table to determine if the flippers have been fired 
							'This is needed because the corrections to ball trajectory should only applied if the flippers have been fired and the ball is in the trigger zones.
							'FlipAT is set to GameTime when the ball enters the flipper trigger zones and if GameTime is less than FlipAT + this time delay then changes to velocity
							'and trajectory are applied.  If the flipper is fired before the ball enters the trigger zone then with this delay added to FlipAT the changes
							'to tragectory and velocity will not be applied.  Also if the flipper is in the final 20 degrees changes to ball values will also not be applied.
							'"Faster" tables will need a smaller value while "slower" tables will need a larger value to give the ball more time to get to the flipper. 		
							'If this value is not set high enough the Flipper Velocity and Polarity corrections will NEVER be applied.
	Next

	'rf.report "Polarity"
	AddPt "Polarity", 0, 0, -2.7
	AddPt "Polarity", 1, 0.16, -2.7	
	AddPt "Polarity", 2, 0.33, -2.7
	AddPt "Polarity", 3, 0.37, -2.7	'4.2
	AddPt "Polarity", 4, 0.41, -2.7
	AddPt "Polarity", 5, 0.45, -2.7 '4.2
	AddPt "Polarity", 6, 0.576,-2.7
	AddPt "Polarity", 7, 0.66, -1.8'-2.1896
	AddPt "Polarity", 8, 0.743, -0.5
	AddPt "Polarity", 9, 0.81, -0.5
	AddPt "Polarity", 10, 0.88, 0

	'"Velocity" Profile
	addpt "Velocity", 0, 0, 	1
	addpt "Velocity", 1, 0.16, 1.06
	addpt "Velocity", 2, 0.41, 	1.05
	addpt "Velocity", 3, 0.53, 	1'0.982
	addpt "Velocity", 4, 0.702, 0.968
	addpt "Velocity", 5, 0.95,  0.968
	addpt "Velocity", 6, 1.03, 	0.945

	LF.Object = LeftFlipper	
	LF.EndPoint = EndPointLp	'you can use just a coordinate, or an object with a .x property. Using a couple of simple primitive objects
	RF.Object = RightFlipper
	RF.EndPoint = EndPointRp
End Sub

Sub AddPt(aStr, idx, aX, aY)	'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LF, RF)
	dim x : for each x in a
		x.addpoint aStr, idx, aX, aY
	Next
End Sub


Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() :  LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub

'Methods:
'.TimeDelay - Delay before trigger shuts off automatically. Default = 80 (ms)
'.AddPoint - "Polarity", "Velocity", "Ycoef" coordinate points. Use one of these 3 strings, keep coordinates sequential. x = %position on the flipper, y = output
'.Object - set to flipper reference. Optional.
'.StartPoint - set start point coord. Unnecessary, if .object is used.

'Called with flipper - 
'ProcessBalls - catches ball data. 
' - OR - 
'.Fire - fires flipper.rotatetoend automatically + processballs. Requires .Object to be set to the flipper.

'***************This is flipperPolarity's addPoint Sub
Class FlipperPolarity
	Public Enabled
	Private FlipAt	'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay	'delay before trigger turns off and polarity is disabled TODO set time!
	private Flipper, FlipperStart, FlipperEnd, LR, PartialFlipCoef
	Private Balls(20), balldata(20)
	
	dim PolarityIn, PolarityOut
	dim VelocityIn, VelocityOut
	dim YcoefIn, YcoefOut

	Public Sub Class_Initialize 
		redim PolarityIn(0) : redim PolarityOut(0) : redim VelocityIn(0) : redim VelocityOut(0) : redim YcoefIn(0) : redim YcoefOut(0)
		Enabled = True: TimeDelay = 50 : LR = 1:  dim x : for x = 0 to uBound(balls) : balls(x) = Empty : set Balldata(x) = new spoofBall: next  
	End Sub
	
	Public Property let Object(aInput) : Set Flipper = aInput : StartPoint = Flipper.x : End Property
	Public Property Let StartPoint(aInput) : if IsObject(aInput) then FlipperStart = aInput.x else FlipperStart = aInput : end if : End Property
	Public Property Get StartPoint : StartPoint = FlipperStart : End Property
	Public Property Let EndPoint(aInput) : if IsObject(aInput) then FlipperEnd = aInput.x else FlipperEnd = aInput : end if : End Property
	Public Property Get EndPoint : EndPoint = FlipperEnd : End Property
	
	Public Sub AddPoint(aChooseArray, aIDX, aX, aY) 'Index #, X position, (in) y Position (out) 
		Select Case aChooseArray
			case "Polarity" : ShuffleArrays PolarityIn, PolarityOut, 1 : PolarityIn(aIDX) = aX : PolarityOut(aIDX) = aY : ShuffleArrays PolarityIn, PolarityOut, 0
			Case "Velocity" : ShuffleArrays VelocityIn, VelocityOut, 1 :VelocityIn(aIDX) = aX : VelocityOut(aIDX) = aY : ShuffleArrays VelocityIn, VelocityOut, 0
			Case "Ycoef" : ShuffleArrays YcoefIn, YcoefOut, 1 :YcoefIn(aIDX) = aX : YcoefOut(aIDX) = aY : ShuffleArrays YcoefIn, YcoefOut, 0
		End Select

	End Sub 

'********Triggered by a ball hitting the flipper trigger area	
	Public Sub AddBall(aBall) : dim x : 
		for x = 0 to uBound(balls)  
			if IsEmpty(balls(x)) then set balls(x) = aBall : exit sub :end if  
		Next   
	End Sub

	Private Sub RemoveBall(aBall)
		dim x : for x = 0 to uBound(balls)
			if TypeName(balls(x) ) = "IBall" then 
				if aBall.ID = Balls(x).ID Then
					balls(x) = Empty
					Balldata(x).Reset
				End If
			End If
		Next
	End Sub

'*********Used to rotate flipper since this is removed from the key down for the flippers	
	Public Sub Fire() 
		Flipper.RotateToEnd
		processballs
	End Sub

	Public Sub ProcessBalls() 'save data of balls in flipper range
		FlipAt = GameTime
		dim x : for x = 0 to uBound(balls)
			if not IsEmpty(balls(x) ) then balldata(x).Data = balls(x)
		Next
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))  '% of flipper swing
		PartialFlipCoef = abs(PartialFlipCoef-1)
		if abs(Flipper.currentAngle - Flipper.EndAngle) < 20 Then 'last 20 degrees of swing is not dealt with
			PartialFlipCoef = 0
		End If
	End Sub

'***********gameTime is a global variable of how long the game has progressed in ms
'***********This function lets the table know if the flipper has been fired
	Private Function FlipperOn() 
'		TB.text = gameTime & ":" & (FlipAT + TimeDelay) ' ******UNCOMMENT THIS WHEN THIS FLIPPER FUNCTIONALITY IS ADDED TO A NEW TABLE TO CHECK IF THE TIME DELAY IS LONG ENOUGH*****
		if gameTime < FlipAt + TimeDelay then FlipperOn = True 
	End Function	'Timer shutoff for polaritycorrect 
	
'***********This is turned on when a ball leaves the flipper trigger area
	Public Sub PolarityCorrect(aBall)
		if FlipperOn() then 'don't run this if the flippers are at rest
						tb.text = "In"
			dim tmp, BallPos, x, IDX, Ycoef : Ycoef = 1
			dim teststr : teststr = "Cutoff"
			tmp = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
			if tmp < 0.1 then 'if real ball position is behind flipper, exit Sub to prevent stucks	'Disabled 1.03, I think it's the Mesh that's causing stucks, not this
			end if

			'y safety Exit
			if aBall.VelY > -8 then 'if ball going down then remove the ball
				RemoveBall aBall
				exit Sub
			end if
			'Find balldata. BallPos = % on Flipper
			for x = 0 to uBound(Balls)
				if aBall.id = BallData(x).id AND not isempty(BallData(x).id) then 
					idx = x
					BallPos = PSlope(BallData(x).x, FlipperStart, 0, FlipperEnd, 1)
					if ballpos > 0.65 then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)				'find safety coefficient 'ycoef' data
				end if
			Next

			'Velocity correction
			if not IsEmpty(VelocityIn(0) ) then
'				tb.text = "Vel corr"
				Dim VelCoef
				if IsEmpty(BallData(idx).id) and aBall.VelY < -12 then 'if tip hit with no collected data, do vel correction anyway
					if PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1) > 1.1 then 'adjust plz
						VelCoef = LinearEnvelope(5, VelocityIn, VelocityOut)
						if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)
						if Enabled then aBall.Velx = aBall.Velx*VelCoef'VelCoef
						if Enabled then aBall.Vely = aBall.Vely*VelCoef'VelCoef
					end if
				Else
		 : 			VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)
					if Enabled then aBall.Velx = aBall.Velx*VelCoef
					if Enabled then aBall.Vely = aBall.Vely*VelCoef
				end if
			End If

			'Polarity Correction (optional now)
			if not IsEmpty(PolarityIn(0) ) then
				If StartPoint > EndPoint then LR = -1	'Reverse polarity if left flipper
				dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
				if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
			End If
		End If
		RemoveBall aBall
	End Sub
End Class

'================================
'Helper Functions


Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
	dim x, aCount : aCount = 0
	redim a(uBound(aArray) )
	for x = 0 to uBound(aArray)	'Shuffle objects in a temp array
		if not IsEmpty(aArray(x) ) Then
			if IsObject(aArray(x)) then 
				Set a(aCount) = aArray(x) 'Set creates an object in VB
			Else
				a(aCount) = aArray(x)
			End If
			aCount = aCount + 1
		End If
	Next
	if offset < 0 then offset = 0
	redim aArray(aCount-1+offset)	'Resize original array
	for x = 0 to aCount-1		'set objects back into original array
		if IsObject(a(x)) then 
			Set aArray(x) = a(x)
		Else
			aArray(x) = a(x)
		End If
	Next
End Sub

'**********Takes in more than one array and passes them to ShuffleArray
Sub ShuffleArrays(aArray1, aArray2, offset)
	ShuffleArray aArray1, offset
	ShuffleArray aArray2, offset
End Sub

'**********Calculate ball speed as hypotenuse of velX/velY triangle
Function BallSpeed(ball) 'Calculates the ball speed
    BallSpeed = SQR(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

'**********Calculates the value of Y for an input x using the slope intercept equation
Function PSlope(Input, X1, Y1, X2, Y2)	'Set up line via two points, no clamping. Input X, output Y
	dim x, y, b, m : x = input : m = (Y2 - Y1) / (X2 - X1) : b = Y2 - m*X2
	Y = M*x+b
	PSlope = Y
End Function

Class spoofball 
	Public X, Y, Z, VelX, VelY, VelZ, ID, Mass, Radius 
	Public Property Let Data(aBall)
		With aBall
			x = .x : y = .y : z = .z : velx = .velx : vely = .vely : velz = .velz
			id = .ID : mass = .mass : radius = .radius
		end with
	End Property
	Public Sub Reset()
		x = Empty : y = Empty : z = Empty  : velx = Empty : vely = Empty : velz = Empty 
		id = Empty : mass = Empty : radius = Empty
	End Sub
End Class

'****************************************************************************
'PHYSICS DAMPENERS

'These are data mined bounce curves, 
'dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
'Requires tracking ballspeed to calculate COR


Sub dPosts_Hit(idx) 
	RubbersD.dampen Activeball
	debugtext.text = "Hit post" & idx
End Sub

Sub dSleeves_Hit(idx) 
	SleevesD.Dampen Activeball
	debugtext2.text = "Hit sleeve" & idx
End Sub

'*********This sets up the rubbers:
dim RubbersD  
Set RubbersD = new Dampener  'Makes a Dampener Class Object 	
RubbersD.name = "Rubbers"

'cor bounce curve (linear)
'for best results, try to match in-game velocity as closely as possible to the desired curve
RubbersD.addpoint 0, 0, 0.935 '0.96	'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.935 '0.96
RubbersD.addpoint 2, 5.76, 0.942 '0.967	'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64	'there's clamping so interpolate up to 56 at least

dim SleevesD : Set SleevesD = new Dampener	'this is just rubber but cut down to 85%...
SleevesD.name = "Sleeves"
SleevesD.CopyCoef RubbersD, 0.85

'**********Class for dampener section of nfozzy's code
Class Dampener
	Public Print, debugOn 'tbpOut.text
	public name, Threshold 	'Minimum threshold. Useful for Flippers, which don't have a hit threshold.
	Public ModIn, ModOut
	Private Sub Class_Initialize : redim ModIn(0) : redim Modout(0): End Sub 

	Public Sub AddPoint(aIdx, aX, aY) 
		ShuffleArrays ModIn, ModOut, 1 : ModIn(aIDX) = aX : ModOut(aIDX) = aY : ShuffleArrays ModIn, ModOut, 0
	End Sub

	public sub Dampen(aBall)
		if threshold then if BallSpeed(aBall) < threshold then exit sub end if end if
		dim RealCOR, DesiredCOR, str, coef
'               Uses the LinearEnvelope function to calculate the correction based upon where it's value sits in relation
	
'               to the addpoint parameters set above.  Basically interpolates values between set points in a linear fashion
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		
'                Uses the function BallSpeed's value at the point of impact/the active ball's velocity which is constantly being updated	
'               RealCor is always less than 1 
		RealCOR = BallSpeed(aBall) / cor.ballvel(aBall.id)

'               Divides the desired CoR by the real COR to make a multiplier to correct velocity in x and y
		coef = desiredcor / realcor 
		
'               Applies the coef to x and y velocities
		aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
	End Sub

'***********This Sub sets the values for Sleeves (or any other future objects) to 85% (or whatever is passed in) of Posts
	Public Sub CopyCoef(aObj, aCoef) 'alternative addpoints, copy with coef
		dim x : for x = 0 to uBound(aObj.ModIn)
			addpoint x, aObj.ModIn(x), aObj.ModOut(x)*aCoef
		Next
	End Sub
End Class

'*****************************Generates cor.ballVel for dampener
Sub RDampen_Timer() ' 1 ms timer always on
	CoR.Update
End Sub

'*********CoR is Coefficient of Restitution defined as "how much of the kinetic energy remains for the objects 
'to rebound from one another vs. how much is lost as heat, or work done deforming the objects 
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


'********Interpolates the value for areas between the low and upper bounds sent to it
Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	dim y 'Y output
	dim L 'Line
	dim ii : for ii = 1 to uBound(xKeyFrame)	'find active line
		if xInput <= xKeyFrame(ii) then L = ii : exit for : end if
	Next
	if xInput > xKeyFrame(uBound(xKeyFrame) ) then L = uBound(xKeyFrame)	'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )

	'clamp 2.0
	if xInput <= xKeyFrame(lBound(xKeyFrame) ) then Y = yLvl(lBound(xKeyFrame) ) 	'Clamp lower
	if xInput >= xKeyFrame(uBound(xKeyFrame) ) then Y = yLvl(uBound(xKeyFrame) )	'Clamp upper

	LinearEnvelope = Y
End Function




'************************************************************************
'VR ROOM Code
'************************************************************************



'******************************************************
'*******	Set Up Backglass Flashers	*******
'******************************************************

Sub SetBackglass()
	Dim obj

	For Each obj In Backglass_flashers
		obj.x = obj.x - 15
		obj.height = - obj.y + 137
		obj.y = 35 'adjusts the distance from the backglass towards the user
	Next
	FlBGLevel1.y = 10
End Sub



'******************************************************
'*******	       VR Plunger	        *******
'******************************************************

Sub TimerVRPlunger_Timer
  If VR_Primary_plunger.Y < 2217.833 then
       VR_Primary_plunger.Y = VR_Primary_plunger.Y + 4
  End If
End Sub

Sub TimerVRPlunger2_Timer
	VR_Primary_plunger.Y = 2126.924 + (5* Plunger.Position) -20
End Sub


'******************************************************
'*******     Match and Current Ball     *******
'******************************************************

Sub FlasherMatch
	If Match = 0 Then FlM00.visible = 1 : FlM00A.visible = 1 : FlM00B.visible = 1 Else FlM00.visible = 0 : FlM00A.visible = 0 : FlM00B.visible = 0  End If
	If Match = 1 Then FlM10.visible = 1 : FlM10A.visible = 1 : FlM10B.visible = 1 Else FlM10.visible = 0 : FlM10A.visible = 0 : FlM10B.visible = 0 End If
	If Match = 2 Then FlM20.visible = 1 : FlM20A.visible = 1 : FlM20B.visible = 1 Else FlM20.visible = 0 : FlM20A.visible = 0 : FlM20B.visible = 0 End If
	If Match = 3 Then FlM30.visible = 1 : FlM30A.visible = 1 : FlM30B.visible = 1 Else FlM30.visible = 0 : FlM30A.visible = 0 : FlM30B.visible = 0 End If
	If Match = 4 Then FlM40.visible = 1 : FlM40A.visible = 1 : FlM40B.visible = 1 Else FlM40.visible = 0 : FlM40A.visible = 0 : FlM40B.visible = 0 End If
	If Match = 5 Then FlM50.visible = 1 : FlM50A.visible = 1 : FlM50B.visible = 1 Else FlM50.visible = 0 : FlM50A.visible = 0 : FlM50B.visible = 0 End If
	If Match = 6 Then FlM60.visible = 1 : FlM60A.visible = 1 : FlM60B.visible = 1 Else FlM60.visible = 0 : FlM60A.visible = 0 : FlM60B.visible = 0 End If
	If Match = 7 Then FlM70.visible = 1 : FlM70A.visible = 1 : FlM70B.visible = 1 Else FlM70.visible = 0 : FlM70A.visible = 0 : FlM70B.visible = 0 End If
	If Match = 8 Then FlM80.visible = 1 : FlM80A.visible = 1 : FlM80B.visible = 1 Else FlM80.visible = 0 : FlM80A.visible = 0 : FlM80B.visible = 0 End If
	If Match = 9 Then FlM90.visible = 1 : FlM90A.visible = 1 : FlM90B.visible = 1 Else FlM90.visible = 0 : FlM90A.visible = 0 : FlM90B.visible = 0 End If
End Sub

Sub FlasherBalls
	If CurrBall = 1 Then FlBalls1.visible = 1 Else FlBalls1.visible = 0 End If
	If CurrBall = 2 Then FlBalls2.visible = 1 Else FlBalls2.visible = 0 End If
	If CurrBall = 3 Then FlBalls3.visible = 1 Else FlBalls3.visible = 0 End If
	If CurrBall = 4 Then FlBalls4.visible = 1 Else FlBalls4.visible = 0 End If
	If CurrBall = 5 Then FlBalls5.visible = 1 Else FlBalls5.visible = 0 End If
End Sub


'******************************************************
'*******     Temperature Bar + Flasher     *******
'******************************************************

Dim BGArr2 
BGArr2=Array (FlAnimTarget0,FlAnimTarget1,FlAnimTarget2,FlAnimTarget3,FlAnimTarget4,FlAnimTarget5,_
FlAnimTarget6,FlAnimTarget7,FlAnimTarget8,FlAnimTarget9,FlAnimTarget10,FlAnimTarget11,FlAnimTarget12,_
FlAnimTarget13,FlAnimTarget14,FlAnimTarget15,FlAnimTarget16,FlAnimTarget17,FlAnimTarget18,FlAnimTarget19,_
FlAnimTarget20,FlAnimTarget21,FlAnimTarget22,FlAnimTarget23,FlAnimTarget24)

Sub fnMoveLevel (ndx)
	FlBGLevel1.height = BGArr2(ndx).height + 280
end Sub

Dim Flasherseq

Sub BGTemperatureFlasher_Timer
	Select Case Flasherseq
		Case 1:for each Object in ColFlTemperature : object.visible = 1 : next
		Case 2:for each Object in ColFlTemperature : object.visible = 1 : next
		Case 3:for each Object in ColFlTemperature : object.visible = 0 : next
		Case 4:for each Object in ColFlTemperature : object.visible = 0 : next
		Case 5:for each Object in ColFlTemperature : object.visible = 0 : next
		Case 6:for each Object in ColFlTemperature : object.visible = 0 : next
		Case 7:for each Object in ColFlTemperature : object.visible = 0 : next
		Case 8:for each Object in ColFlTemperature : object.visible = 0 : next
	End Select	
	Flasherseq = Flasherseq + 1
	If Flasherseq > 8 Then
	Flasherseq = 1
	End if
End Sub


Sub VRRoomChoice
	If VRChooseRoom = 1 Then
		for each Object in VRRoomBar : object.visible = 1 : next
		for each Object in VRRoomMinimal : object.visible = 0 : next
	End If
	If VRChooseRoom = 0 Then
		for each Object in VRRoomBar : object.visible = 0 : next
		for each Object in VRRoomMinimal : object.visible = 1 : next
	End If
End Sub


Sub Fantimer_timer()
	Fan.Objrotz = Fan.Objrotz + 2
	Fan2.Objrotz = Fan2.Objrotz + 2.2
	Fan3.Objrotz = Fan3.Objrotz + 2.3
	Fan4.Objrotz = Fan4.Objrotz + 2.4
End Sub



Dim TVCounter: TVCounter = 0

Sub TimerTV_Timer()
	TV.Image = "Frame " & TVCounter
	TVCounter = TVCounter + 1
	If TVCounter > 108 Then
		TVCounter = 0
	End If
End Sub

sub PoolLightTimer2_timer()
	PoolTable.Image = "PoolNew_l"   'change pool table image to the Lit one...
	PoolLight1.visible = true
	PoolShadow.visible = true
	PoolCue1.Image = "Pool_Cue_v1_Diffuse"
	PoolCue2.Image = "Pool_Cue_v1_DiffuseDARKHALF"
	Randomize (21)
	PoolLightTimer2.Interval = 200 * ((7 * Rnd) + 1)  ' 20 *  a random number between 1.05 and 7.95
	PoolLightTimer2.enabled = False
	PoolLightTimer1.enabled = True
	
	VRSign2.disablelighting = 1
	BlueLight1.visible = true
	
	VRSign2.Material = "_noXtrashadingLight"
End Sub


sub PoolLightTimer1_timer()
	PoolTable.Image = "PoolNew_D"
	PoolLight1.visible = false
	PoolShadow.visible = false
	PoolCue1.Image = "Pool_Cue_v1_DiffuseDARK"
	PoolCue2.Image = "Pool_Cue_v1_DiffuseDARK"
	Randomize (21)
	PoolLightTimer1.Interval = 600 * ((7 * Rnd) + 1)  ' 600 *  a random number between 1.05 and 7.95
	PoolLightTimer1.enabled = false
	PoolLightTimer2.enabled = true

	VRSign2.disablelighting = 0
	BlueLight1.visible = false

	VRSign2.Material = "_noXtrashading"
End Sub




' ***************************************************************************
'          BASIC FSS(EM) 1-4 player 5x drums, 1 credit drum CORE CODE
' ****************************************************************************


' ********************* POSITION EM REEL DRUMS ON BACKGLASS *************************

Dim xoff,yoff,zoff,xrot,zscale, xcen,ycen
Dim inx
xoff =463
yoff = 0
zoff =825
xrot = -90

Const USEEMS = 1 ' 1-4 set between 1 to 4 based on number of players

const idx_emp1r1 =0 'player 1
const idx_emp2r1 =5 'player 2
const idx_emp3r1 =10 'player 3
const idx_emp4r1 =15 'player 4
const idx_emp4r6 =20 'credits


Dim BGObjEM(1)
if USEEMS = 1 then
	BGObjEM(0) = Array(emp1r1, emp1r2, emp1r3, emp1r4, emp1r5, _
	Empty,Empty,Empty,Empty,Empty,_
	Empty,Empty,Empty,Empty,Empty,_
	Empty,Empty,Empty,Empty,Empty,_
	emp4r6) ' credits
elseif USEEMS = 2 then
	BGObjEM(0) = Array(emp1r1, emp1r2, emp1r3, emp1r4, emp1r5, _
	emp2r1, emp2r2, emp2r3, emp2r4, emp2r5, _
	Empty,Empty,Empty,Empty,Empty,_
	Empty,Empty,Empty,Empty,Empty,_
	emp4r6) ' credits
elseif USEEMS = 3 then
	BGObjEM(0) = Array(emp1r1, emp1r2, emp1r3, emp1r4, emp1r5, _
	emp2r1, emp2r2, emp2r3, emp2r4, emp2r5, _
	emp3r1, emp3r2, emp3r3, emp3r4, emp3r5, _
	Empty,Empty,Empty,Empty,Empty,_
	emp4r6) ' credits
elseif USEEMS = 4 then
	BGObjEM(0) = Array(emp1r1, emp1r2, emp1r3, emp1r4, emp1r5, _
	emp2r1, emp2r2, emp2r3, emp2r4, emp2r5, _
	emp3r1, emp3r2, emp3r3, emp3r4, emp3r5, _
	emp4r1, emp4r2, emp4r3, emp4r4, emp4r5, _
	emp4r6) ' credits
end If

Sub center_objects_em()
Dim cnt,ii, xx, yy, yfact, xfact, objs
'exit sub
yoff = -150
zscale = 0.0000001
xcen =(960 /2) - (17 / 2)
ycen = (1065 /2 ) + (313 /2)

yfact = -25
xfact = 0

cnt =0
	For Each objs In BGObjEM(0)
	If Not IsEmpty(objs) then
		if objs.name = emp4r6.name then
		yoff = -5 ' credit drum is 60% smaller
		Else
		yoff = -60
		end if

	xx =objs.x 
		
	objs.x = (xoff - xcen) + xx + xfact
	yy = objs.y 
	objs.y =yoff 

		If yy < 0 then
		yy = yy * -1
		end if

	objs.z = (zoff - ycen) + yy - (yy * zscale) + yfact
	
	'objs.rotx = xrot
	end if
	cnt = cnt + 1
	Next

end sub



' ********************* UPDATE EM REEL DRUMS CORE LIB *************************

Dim cred,ix, np,npp, reels(5, 7), scores(6,2)

'reset scores to defaults
for np =0 to 5 
scores(np,0 ) = 0
scores(np,1 ) = 0
Next

'reset EM drums to defaults
For np =0 to 3 
	For  npp =0 to 6
	reels(np, npp) =0 ' default to zero 
	Next
Next


Sub SetScore(player, ndx , val) 

Dim ncnt 

	if player = 5 or player = 6 then
		if val > 0 then
			If(ndx = 0)Then ncnt = val * 10
			If(ndx = 1)Then ncnt = val 

			scores(player, 0) = scores(player, 0) + ncnt  
		end if
	else
		if val > 0 then

		If(ndx = 0)then ncnt = val * 10000
		If(ndx = 1)then ncnt = val * 1000
		If(ndx = 2)Then ncnt = val * 100
		If(ndx = 3)Then ncnt = val * 10
		If(ndx = 4)Then ncnt = val 

		scores(player, 0) = scores(player, 0) + ncnt  
		'scores(player, 0) + ncnt  

		end if
	end if
End Sub


Sub SetDrum(player, drum , val) 
Dim cnt
Dim objs : objs =BGObjEM(0)

	If val = 0 then
		Select case player
		case -1: ' the credit drum
		If Not IsEmpty(objs(idx_emp4r6)) then
		objs(idx_emp4r6).ObjrotX = 0 ' 285
		'cnt =objs(idx_emp4r6).ObjrotX
		end if
		Case 0: 
		Select Case drum 
				Case 1: If Not IsEmpty(objs(idx_emp1r1)) then: objs(idx_emp1r1).ObjrotX = 0: end if' 283
				Case 2: If Not IsEmpty(objs(idx_emp1r1+1)) then: objs(idx_emp1r1+1).ObjrotX=0: end if
				Case 3: If Not IsEmpty(objs(idx_emp1r1+2)) then: objs(idx_emp1r1+2).ObjrotX=0: end if
				Case 4: If Not IsEmpty(objs(idx_emp1r1+3)) then: objs(idx_emp1r1+3).ObjrotX=0: end if
				Case 5: If Not IsEmpty(objs(idx_emp1r1+4)) then: objs(idx_emp1r1+4).ObjrotX=0: end if
		End Select
		Case 1: 
		Select Case drum 
				Case 1: If Not IsEmpty(objs(idx_emp2r1)) then: objs(idx_emp2r1).ObjrotX = 0: end if' 283
				Case 2: If Not IsEmpty(objs(idx_emp2r1+1)) then: objs(idx_emp2r1+1).ObjrotX=0: end if
				Case 3: If Not IsEmpty(objs(idx_emp2r1+2)) then: objs(idx_emp2r1+2).ObjrotX=0: end if
				Case 4: If Not IsEmpty(objs(idx_emp2r1+3)) then: objs(idx_emp2r1+3).ObjrotX=0: end if
				Case 5: If Not IsEmpty(objs(idx_emp2r1+4)) then: objs(idx_emp2r1+4).ObjrotX=0: end if
		End Select
		Case 2: 
		Select Case drum 
				Case 1: If Not IsEmpty(objs(idx_emp3r1)) then: objs(idx_emp3r1).ObjrotX = 0: end if' 283
				Case 2: If Not IsEmpty(objs(idx_emp3r1+1)) then: objs(idx_emp3r1+1).ObjrotX=0: end if
				Case 3: If Not IsEmpty(objs(idx_emp3r1+2)) then: objs(idx_emp3r1+2).ObjrotX=0: end if
				Case 4: If Not IsEmpty(objs(idx_emp3r1+3)) then: objs(idx_emp3r1+3).ObjrotX=0: end if
				Case 5: If Not IsEmpty(objs(idx_emp3r1+4)) then: objs(idx_emp3r1+4).ObjrotX=0: end if
		End Select
		Case 3: 
		Select Case drum 
				Case 1: If Not IsEmpty(objs(idx_emp4r1)) then: objs(idx_emp4r1).ObjrotX = 0: end if' 283
				Case 2: If Not IsEmpty(objs(idx_emp4r1+1)) then: objs(idx_emp4r1+1).ObjrotX=0: end if
				Case 3: If Not IsEmpty(objs(idx_emp4r1+2)) then: objs(idx_emp4r1+2).ObjrotX=0: end if
				Case 4: If Not IsEmpty(objs(idx_emp4r1+3)) then: objs(idx_emp4r1+3).ObjrotX=0: end if
				Case 5: If Not IsEmpty(objs(idx_emp4r1+4)) then: objs(idx_emp4r1+4).ObjrotX=0: end if
		End Select
	End Select

	else
	Select case player

		Case -1: ' the credit drum
		'emp4r6.ObjrotX = emp4r6.ObjrotX + val
		If Not IsEmpty(objs(idx_emp4r6)) then
		objs(idx_emp4r6).ObjrotX = objs(idx_emp4r6).ObjrotX + val 
		end if

		Case 0: 
		Select Case drum 
				Case 1: If Not IsEmpty(objs(idx_emp1r1)) then: objs(idx_emp1r1).ObjrotX= objs(idx_emp1r1).ObjrotX + val: end if
				Case 2: If Not IsEmpty(objs(idx_emp1r1+1)) then: objs(idx_emp1r1+1).ObjrotX= objs(idx_emp1r1+1).ObjrotX + val: end if
				Case 3: If Not IsEmpty(objs(idx_emp1r1+2)) then: objs(idx_emp1r1+2).ObjrotX= objs(idx_emp1r1+2).ObjrotX + val: end if
				Case 4: If Not IsEmpty(objs(idx_emp1r1+3)) then: objs(idx_emp1r1+3).ObjrotX= objs(idx_emp1r1+3).ObjrotX + val: end if
				Case 5: If Not IsEmpty(objs(idx_emp1r1+4)) then: objs(idx_emp1r1+4).ObjrotX= objs(idx_emp1r1+4).ObjrotX + val: end if
		End Select
		Case 1: 
		Select Case drum 
				Case 1: If Not IsEmpty(objs(idx_emp2r1)) then: objs(idx_emp2r1).ObjrotX= objs(idx_emp2r1).ObjrotX + val: end if
				Case 2: If Not IsEmpty(objs(idx_emp2r1+1)) then: objs(idx_emp2r1+1).ObjrotX= objs(idx_emp2r1+1).ObjrotX + val: end if
				Case 3: If Not IsEmpty(objs(idx_emp2r1+2)) then: objs(idx_emp2r1+2).ObjrotX= objs(idx_emp2r1+2).ObjrotX + val: end if
				Case 4: If Not IsEmpty(objs(idx_emp2r1+3)) then: objs(idx_emp2r1+3).ObjrotX= objs(idx_emp2r1+3).ObjrotX + val: end if
				Case 5: If Not IsEmpty(objs(idx_emp2r1+4)) then: objs(idx_emp2r1+4).ObjrotX= objs(idx_emp2r1+4).ObjrotX + val: end if
		End Select
		Case 2: 
		Select Case drum 
				Case 1: If Not IsEmpty(objs(idx_emp3r1)) then: objs(idx_emp3r1).ObjrotX= objs(idx_emp3r1).ObjrotX + val: end if
				Case 2: If Not IsEmpty(objs(idx_emp3r1+1)) then: objs(idx_emp3r1+1).ObjrotX= objs(idx_emp3r1+1).ObjrotX + val: end if
				Case 3: If Not IsEmpty(objs(idx_emp3r1+2)) then: objs(idx_emp3r1+2).ObjrotX= objs(idx_emp3r1+2).ObjrotX + val: end if
				Case 4: If Not IsEmpty(objs(idx_emp3r1+3)) then: objs(idx_emp3r1+3).ObjrotX= objs(idx_emp3r1+3).ObjrotX + val: end if
				Case 5: If Not IsEmpty(objs(idx_emp3r1+4)) then: objs(idx_emp3r1+4).ObjrotX= objs(idx_emp3r1+4).ObjrotX + val: end if
		End Select
		Case 3: 
		Select Case drum 
				Case 1: If Not IsEmpty(objs(idx_emp4r1)) then: objs(idx_emp4r1).ObjrotX= objs(idx_emp4r1).ObjrotX + val: end if
				Case 2: If Not IsEmpty(objs(idx_emp4r1+1)) then: objs(idx_emp4r1+1).ObjrotX= objs(idx_emp4r1+1).ObjrotX + val: end if
				Case 3: If Not IsEmpty(objs(idx_emp4r1+2)) then: objs(idx_emp4r1+2).ObjrotX= objs(idx_emp4r1+2).ObjrotX + val: end if
				Case 4: If Not IsEmpty(objs(idx_emp4r1+3)) then: objs(idx_emp4r1+3).ObjrotX= objs(idx_emp4r1+3).ObjrotX + val: end if
				Case 5: If Not IsEmpty(objs(idx_emp4r1+4)) then: objs(idx_emp4r1+4).ObjrotX= objs(idx_emp4r1+4).ObjrotX + val: end if
		End Select

	End Select
	end if
End Sub


Sub SetReel(player, drum, val)

'TextBox1.text = "playr:" & player +1 & " drum:" & drum & "val:" & val  

Dim  inc , cur, dif, fix, fval

inc = 33.5
fval = -5 ' graphic seam between 5 & 6 fix value, easier to fix here than photoshop 

If  (player <= 3) or (drum = -1) then

	If drum = -1 then drum = 0

	cur =reels(player, drum)	

	If val <> cur then ' something has changed
	Select Case drum

		Case 0: ' credits drum

			if val > cur then
				dif =val - cur
				fix =0
					If cur < 5 and cur+dif > 5 then
					fix = fix- fval
					end if
				dif = dif * inc
					
				dif = dif-fix
					
				SetDrum -1,0,  -dif
			Else
				if val = 0 Then	
				SetDrum -1,0,  0' reset the drum to abs. zero
				Else
				dif = 11 - cur
				dif = dif + val

				dif = dif * inc
				dif = dif-fval

				SetDrum -1,0,   -dif
				end if
			end if
		Case 1: 
		'TB1.text = val
		if val > cur then
			dif =val - cur
			fix =0
				If cur < 5 and cur+dif > 5 then
				fix = fix- fval
				end if
			dif = dif * inc
				
			dif = dif-fix
				
			SetDrum player,drum,  -dif
		Else
			if val = 0 Then	
			SetDrum player,drum,  0' reset the drum to abs. zero
			Else
			dif = 11 - cur
			dif = dif + val

			dif = dif * inc
			dif = dif-fval

			SetDrum player,drum,   -dif
			end if

		end if
		reels(player, drum) = val

		Case 2: 
		'TB2.text = val

		if val > cur then
			dif =val - cur
			fix =0
				If cur < 5 and cur+dif > 5 then
				fix = fix- fval
				end if
			dif = dif * inc
			dif = dif-fix
			SetDrum player,drum,  -dif
		Else
			if val = 0 Then	
			SetDrum player,drum,  0 ' reset the drum to abs. zero
			Else
			dif = 11 - cur
			dif = dif + val
			dif = dif * inc
			dif = dif-fval
			SetDrum player,drum,  -dif 
			end if
		end if
		reels(player, drum) = val

		Case 3: 
		'TB3.text = val

		if val > cur then
			dif =val - cur
			fix =0
				If cur < 5 and cur+dif > 5 then
				fix = fix- fval
				end if
			dif = dif * inc
			dif = dif-fix
			
			SetDrum player,drum,  -dif
		Else
			if val = 0 Then	
			SetDrum player,drum,  0 ' reset the drum to abs. zero
			Else
			dif = 11 - cur
			dif = dif + val
			dif = dif * inc
			dif = dif-fval
			SetDrum player,drum,  -dif 
			end if

		end if
		reels(player, drum) = val

		Case 4: 
		'TB4.text = val

		if val > cur then
			dif =val - cur
			fix =0
				If cur < 5 and cur+dif > 5 then
				fix = fix- fval
				end if
			dif = dif * inc
			dif = dif-fix
			SetDrum player,drum,  -dif 
		Else
			if val = 0 Then	
			SetDrum player,drum,  0 ' reset the drum to abs. zero
			Else
			dif = 11 - cur
			dif = dif + val
			dif = dif * inc
			dif = dif-fval
			SetDrum player,drum,  -dif 
			end if

		end if
		reels(player, drum) = val

		Case 5: 
		'TB5.text = val

		if val > cur then
			dif =val - cur
			fix =0
				If cur < 5 and cur+dif > 5 then
				fix = fix- fval
				end if
			dif = dif * inc
			dif = dif-fix
			SetDrum player,drum,  -dif 
		Else
			if val = 0 Then	
			SetDrum player,drum,  0 ' reset the drum to abs. zero
			Else
			dif = 11 - cur
			dif = dif + val
			dif = dif * inc
			dif = dif-fval
			SetDrum player,drum,  -dif 
			end if

		end if
		reels(player, drum) = val
	 End Select

	end if
end if
End Sub

Dim EMMODE: EMMODE = 0
'Dim Score10,Score100,Score1000,Score10000,Score100000
Dim Score1000,Score10000,Score100000, ActivePLayer
Dim nplayer,playr,value,curscr,curplayr

' EMMODE = 1
'ex: UpdateVRReels 0-3, 0-3, 0-199999, n/a, n/a, n/a, n/a, n/a, n/a
' EMMODE = 0
'ex: UpdateVRReels 0-3, 0-3, n/a, 0-1,0-99999 ,0-9999, 0-999, 0-99, 0-9  

Sub UpdateVRReels (Player,nReels ,nScore, n100K, Score10000 ,Score1000,Score100,Score10,Score1)

' to-do find out if player is one or zero based, if 1 based subtract 1.
value =nScore'Score(Player)
	nplayer = Player -1

	curscr = value
	curplayr = nplayer


scores(0,1) = scores(0,0)
	scores(0,0) = 0
	scores(1,1) = scores(1,0)
	scores(1,0) = 0
	scores(2,1) = scores(2,0)
	scores(2,0) = 0
	scores(3,1) = scores(3,0)
	scores(3,0) = 0

	For  ix =0 to 6
		reels(0, ix) =0 
		reels(1, ix) =0 
		reels(2, ix) =0 
		reels(3, ix) =0 
	Next
	
	For  ix =0 to 4
		
	SetDrum ix, 1 , 0 
	SetDrum ix, 2 , 0  
	SetDrum ix, 3 , 0  
	SetDrum ix, 4 , 0  
	SetDrum ix, 5 , 0  
		
	Next 
	
	For playr =0 to nReels
	
		if EMMODE = 0 then
		If (ActivePLayer) = playr Then
		nplayer = playr

		SetReel nplayer, 1 , Score10000 : SetScore nplayer,0,Score10000
		SetReel nplayer, 2 , Score1000 : SetScore nplayer,1,Score1000
		SetReel nplayer, 3 , Score100 : SetScore nplayer,2,Score100
		SetReel nplayer, 4 , Score10 : SetScore nplayer,3,Score10 
		SetReel nplayer, 5 , 0 : SetScore nplayer,4,0 ' assumes ones position is always zero 

		else
		nplayer = playr
		value =scores(nplayer, 1) 
	
	
	' do ten thousands
		if(value >= 90000)  then:  SetReel nplayer, 1 , 9 : SetScore nplayer,0,9 : value = value - 90000: end if
		if(value >= 80000)  then:  SetReel nplayer, 1 , 8 : SetScore nplayer,0,8 : value = value - 80000: end if
		if(value >= 70000)  then:  SetReel nplayer, 1 , 7 : SetScore nplayer,0,7 : value = value - 70000: end if
		if(value >= 60000)  then:  SetReel nplayer, 1 , 6 : SetScore nplayer,0,6 : value = value - 60000: end if
		if(value >= 50000)  then:  SetReel nplayer, 1 , 5 : SetScore nplayer,0,5 : value = value - 50000: end if
		if(value >= 40000)  then:  SetReel nplayer, 1 , 4 : SetScore nplayer,0,4 : value = value - 40000: end if
		if(value >= 30000)  then:  SetReel nplayer, 1 , 3 : SetScore nplayer,0,3 : value = value - 30000: end if
		if(value >= 20000)  then:  SetReel nplayer, 1 , 2 : SetScore nplayer,0,2 : value = value - 20000: end if
		if(value >= 10000)  then:  SetReel nplayer, 1 , 1 : SetScore nplayer,0,1 : value = value - 10000: end if


	' do thousands
		if(value >= 9000)  then:  SetReel nplayer, 2 , 9 : SetScore nplayer,1,9 : value = value - 9000: end if
		if(value >= 8000)  then:  SetReel nplayer, 2 , 8 : SetScore nplayer,1,8 : value = value - 8000: end if
		if(value >= 7000)  then:  SetReel nplayer, 2 , 7 : SetScore nplayer,1,7 : value = value - 7000: end if
		if(value >= 6000)  then:  SetReel nplayer, 2 , 6 : SetScore nplayer,1,6 : value = value - 6000: end if
		if(value >= 5000)  then:  SetReel nplayer, 2 , 5 : SetScore nplayer,1,5 : value = value - 5000: end if
		if(value >= 4000)  then:  SetReel nplayer, 2 , 4 : SetScore nplayer,1,4 : value = value - 4000: end if
		if(value >= 3000)  then:  SetReel nplayer, 2 , 3 : SetScore nplayer,1,3 : value = value - 3000: end if
		if(value >= 2000)  then:  SetReel nplayer, 2 , 2 : SetScore nplayer,1,2 : value = value - 2000: end if
		if(value >= 1000)  then:  SetReel nplayer, 2 , 1 : SetScore nplayer,1,1 : value = value - 1000: end if

		'do hundreds
		
		if(value >= 900) then: SetReel nplayer, 3 , 9 : SetScore nplayer,2,9 : value = value - 900: end if
		if(value >= 800) then: SetReel nplayer, 3 , 8 : SetScore nplayer,2,8 : value = value - 800: end if
		if(value >= 700) then: SetReel nplayer, 3 , 7 : SetScore nplayer,2,7 : value = value - 700: end if
		if(value >= 600) then: SetReel nplayer, 3 , 6 : SetScore nplayer,2,6 : value = value - 600: end if
		if(value >= 500) then: SetReel nplayer, 3 , 5 : SetScore nplayer,2,5 : value = value - 500: end if
		if(value >= 400) then: SetReel nplayer, 3 , 4 : SetScore nplayer,2,4 : value = value - 400: end if
		if(value >= 300) then: SetReel nplayer, 3 , 3 : SetScore nplayer,2,3 : value = value - 300: end if
		if(value >= 200) then: SetReel nplayer, 3 , 2 : SetScore nplayer,2,2 : value = value - 200: end if
		if(value >= 100) then: SetReel nplayer, 3 , 1 : SetScore nplayer,2,1 : value = value - 100: end if

		'do tens
		if(value >= 90) then: SetReel nplayer, 4 , 9 : SetScore nplayer,3,9 : value = value - 90: end if
		if(value >= 80) then: SetReel nplayer, 4 , 8 : SetScore nplayer,3,8 : value = value - 80: end if
		if(value >= 70) then: SetReel nplayer, 4 , 7 : SetScore nplayer,3,7 : value = value - 70: end if
		if(value >= 60) then: SetReel nplayer, 4 , 6 : SetScore nplayer,3,6 : value = value - 60: end if
		if(value >= 50) then: SetReel nplayer, 4 , 5 : SetScore nplayer,3,5 : value = value - 50: end if
		if(value >= 40) then: SetReel nplayer, 4 , 4 : SetScore nplayer,3,4 : value = value - 40: end if
		if(value >= 30) then: SetReel nplayer, 4 , 3 : SetScore nplayer,3,3 : value = value - 30: end if
		if(value >= 20) then: SetReel nplayer, 4 , 2 : SetScore nplayer,3,2 : value = value - 20: end if
		if(value >= 10) then: SetReel nplayer, 4 , 1 : SetScore nplayer,3,1 : value = value - 10: end if

		'do ones
		if(value >= 9) then: SetReel nplayer, 5 , 9 : SetScore nplayer,4,9 : value = value - 9: end if
		if(value >= 8) then: SetReel nplayer, 5 , 8 : SetScore nplayer,4,8 : value = value - 8: end if
		if(value >= 7) then: SetReel nplayer, 5 , 7 : SetScore nplayer,4,7 : value = value - 7: end if
		if(value >= 6) then: SetReel nplayer, 5 , 6 : SetScore nplayer,4,6 : value = value - 6: end if
		if(value >= 5) then: SetReel nplayer, 5 , 5 : SetScore nplayer,4,5 : value = value - 5: end if
		if(value >= 4) then: SetReel nplayer, 5 , 4 : SetScore nplayer,4,4 : value = value - 4: end if
		if(value >= 3) then: SetReel nplayer, 5 , 3 : SetScore nplayer,4,3 : value = value - 3: end if
		if(value >= 2) then: SetReel nplayer, 5 , 2 : SetScore nplayer,4,2 : value = value - 2: end if
		if(value >= 1) then: SetReel nplayer, 5 , 1 : SetScore nplayer,4,1 : value = value - 1: end if

		end if
		Else
			If curplayr = playr Then
			nplayer = curplayr
			value = curscr
			else
			value =scores(playr, 1) ' store score
			nplayer = playr
			end if

		scores(playr, 0)  = 0 ' reset score

		if(value >= 100000) then

			'if nplayer = 0 then: FL100K1.visible = 1
			'if nplayer = 1 then: FL100K2.visible = 1
			'if nplayer = 2 then: FL100K3.visible = 1
			'if nplayer = 3 then: FL100K4.visible = 1

		value = value - 100000
		
		end if


	' do ten thousands
		if(value >= 90000)  then:  SetReel nplayer, 1 , 9 : SetScore nplayer,0,9 : value = value - 90000: end if
		if(value >= 80000)  then:  SetReel nplayer, 1 , 8 : SetScore nplayer,0,8 : value = value - 80000: end if
		if(value >= 70000)  then:  SetReel nplayer, 1 , 7 : SetScore nplayer,0,7 : value = value - 70000: end if
		if(value >= 60000)  then:  SetReel nplayer, 1 , 6 : SetScore nplayer,0,6 : value = value - 60000: end if
		if(value >= 50000)  then:  SetReel nplayer, 1 , 5 : SetScore nplayer,0,5 : value = value - 50000: end if
		if(value >= 40000)  then:  SetReel nplayer, 1 , 4 : SetScore nplayer,0,4 : value = value - 40000: end if
		if(value >= 30000)  then:  SetReel nplayer, 1 , 3 : SetScore nplayer,0,3 : value = value - 30000: end if
		if(value >= 20000)  then:  SetReel nplayer, 1 , 2 : SetScore nplayer,0,2 : value = value - 20000: end if
		if(value >= 10000)  then:  SetReel nplayer, 1 , 1 : SetScore nplayer,0,1 : value = value - 10000: end if


	' do thousands
		if(value >= 9000)  then:  SetReel nplayer, 2 , 9 : SetScore nplayer,1,9 : value = value - 9000: end if
		if(value >= 8000)  then:  SetReel nplayer, 2 , 8 : SetScore nplayer,1,8 : value = value - 8000: end if
		if(value >= 7000)  then:  SetReel nplayer, 2 , 7 : SetScore nplayer,1,7 : value = value - 7000: end if
		if(value >= 6000)  then:  SetReel nplayer, 2 , 6 : SetScore nplayer,1,6 : value = value - 6000: end if
		if(value >= 5000)  then:  SetReel nplayer, 2 , 5 : SetScore nplayer,1,5 : value = value - 5000: end if
		if(value >= 4000)  then:  SetReel nplayer, 2 , 4 : SetScore nplayer,1,4 : value = value - 4000: end if
		if(value >= 3000)  then:  SetReel nplayer, 2 , 3 : SetScore nplayer,1,3 : value = value - 3000: end if
		if(value >= 2000)  then:  SetReel nplayer, 2 , 2 : SetScore nplayer,1,2 : value = value - 2000: end if
		if(value >= 1000)  then:  SetReel nplayer, 2 , 1 : SetScore nplayer,1,1 : value = value - 1000: end if

		'do hundreds
		
		if(value >= 900) then: SetReel nplayer, 3 , 9 : SetScore nplayer,2,9 : value = value - 900: end if
		if(value >= 800) then: SetReel nplayer, 3 , 8 : SetScore nplayer,2,8 : value = value - 800: end if
		if(value >= 700) then: SetReel nplayer, 3 , 7 : SetScore nplayer,2,7 : value = value - 700: end if
		if(value >= 600) then: SetReel nplayer, 3 , 6 : SetScore nplayer,2,6 : value = value - 600: end if
		if(value >= 500) then: SetReel nplayer, 3 , 5 : SetScore nplayer,2,5 : value = value - 500: end if
		if(value >= 400) then: SetReel nplayer, 3 , 4 : SetScore nplayer,2,4 : value = value - 400: end if
		if(value >= 300) then: SetReel nplayer, 3 , 3 : SetScore nplayer,2,3 : value = value - 300: end if
		if(value >= 200) then: SetReel nplayer, 3 , 2 : SetScore nplayer,2,2 : value = value - 200: end if
		if(value >= 100) then: SetReel nplayer, 3 , 1 : SetScore nplayer,2,1 : value = value - 100: end if

		'do tens
		if(value >= 90) then: SetReel nplayer, 4 , 9 : SetScore nplayer,3,9 : value = value - 90: end if
		if(value >= 80) then: SetReel nplayer, 4 , 8 : SetScore nplayer,3,8 : value = value - 80: end if
		if(value >= 70) then: SetReel nplayer, 4 , 7 : SetScore nplayer,3,7 : value = value - 70: end if
		if(value >= 60) then: SetReel nplayer, 4 , 6 : SetScore nplayer,3,6 : value = value - 60: end if
		if(value >= 50) then: SetReel nplayer, 4 , 5 : SetScore nplayer,3,5 : value = value - 50: end if
		if(value >= 40) then: SetReel nplayer, 4 , 4 : SetScore nplayer,3,4 : value = value - 40: end if
		if(value >= 30) then: SetReel nplayer, 4 , 3 : SetScore nplayer,3,3 : value = value - 30: end if
		if(value >= 20) then: SetReel nplayer, 4 , 2 : SetScore nplayer,3,2 : value = value - 20: end if
		if(value >= 10) then: SetReel nplayer, 4 , 1 : SetScore nplayer,3,1 : value = value - 10: end if

		'do ones
		if(value >= 9) then: SetReel nplayer, 5 , 9 : SetScore nplayer,4,9 : value = value - 9: end if
		if(value >= 8) then: SetReel nplayer, 5 , 8 : SetScore nplayer,4,8 : value = value - 8: end if
		if(value >= 7) then: SetReel nplayer, 5 , 7 : SetScore nplayer,4,7 : value = value - 7: end if
		if(value >= 6) then: SetReel nplayer, 5 , 6 : SetScore nplayer,4,6 : value = value - 6: end if
		if(value >= 5) then: SetReel nplayer, 5 , 5 : SetScore nplayer,4,5 : value = value - 5: end if
		if(value >= 4) then: SetReel nplayer, 5 , 4 : SetScore nplayer,4,4 : value = value - 4: end if
		if(value >= 3) then: SetReel nplayer, 5 , 3 : SetScore nplayer,4,3 : value = value - 3: end if
		if(value >= 2) then: SetReel nplayer, 5 , 2 : SetScore nplayer,4,2 : value = value - 2: end if
		if(value >= 1) then: SetReel nplayer, 5 , 1 : SetScore nplayer,4,1 : value = value - 1: end if


		end if

	Next
End Sub 


Sub ClockTimer_Timer()

    ' ROB AND WALTER'S Digital Clock below**************************************
	dim n
    n=Hour(now) MOD 12
    if n = 0 then n = 12
	hour1.imagea="digit" & CStr(n \ 10)
    hour2.imagea="digit" & CStr(n mod 10)
	n=Minute(now)
	minute1.imagea="digit" & CStr(n \ 10)
    minute2.imagea="digit" & CStr(n mod 10)
	'n=Second(now)
	'second1.imagea="digit" & CStr(n \ 10)
    'second2.imagea="digit" & CStr(n mod 10)
End Sub