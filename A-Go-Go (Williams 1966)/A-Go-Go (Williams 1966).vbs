'Williams A-Go-Go
'
'By: PinPlayer
'Version: 1.2
'Date: February 13, 2020'
'Press F6 or hold LFlipper to set game options
'
'
'v1.1 - kicSpin.kick set to 183 degrees eject angle to avoid having ball loiter in bunpers and saucer.
'	  - Hexagon walB6615 side set to visible.
'     - Mechanical Tilt key added.  
'     - Turned on FilterMEchanicalPlunger.
'     - Four score tiers added that match points shown on instruction card.

'v1.2 - Modified Wall at top left lane and increased flipper strength to 1300 (was 1175) to
'		allow ball to roll through lane completely.

Option Explicit
Randomize

Const cGameName = "A-Go-Go"
Const cBallSize = 45
Const BallMass=2.75
Const cCaptiveBallSize = 35
Const cNudgeStrength = 2
Const cMaxNumberOfPlayers = 4
Dim ActiveBall
Dim Amount
Dim B2SOn
'Dim BallMass
Dim BallReady			
Dim BallInPlay			
Dim BallsPerCredit
Dim BallShadows	
Dim BonusValue
Dim CreditLock				
Dim Credits				
Dim CurrentPlayer
Dim EnableBallControl
Dim HighScore
Dim i
Dim FlipperShadows
Dim GameStateIsActive
Dim HighScoreAward
Dim KickerWithBall
Dim MatchReelPosition
Dim MatchValue
Dim MaxNumberOfPlayers
Dim NumberOfPlayers
Dim obj
Dim objBonusLight
Dim OptionMenuActive
Dim OptionPosition
Dim Pulses
Dim ScoreBonusValue
Dim ScoreBonusType
Dim ScoreMatchIsActive
Dim TiltCount
Dim Tilted
Dim TiltSensitivity
Dim Controller
Dim BWR
Dim bill

dim raceRflag
dim raceAflag
dim raceCflag
dim raceEflag
dim x

'BallMass = (cBallSize^3)/100000
TiltSensitivity = 3
KickerWithBall = 7
OptionMenuActive = False
OptionPosition = 0

'Array Sizes must equal cMaxNumberOfPlayers
Dim PlayerRealScore(4)
Dim PlayerScore(4)

Dim ReplayScores(3)
Dim PlayerReplayTier(3)
PlayerReplayTier(0) = 0
PlayerReplayTier(1) = 0
PlayerReplayTier(2) = 0
PlayerReplayTier(3) = 0

ReplayScores(0) = 39 ' Extra Ball when player exceeds 3900 pts
ReplayScores(1) = 51 ' Extra Ball when player exceeds 5100 pts
ReplayScores(2) = 60 ' Extra Ball when player exceeds 7800 pts
ReplayScores(3) = 73 ' Extra Ball when player exceeds 7800 pts


'******************************************
'***		    DOF Options   			***
'******************************************

Const DOFStateActivate =      0
Const DOFStateDeactivate =    1
Const DOFStatePulse =         2

Const DOFLeftFlipper =        101
Const DOFRightFlipper =       102
Const DOFLeftSling =          103
Const DOFRightSling =         104
Const DOFBumperBackLeft =     105
Const DOFBumperBackCenter =   112
Const DOFBumperBackRight =    106
Const DOFBumperMiddleCenter = 110 ' Bonus Advance 
Const DOFChime1 =             141 ' Small Bell in backbox that rings on 1 Points
Const DOFChime2 =             142 ' Large Bell in back of cabinet that rings on 10 or 100 Points
Const DOFChime3 =             143 ' not applicable
Const DOFChime4 =             144 ' not applicable
Const DOFKnock =              111
Const DOFKickerTop =          121 ' Bonus Spin Kickout Cup
Const DOFKickerBottom =       120 ' Left Outlane Kicker 


'******************************************
'***			Table Options 			***
'******************************************


Ballshadows = True
FlipperShadows = True

'******************************************
'***		Table Initialization 		***
'******************************************

On Error Resume Next
ExecuteGlobal GetTextFile("Controller.vbs")
If Err Then MsgBox "Unable to open Controller.vbs. Ensure that it is in the scripts folder."
On Error Goto 0


Sub Table1_Init()
	
	If Table1.ShowDT = False Then LoadEM

	'*** Gameplay Options ***
	' Set default values
	BallsPerCredit = 5 ' Balls per credit (3 or 5)
	ScoreMatchIsActive = True ' Match last 2 digits of score to win credit? (True or False)
	MatchReelPosition = int(10*rnd(1))
	MatchReel_Update

	WheelSurface.IsDropped = True
	Dim RouletteBall
	KickerWithBall = int(11*rnd(1))
	Dim kicRouletteStart
	For each obj in colBonusKickers
		obj.DestroyBall
	Next
	Set kicRouletteStart = colBonusKickers.Item(KickerWithBall)
	Set RouletteBall = kicRouletteStart.CreateSizedBallWithMass(20,1.0*((22.5*2)^3)/125000)
	


	'Load previously stored settings
	LoadSettings

	flaInstBallsPerCredit.ImageA = "Inst_" & BallsPerCredit

	if HSA1="" then HSA1=23
	if HSA2="" then HSA2=10
	if HSA3="" then HSA3=18
	UpdatePostIt

	if B2SOn then 
		Controller.B2SSetCredits Credits
		For each obj in colBallInPlay : obj.visible = False : Next
		For each obj in colScoreReel :	obj.visible = False : Next
		For each obj in colCanPlay :	obj.visible = False : Next
		For each obj in colPlayerUp :	obj.visible = False : Next
		For each obj in colScoreReel :	obj.visible = False : Next
		For each obj in colMatchDT :	obj.visible = False : Next
		DT_CreditReel.visible = False
		ligGameOver.visible = False
		ligShootAgainDT.visible = False
		ligTiltDT.visible = False
	End If

	If Table1.ShowDT = True Then
		For each obj in colBallInPlay : obj.visible = False : Next
		For each obj in colScoreReel :	obj.visible = True : Next
		For each obj in colCanPlay :	obj.visible = True : Next
		For each obj in colPlayerUp :	obj.visible = True : Next
		For each obj in colScoreReel :	obj.visible = True : Next
		For each obj in colMatchDT :	obj.visible = False : Next
		DT_CreditReel.visible = True
		ligGameOver.visible = True
		ligShootAgainDT.visible = False
		ligTiltDT.visible = False
		DT_CreditReel.SetValue Credits
	End If

	GameStateIsActive = False


	BallShadowUpdate.enabled = Ballshadows
	priFlipperShadowLeft.Visible = FlipperShadows
	priFlipperShadowRight.Visible = FlipperShadows
	priFlipperShadowCenterLeft.Visible = FlipperShadows
	priFlipperShadowCenterRight.Visible = FlipperShadows
	BWR=0
	CreditLock = False
	NumberOfPlayers = 0
	CurrentPlayer = 1
	GameStateIsActive = False
	BallInPlay = 0

	if B2SOn then
		Controller.B2SSetGameOver 35, 1
		Controller.B2SSetBallInPlay 32, BallInPlay
		Controller.B2SSetCanPlay 1
		Controller.B2SSetScoreRolloverPlayer1 1
		Controller.B2SSetScoreRolloverPlayer2 1
		Controller.B2SSetScoreRolloverPlayer3 1
		Controller.B2SSetScoreRolloverPlayer4 1
	end if
   
	timTableLights.Enabled = True
	PlaySoundAtVol "fx_lightsrelay", kicDrain, 0.75

	PlaySoundAtVol "fx_table_startup", kicDrain, 1

End Sub


Sub Table1_Exit()
	SaveSettings
	If B2SOn then Controller.Stop
End Sub

Sub TimerR_timer
	if B2SOn then
		if raceRflag=1 then
			Controller.B2SSetData 15,0
			raceRflag=0
		else
			Controller.B2SSetData 15,1
			raceRflag=1
		end if
	end if
end sub

Sub TimerA_timer
	if B2SOn then
		if raceAflag=1 then
			Controller.B2SSetData 12,0
			raceAflag=0
		else
			Controller.B2SSetData 12,1
			raceAflag=1
		end if
	end if
end sub

Sub TimerC_timer
	if B2SOn then
		if raceCflag=1 then
			Controller.B2SSetData 13,0
			raceCflag=0
		else
			Controller.B2SSetData 13,1
			raceCflag=1
		end if
	end if
end sub

Sub TimerE_timer
	if B2SOn then
		if raceEflag=1 then
			Controller.B2SSetData 14,0
			raceEflag=0
		else
			Controller.B2SSetData 14,1
			raceEflag=1
		end if
	end if
end sub

sub timTableLights_Timer
	For i = 1 to colTableLights.Count
		colTableLights(i - 1).State = LightStateOn
	Next
	For i = 1 to colPlasticsGlow.Count
		colPlasticsGlow(i - 1).State = LightStateOn
	Next
	For i = 1 to colTableLightsGlow.Count
		colTableLightsGlow(i - 1).State = LightStateOn
	Next	
	timTableLights.Enabled = False
end Sub



'******************************************
'***		Keyboard Handler			***
'******************************************

Sub table1_KeyDown(ByVal keycode)

	If OptionMenuActive Then
		Options_Update(keycode)
		Exit Sub
	End If

	If HSEnterMode Then
		HighScoreProcessKey(keycode)
		Exit Sub
	End If

	Select Case keycode
		Case PlungerKey 
			plgPlunger.PullBack
		
		Case LeftFlipperKey
			If Tilted = False and GameStateIsActive = True  then
				flpCenterRight.RotateToEnd
				flpLeft.RotateToEnd
				PlaySoundAtVolLoops "fx_flipper_buzz", flpLeft, 0.5, -1
				PlaySoundAt "LeftflipperupH", flpLeft
				If B2SOn Then DOF DOFLeftFlipper, DOFStatePulse
			Elseif GameStateIsActive = False and OptionMenuActive = False then
				timOptionsMenu.Enabled = True
			End if
		
		Case RightFlipperKey  
			If Tilted = False and GameStateIsActive = True  then
				flpCenterLeft.RotateToEnd
				flpRight.RotateToEnd
				PlaySoundAtVolLoops "fx_flipper_buzz", flpRight, 0.5, -1
				PlaySoundAt "RightflipperupH", flpRight
				If B2SOn Then DOF DOFRightFlipper, DOFStatePulse
			End If
		Case LeftTiltKey 
			Nudge 90, cNudgeStrength
			CheckTilt
		Case RightTiltKey 
			Nudge 270, cNudgeStrength
			CheckTilt
		Case CenterTiltKey 
			Nudge 0, cNudgeStrength * 1.3
			CheckTilt
		Case StartGameKey 'User pressed 1
			If CreditLock = False and Credits > 0 and NumberOfPlayers <= cMaxNumberOfPlayers and HSEnterMode = False and BallInPlay <=1 Then
				PlaysoundAt "fx_solenoid", kicDrain
				SubtractCredit
				NumberOfPlayers = NumberOfPlayers + 1	
				if B2SOn then Controller.B2SSetCanPlay 31, NumberOfPlayers
				if B2SOn then Controller.B2SSetMatch 0
				if Table1.ShowDT = True Then
					For each obj in colCanPlay
						obj.State = LightStateOff
					Next
					colCanPlay.Item(NumberOfPlayers -1).State = LightstateOn
				End If
				StartGame
			End If 
		Case AddCreditKey 'User pressed 5
			PlaySoundAt "fx_coin_drop", kicDrain
			AddCredit
		Case MechanicalTilt
			If GameStateIsActive = True Then
				Tilted = True
				TiltedMode (True)
			End If
		Case 64 'User pressed F6
			'*** Options Menu ***
			If GameStateIsActive = False then
				timOptionsMenu.Enabled = True
			End If 
		Case 65 'User pressed F7
			BonusWheel_Spin
	End Select


End Sub

Sub Table1_KeyUp(ByVal keycode)
	Select Case keycode
		Case PlungerKey
			plgPlunger.Fire
			If BallReady = True then
				PlaySoundAt "fx_plunger_release_ball", plgPlunger
			Else
				PlaySoundAt "fx_plunger_release_noball", plgPlunger
			End If
		Case LeftFlipperKey
			If Tilted = False and GameStateIsActive = True Then
				flpCenterRight.RotateToStart
				flpLeft.RotateToStart
				StopSound "fx_flipper_buzz"
				PlaySoundAt "LeftflipperdownH", flpCenterRight
			End If
		Case RightFlipperKey
			If Tilted = False and GameStateIsActive = True Then
				flpCenterLeft.RotateToStart
				flpRight.RotateToStart
				StopSound "fx_flipper_buzz"
				PlaySoundAt "RightflipperdownH", flpCenterLeft
			End If
	End Select
End Sub



'******************************************
'***		Game Options                ***
'******************************************
Dim LastOption: LastOption = 4

Sub Options_Show()
	PlaySound "fx_agogo"
	OptionPosition = 0	
	debug.print "Showing Options Menu"
	OptionBackground.visible = True
	OptionDisplay.visible = True
	OptionDisplay.image = "Option_" & BallsPerCredit & "Balls"
End Sub


Sub Options_Hide()
	OptionBackground.visible = False
	OptionDisplay.visible = False
End Sub

Sub Options_Update(ByVal keycode)
	Select Case keycode
		Case LeftFlipperKey
			Playsound "fx_ball_drop5"
			OptionPosition = OptionPosition + 1
			if OptionPosition = LastOption Then OptionPosition = 0
			if OptionPosition = 0 Then OptionDisplay.image = "Option_" & BallsPerCredit & "Balls"
			if OptionPosition = 1 Then OptionDisplay.image = "Option_BS" & IIF(BallShadows,"On","Off")
			if OptionPosition = 2 Then OptionDisplay.image = "Option_FS" & IIF(FlipperShadows,"On","Off")
			if OptionPosition = 3 Then OptionDisplay.image = "Option_Exit"
			Playsound "fx_balldrop5"

		Case RightFlipperKey
			Playsound "fx_leafswitch_hit"
			Select Case OptionPosition
				Case 0
					debug.print "Changing BPC"				
					BallsPerCredit =  BallsPerCredit + ((BallsPerCredit - 4) * -2)
					OptionDisplay.image = "Option_" & BallsPerCredit & "Balls"
					flaInstBallsPerCredit.ImageA = "Inst_" & BallsPerCredit
				Case 1
					debug.print "Changing Ball Shadows"				
					BallShadows =  not BallShadows
					OptionDisplay.image = "Option_BS" & IIF(BallShadows,"On","Off")
				Case 2
					debug.print "Changing Flipper Shadows"				
					FlipperShadows =  not FlipperShadows
					OptionDisplay.image = "Option_FS" & IIF(FlipperShadows,"On","Off")
				Case 3
					debug.print "Exiting Menu"
					Call Options_Hide
					OptionMenuActive = False
					SaveSettings
					timOptionsMenu.Enabled = False
					Table1_Init
			End Select
	End Select
	
End Sub


Sub timOptionsMenu_Timer()
	OptionMenuActive = True	
	timOptionsMenu.Enabled = False
	Options_Show
End Sub

'******************************************
'***		Bonus Wheel Handler	    	***
'******************************************
Dim KickForce, NewAngle

Sub BonusWheel_Spin()
	BWR=1
	timBonusSpinner.Interval = 10
	Randomize
	i = 0
	KickForce = 100 + int(300 * rnd(1))
	timBonusSpinner.Enabled = True
	WheelSurface.IsDropped = False
'	For each obj in colBonusKickers
'		obj.Enabled = False
'	Next
	colBonusKickers.Item(KickerWithBall).Kick colBonusKickers.Item(KickerWithBall).Orientation - 10, KickForce, 0.19
	PlaySound "fx_bonuswheel", 1,1
End Sub

Sub timBonusSpinner_Timer
	Randomize
	i = i + 1
	priBonusWheel.objRotz = priBonusWheel.objRotz + 5
	priBonusWheelContacts.objrotz = priBonusWheel.objRotz
	if priBonusWheel.objRotz > 360 then priBonusWheel.objRotz = priBonusWheel.objRotz - 360
	priBonusScore.objrotz = priBonusWheel.objRotz
	if i > 315 then 
		timBonusSpinner.Enabled = False
		BWR=0
		NewAngle = 30 * int(0.5 + priBonusWheel.objRotz/30)
		priBonusWheel.objRotz = NewAngle
		priBonusScore.objrotz = NewAngle
		priBonusWheelContacts.objrotz = NewAngle
'		For each obj in colBonusKickers
'			obj.Enabled = True
'		Next
		WheelSurface.IsDropped = True
		StopSound "fx_bonuswheel"
	end if
End Sub

Sub colBonusKickers_Hit(Index)
	PlaySoundAtVol "fx_saucer_hit", priBonusScore, 0.5
	KickerWithBall = Index
	debug.print "Black slice nearest kicker " & NewAngle/30 
	debug.print "Landed on kicker " & KickerWithBall
	debug.print "Delta: " & abs(NewAngle/30 - KickerWithBall)
	If GameStateIsActive Then	
		Select Case abs(NewAngle/30 - KickerWithBall)
			Case 0
				debug.print "Black: 200 Bonus + Extra Ball"
				AddToScore 100, 2
				IF B2SOn then Controller.B2SSetShootAgain 36, 1
				If Table1.ShowDT Then ligShootAgainDT.visible = True
				ligShootAgainPF.State = LightStateOn
			Case 1,3,5,7,9,11
				debug.print "Green: 50 Points"
				AddToScore 10, 5
			Case 2,6,10
				debug.print "White: Collect Bonus"
				CollectBonus()
			Case 4,8
				debug.print "Yellow: 500 Points"
				AddToScore 100, 5
		End Select
'		kicSpin_Timer 'Eject ball from cup
		KST.enabled= 1
	End If
End Sub


'******************************************
'***		StartGame Handler			***
'******************************************

Sub StartGame

	Bonus_Advance

	If GameStateIsActive = False Then 
		Playsound"BallyStartButtonPlayer1" 'Chr
		For i = 0 to 1
			PlayerScore(i) = 0
			PlayerRealScore(i) = 0
			if B2SOn then Controller.B2SSetScorePlayer i, 0
			if Table1.ShowDT then ligGameOver.Visible = False
			PlaySoundAt "fx_ball_reel", kicDrain
			Next
		CurrentPlayer = 1
		BallInPlay = 1
		if B2SOn then 
			Controller.B2SSetGameOver 35, 0
			Controller.B2SSetPlayerUp 30, CurrentPlayer
			Controller.B2SSetBallInPlay BallInPlay
		End If 
		If Table1.ShowDT = True Then
			ligGameOver.State = LightStateOff
			ligPlayer1.State = LightStateOn
			for each obj in colBallInPlay: obj.visible = False : Next
			colBallInPlay.Item(BallInPlay - 1).visible = True
		End If
		
		For i = 1 to cMaxNumberOfPlayers
			PlayerScore(i) = 0
			PlayerRealScore(i) = 0
			if B2SOn then Controller.B2SSetScorePlayer i, 0
			If Table1.ShowDT = True Then
				DT_ScoreReel1.SetValue 0
				DT_ScoreReel2.SetValue 0
			End If
		Next

		GameStateIsActive = True
		BED.enabled=1
		If B2SOn Then DOF DOFKickerBottom, DOFStatePulse

	End If
End Sub

Sub BED_Timer()
	BED.enabled=0
		Set ActiveBall = kicLaunch.CreateBall
		kicLaunch.Kick 67, 20
		PlaySoundAt "Right_KickerHoleKick", kicLaunch
End Sub


'******************************************
'***		Credit Handler				***
'******************************************

Sub AddCredit
	Credits = Credits + 1
	If Credits > 25 then
		Credits = 25
	End If
	if B2SOn then Controller.B2SSetCredits Credits
	if Table1.ShowDT = True Then DT_CreditReel.SetValue Credits
End Sub

Sub SubtractCredit
	Credits = Credits - 1
	if B2SOn then Controller.B2SSetCredits Credits
	if Table1.ShowDT = True Then DT_CreditReel.SetValue Credits
End Sub

'******************************************
'***		Scoring Handler				***
'******************************************
Dim ScoreRepeat: ScoreRepeat = 0
Dim BellSound


Sub AddToScore(Amount, Pulses)

	
	Select Case Amount
		Case 1
			BellSound = 1
		Case 10, 100
			BellSound = 2
	End Select

	Select Case Amount
		Case 1
			bill = 1
		Case 10
			bill = 10
		Case 100
			bill = 100
	End Select


	For i = 1 to Pulses
		timChimeBox.Enabled = True
		timChimeBox.Uservalue = Pulses 'Chr
'		PlayerRealScore(CurrentPlayer - 1) = PlayerRealScore(CurrentPlayer - 1) + Amount
'		PlayerScore(CurrentPlayer - 1) = PlayerScore(CurrentPlayer - 1) + Amount
'		If PlayerScore(CurrentPlayer - 1) > 9999 then
'			PlayerScore(CurrentPlayer - 1) = right(PlayerRealScore(CurrentPlayer - 1),4)
'		End If	
'		If B2SOn then Controller.B2SSetScorePlayer CurrentPlayer, PlayerScore(CurrentPlayer - 1)
'		If Table1.ShowDT Then colScoreReel.Item(CurrentPlayer - 1).SetValue(PlayerScore(CurrentPlayer - 1))
		If Amount = 1 and Pulses = 1 Then 'Advance Match Reel when 1 point is added to score
			MatchReelPosition = MatchReelPosition + 1
'			MatchReel_Update	
		End If
		If Amount = 100 and Pulses = 5 Then 'Advance Match Reel when 500 points is added to score
			MatchReelPosition = MatchReelPosition + 1
'			MatchReel_Update
		End If

	Next

	ExtraBall_Check

End Sub



Sub timChimeBox_Timer()
	debug.print "Ring # " & timChimeBox.Uservalue & " Bell Sound = " & BellSound
    timChimeBox.Uservalue = timChimeBox.Uservalue - 1
    If BellSound = 1 Then playsound SoundFXDOF("fx_chime_small",141,DOFPulse,DOFChimes)
    If BellSound = 2 Then playsound SoundFXDOF("fx_chime_large",142,DOFPulse,DOFChimes)
'*********** vereinzeltes Zählen ****************************************************
		PlayerRealScore(CurrentPlayer - 1) = PlayerRealScore(CurrentPlayer - 1) + bill
		PlayerScore(CurrentPlayer - 1) = PlayerScore(CurrentPlayer - 1) + bill
		If PlayerScore(CurrentPlayer - 1) > 9999 then
			PlayerScore(CurrentPlayer - 1) = right(PlayerRealScore(CurrentPlayer - 1),4)
		End If	
		If B2SOn then Controller.B2SSetScorePlayer CurrentPlayer, PlayerScore(CurrentPlayer - 1)
		If Table1.ShowDT Then colScoreReel.Item(CurrentPlayer - 1).SetValue(PlayerScore(CurrentPlayer - 1))
'*********** vereinzeltes Zählen ****************************************************
    If timChimeBox.Uservalue = 0 Then 
		timChimeBox.Enabled = False
    end If
end sub 


'******************************************
'***	 Score Awards Ball Handler      ***
'******************************************
Sub ExtraBall_Check
	'Check if current player score crossed threshhold to win extra ball
debug.print "Check Player " & CurrentPlayer & " hundreds score - " &left(right(PlayerScore(CurrentPlayer - 1),4),2)
debug.print "Check Player " & CurrentPlayer & " replay tier - " & PlayerReplayTier(CurrentPlayer -1) & " ["&ReplayScores(PlayerReplayTier(CurrentPlayer -1) )&"]"

	If CInt(left(right(PlayerScore(CurrentPlayer - 1),4),2)) = CInt(ReplayScores(PlayerReplayTier(CurrentPlayer -1))) Then
		debug.print "Player " & CurrentPlayer & " wins extra ball" 
		IF B2SOn then Controller.B2SSetShootAgain 36, 1
		If Table1.ShowDT Then ligShootAgainDT.visible = True
		ligShootAgainPF.State = LightStateOn	
		PlaysoundAt "fx_knocker", kicSpin
		PlayerReplayTier(CurrentPlayer -1) = PlayerReplayTier(CurrentPlayer -1) + 1
		If PlayerReplayTier(CurrentPlayer -1) = 4 then PlayerReplayTier(CurrentPlayer -1) =0
		debug.print "Player " & CurrentPlayer & " replay tier set to " & PlayerReplayTier(CurrentPlayer -1)
	End If
End Sub


'******************************************
'***		Saucer Handler  			***
'******************************************

Sub kicSpin_Hit()
	PlaysoundAt "fx_hole_in", kicSpin
	If Tilted = True Then
		kicSpin.TimerEnabled = True
		Exit Sub
	End If
	BonusWheel_Spin
End Sub

Sub kicSpin_Timer()
	PlaysoundAt "fx_hole_out", kicSpin
	kicSpin.Kick 183, 16,0.3
	kicSpin.TimerEnabled = False
End Sub

Sub KST_Timer()
	Randomize
	KST.enabled= 0
	PlaysoundAt "fx_hole_out", kicSpin
	kicSpin.Kick (int(rnd(1)*7)+175), 16,0.3
	kicSpin.TimerEnabled = False
End Sub

'******************************************
'***		Match Reel Handler			***
'******************************************
' A non-sequential, ten position reel is advanced 
' whenever an add one point switch has been triggered.
Sub MatchReel_Update()

	If MatchReelPosition = 0 then MatchReelPosition = 10
	If MatchReelPosition > 10 then MatchReelPosition = 1

	Select Case MatchReelPosition
		Case 1
			MatchValue = 6
		Case 2
			MatchValue = 1
		Case 3
			MatchValue = 7
		Case 4
			MatchValue = 3
		Case 5
			MatchValue = 8
		Case 6
			MatchValue = 2
		Case 7
			MatchValue = 5
		Case 8
			MatchValue = 9
		Case 9
			MatchValue = 4
		Case 10
			MatchValue = 10
	End select
	if B2SOn then Controller.B2SSetMatch MatchValue

	if Table1.ShowDT then
		For each obj in colMatchDT :	obj.visible = False : Next
		colMatchDT(MatchValue - 1).visible = True
	End If
	debug.print "Current Match Value is: " & MatchValue

End Sub



'******************************************
'***		Bonus Lights Handler	   	***
'******************************************

Sub Bonus_Advance()
	BonusValue = BonusValue + 100
	If BonusValue > 1900 then
		BonusValue = 1900
	End If
	debug.print "BonusValue = " & BonusValue
	SetBonusLights
End Sub

Sub CollectBonus()
	debug.print "Collect Bonus Started" 
	debug.print "BonusValue = " & BonusValue
	timCollectBonus.Enabled = True
	SetBonusLights
	debug.print "Collect Bonus Ended" 
End Sub

Sub timCollectBonus_Timer()
	Call AddToScore(100,1)
	BonusValue = BonusValue - 100
	SetBonusLights
	debug.print " - remaining = " & BonusValue
	If BonusValue <= 0 then
		BonusValue = 100
		SetBonusLights
		timCollectBonus.Enabled = False	
	End If
End Sub

Sub Bonus_Reset()
	BonusValue=100
	SetBonuslights
End Sub

Sub SetBonusLights()
	for each obj in colAdvanceBonusLights
		obj.State = LightStateOff
	next
	debug.print " - Setting Bonus Lights to " & BonusValue 
	Select Case BonusValue
		Case 100, 1100
			ligBonus100.State = LightstateOn: If BonusValue > 1000 then ligBonus1000.State = LightstateOn
		Case 200, 1200
			ligBonus200.State = LightstateOn: If BonusValue > 1000 then ligBonus1000.State = LightstateOn
		Case 300, 1300
			ligBonus300.State = LightstateOn: If BonusValue > 1000 then ligBonus1000.State = LightstateOn
		Case 400, 1400
			ligBonus400.State = LightstateOn: If BonusValue > 1000 then ligBonus1000.State = LightstateOn
		Case 500, 1500
			ligBonus500.State = LightstateOn: If BonusValue > 1000 then ligBonus1000.State = LightstateOn
		Case 600, 1600
			ligBonus600.State = LightstateOn: If BonusValue > 1000 then ligBonus1000.State = LightstateOn
		Case 700, 1700
			ligBonus700.State = LightstateOn: If BonusValue > 1000 then ligBonus1000.State = LightstateOn
		Case 800, 1800
			ligBonus800.State = LightstateOn: If BonusValue > 1000 then ligBonus1000.State = LightstateOn
		Case 900, 1900
			ligBonus900.State = LightstateOn: If BonusValue > 1000 then ligBonus1000.State = LightstateOn
		Case 1000
			ligBonus1000.State = LightstateOn
	End Select
End Sub



'******************************************
'***		When Lit Lights Handler		***
'******************************************

Sub InvertWhenLitLights()
	ligSideLaneL.State = abs(ligSideLaneL.State - 1)
	ligSideLaneR.State = abs(ligSideLaneR.State -1 )
	ligSpinWhenLitL.State = abs(ligSpinWhenLitL.State - 1)
	ligSpinWhenLitR.State = abs(ligSpinWhenLitR.State - 1)
End Sub

Sub WhenLitLights(OnOff)
	ligWhenLitLeft.State = OnOff
	ligWhenLitRight.State = OnOff
End Sub

Sub PopBumperLights(OnOff)
	ligBumperLeft.State = OnOff
	ligBumperRight.State = OnOff
End Sub



'******************************************
'***		Fixed Targets Handler		***
'******************************************


Sub trgBonus_Advance_Hit()
	If Tilted = True Then Exit Sub
	Bonus_Advance     'Advance Bonus Lights
	Call AddToScore(100,1)
End Sub

Sub target1_Hit()
	If Tilted = True Then Exit Sub
	WhenLitLights(1)
	PopBumperLights(1) 'Turn On Yellow Pop Bumpers
	Call AddToScore(100,1)
End Sub

Sub target2_Hit()
	If Tilted = True Then Exit Sub
	WhenLitLights(1)
	PopBumperLights(1) 'Turn On Yellow Pop Bumpers
	Call AddToScore(100,1)
End Sub


Sub target3_Hit()
	If Tilted = True Then Exit Sub
	Bonus_Advance     'Advance Bonus Lights
	Call AddToScore(100,1)
End Sub

Sub target4_Hit()
	If Tilted = True Then Exit Sub
	Bonus_Advance     'Advance Bonus Lights
	Call AddToScore(100,1)
End Sub

Sub trgOulaneLR_Hit()
	If Tilted = True Then Exit Sub
	Call AddToScore(10,1)	
End Sub

Sub trgOulaneRL_Hit()
	If Tilted = True Then Exit Sub
	Call AddToScore(10,1)	
End Sub

Sub trgOulaneLL_Hit()
	If Tilted = True Then Exit Sub
	Call AddToScore(100,1)
	If ligSpinWhenLitL.State = LightstateOn Then BonusWheel_Spin
End Sub

Sub trgOulaneLC_Hit()
	If Tilted = True Then Exit Sub
	Call AddToScore(100,1)
End Sub

Sub trgOulaneRC_Hit()
	If Tilted = True Then Exit Sub
	Call AddToScore(100,1)
End Sub

Sub trgOulaneRR_Hit()
	If Tilted = True Then Exit Sub
	Call AddToScore(100,1)
	If ligSpinWhenLitR.State = LightstateOn Then BonusWheel_Spin
End Sub

Sub trgSideLaneR_Hit()
	If Tilted = True Then Exit Sub
	If ligSideLaneR.State = LightStateOn Then
		Call AddToScore(100,1)
	Else
		Call AddToScore(10,5)
	End If
End Sub

Sub trgTenLeft_Hit()
	If Tilted = True Then Exit Sub
	If ligWhenLitLeft.State = LightStateOn Then
		Call AddToScore(10,1)
	Else
		Call AddToScore(1,1)
	End If
End Sub

Sub trgTenRight_Hit()
	If Tilted = True Then Exit Sub
	If ligWhenLitRight.State = LightStateOn Then
		Call AddToScore(10,1)
	Else
		Call AddToScore(1,1)
	End If
End Sub

Sub trgSideLaneUL_Hit()
	If Tilted = True Then Exit Sub
	If ligSideLaneL.State = LightStateOn Then
		Call AddToScore(100,5)
	Else
		Call AddToScore(100,1)
	End If
	
End Sub

Sub trgAdvanceBonus_Hit()
	AddToScore 100, 1
	Bonus_Advance
End Sub

'******************************************
'***		Pop Bumpers Handler			***
'******************************************

'====================Top Left Bumper=======================
Sub bmpBumperTopLeft_Hit()
	If Tilted = True Then Exit Sub
	InvertWhenLitLights()
	If ligBumperLeft.State = LightStateOn Then
		Call AddToScore(10,1)
	Else
		Call AddToScore(1,1)
	End If
End Sub

'====================Top Right Bumper======================
Sub bmpBumperTopRight_Hit()
	If Tilted = True Then Exit Sub
	InvertWhenLitLights()
	If ligBumperRight.State = LightStateOn Then
		Call AddToScore(10,1)
	Else
		Call AddToScore(1,1)
	End If
End Sub

'====================Center Bumper=========================
Sub bmpBumperCenter_Hit()
	If Tilted = True Then Exit Sub
	InvertWhenLitLights()
	Call AddToScore(1,1)
End Sub




'******************************************
'*******  Flipper Primitive Handler     ***
'******************************************

Sub FlipperPrimitives_Timer
	priLLFlip.ObjRotZ = flpLeft.CurrentAngle-90
	priLRFlip.ObjRotZ = flpCenterLeft.CurrentAngle+90
	priRLFlip.ObjRotZ = flpCenterRight.CurrentAngle-90
	priRRFlip.ObjRotZ = flpRight.CurrentAngle+90
End Sub

'******************************************
'********   Slingshot Handlers          ***
'******************************************
Dim intAnimationStepRight
Dim intAnimationStepLeft

Sub walSlingshotLowerLeft_Slingshot
	If Tilted = True Then Exit Sub
	If B2SOn Then DOF DOFLeftSling, DOFStatePulse
	rubSlinghotLeft.Visible = 0
	rubSlinghotLeft2.Visible = 1
	priSlingShotKickerLeft.TransZ = -20
	intAnimationStepLeft = 0
	walSlingshotLowerLeft.TimerEnabled = 1
	Call AddToScore(1,1)
	InvertWhenLitLights()
End Sub

Sub walSlingshotLowerLeft_Timer
    Select Case intAnimationStepLeft
        Case 3:
			rubSlinghotLeft2.Visible = 0
			rubSlinghotLeft1.Visible = 1
			priSlingShotKickerLeft.TransZ = -10
        Case 4
			rubSlinghotLeft1.Visible = 0
			rubSlinghotLeft.Visible = 1
			priSlingShotKickerLeft.TransZ = 0
			walSlingshotLowerLeft.TimerEnabled = 0
    End Select
    intAnimationStepLeft = intAnimationStepLeft + 1
End Sub


Sub walSlingshotLowerRight_Slingshot
	If Tilted = True Then Exit Sub
	If B2SOn Then DOF DOFLeftSling, DOFStatePulse
	rubSlinghotRight.Visible = 0
	rubSlinghotRight2.Visible = 1
	priSlingShotKickerRight.TransZ = -20
	intAnimationStepRight = 0
	walSlingshotLowerRight.TimerEnabled = 1
	Call AddToScore(1,1)
	InvertWhenLitLights()
End Sub

Sub walSlingshotLowerRight_Timer
    Select Case intAnimationStepRight
        Case 3
			rubSlinghotRight2.Visible = 0
			rubSlinghotRight1.Visible = 1
			priSlingShotKickerRight.TransZ = -10
        Case 4
			rubSlinghotRight1.Visible = 0
			rubSlinghotRight.Visible = 1
			priSlingShotKickerRight.TransZ = 0
			walSlingshotLowerRight.TimerEnabled = 0
    End Select
    intAnimationStepRight = intAnimationStepRight + 1
End Sub


Sub walSlingshotUpperLeft_Slingshot
	If Tilted = True Then Exit Sub
	Call AddToScore(1,1)
	InvertWhenLitLights()
End Sub


Sub walSlingshotMiddleLeft_Slingshot
	If Tilted = True Then Exit Sub
	Call AddToScore(1,1)
	InvertWhenLitLights()
End Sub

Sub walSlingshotMiddleRight_Slingshot
	If Tilted = True Then Exit Sub
	Call AddToScore(1,1)
	InvertWhenLitLights()
End Sub


'******************************************
'***		Drain Handler				***
'******************************************
Dim BallOut
Dim BonusMultiplier


Sub kicDrain_Timer()
	Tilted = False
	kicDrain.TimerEnabled = False
End Sub

Sub kicDrain_Hit()
	if BWR=0 then
	timNextBallDelay.interval = 2250
	else
	timNextBallDelay.interval = 4250
	end If
	BallOut = False
	kicDrain.DestroyBall
	PlaySoundAt "fx_drain_center", kicDrain
	DST.enabled=1
	WhenLitLights(0)
	PopBumperLights(0)

	If Tilted Then
		Tilted = False
		ligShootAgainPF.State = LightStateOff
		if B2SOn then 
			Controller.B2SSetTilt 0
			If B2Son Then Controller.B2SSetShootAgain 36, 0
		end if
		if Table1.ShowDT = True then ligShootAgainDT.state = LightStateOff		
		TiltedMode(False)
	End If

	' Delay before ejecting next player's ball
	timNextBallDelay.Enabled = True
	PlaySoundAt "fx_ball_reel", kicDrain
End Sub

Sub DST_timer()
	DST.enabled=0
	Bonus_Reset
	Playsound"MotorLeer2"
End Sub

'Check if BW came to stop Gaby

'Sub CBW_Timer()
'	if (BWR=0 and ballout=false) then
'	timNextBallDelay.Enabled = True
'	PlaySoundAt "fx_ball_reel", kicDrain
'	else
'	end If
'End Sub

'Sub CBW_Timer()
'	if BWR=0 then
'	timNextBallDelay.interval = 2250
'	else
'	timNextBallDelay.interval = 4250
'	end If
'End Sub

Sub NextBall()
	If ligShootAgainPF.State = LightStateOn Then
		If BallOut = False Then 
			Set ActiveBall = kicLaunch.CreateBall
			kicLaunch.kick 67,14
			PlaySoundAt "fx_ball_reel", kicDrain
			PlaySoundAt "fx_ball_kickout", kicLaunch
			If B2SOn Then DOF DOFKickerBottom, DOFStatePulse
		End If	
	Else
		CurrentPlayer = CurrentPlayer + 1

		If CurrentPlayer > NumberOfPlayers Then 
			CurrentPlayer = 1
			BallInPlay = BallInPlay + 1
			If BallInPlay > 1 then CreditLock = True
		End If

		if B2SOn then 
			Controller.B2SSetBallInPlay 32, BallInPlay
			Controller.B2SSetPlayerUp 30, CurrentPlayer
		end If
	
		If Table1.ShowDT = True Then
			for each obj in colBallInPlay : obj.visible = False : Next
			If BallInPlay > 0 and BallInPlay <= BallsPerCredit then
				colBallInPlay.Item(BallInPlay - 1).visible = True
			End If
			for each obj in colPlayerUp : obj.State = LightstateOff : Next
			colPlayerUp.Item(CurrentPlayer - 1).State = LightStateOn
		End If	

		If BallInPlay > BallsPerCredit Then
			GameOver
		Else
			If BallOut = False Then 
				Set ActiveBall = kicLaunch.CreateBall
				kicLaunch.kick 67,14
				PlaySoundAt "fx_ball_kickout", kicLaunch
				If B2SOn Then DOF DOFKickerBottom, DOFStatePulse
			End If	
		End If
	End If
End Sub 


Sub GameOver
	MatchReel_Update
	ligShootAgainPF.State = LightStateOff
	If B2SOn then 
		Controller.B2SSetGameOver 35, 1
		Controller.B2SSetBallInPlay 32, 0
		Controller.B2SSetCanPlay 31, 1
		Controller.B2SSetPlayerUp 30, 0
	End if

	If Table1.ShowDT = True Then
		ligGameOver.Visible = True
		ligShootAgainDT.visible = False
		for each obj in colBallInPlay : obj.State = LightStateOff : Next
		for each obj in colCanPlay : obj.State = LightStateOff : Next
		for each obj in colPlayerUp : obj.State = LightStateOff : Next
	End If
	
	BallInPlay = 0

	'Check if last digit of score equals match value

	If ScoreMatchIsActive = True and Tilted = False Then
		For i = 1 to NumberOfPlayers
			If Right(Matchvalue,1) = Right(PlayerScore(i-1),1) Then
				PlaysoundAt "fx_knocker",priKnocker
				If B2SOn Then DOF DOFKnock, DOFStatePulse
				AddCredit
			End If
		Next
	End If

	'Check for new high score
	Dim MaxScore
	If PlayerRealScore(0) > PlayerRealScore(1) Then
		MaxScore = PlayerRealScore(0)
	Else
		MaxScore = PlayerRealScore(1)
	End If
	If MaxScore > HighScore then
		HighScore = MaxScore
		HighScoreEntryInit()
		UpdatePostIt
	End If
	NumberOfPlayers = 0
	SaveSettings
	GameStateIsActive = False
	CreditLock = False
End Sub


'******************************************
'***		Load/Save Handler			***
'******************************************

Sub LoadSettings
    Dim temp
	temp = LoadValue(cGameName, "Credits")
    If (temp <> "") then Credits = CDbl(temp)
	temp = LoadValue(cGameName, "BallsPerCredit")
    If (temp <> "") then BallsPerCredit = CDbl(temp)
	temp = LoadValue(cGameName, "ScoreMatchIsActive")
    If (temp <> "") then ScoreMatchIsActive = CBool(temp)
	temp = LoadValue(cGameName, "hsa1")
    If (temp <> "") then HSA1 = CDbl(temp)
	temp = LoadValue(cGameName, "hsa2")
    If (temp <> "") then HSA2 = CDbl(temp)
	temp = LoadValue(cGameName, "hsa3")
    If (temp <> "") then HSA3 = CDbl(temp)
	temp = LoadValue(cGameName, "HighScore")
    If (temp <> "") then HighScore = CDbl(temp)
	temp = LoadValue(cGameName, "FlipperShadows")
    If (temp <> "") then FlipperShadows = CBool(temp)
	temp = LoadValue(cGameName, "BallShadows")
    If (temp <> "") then BallShadows = CBool(temp)
End Sub

Sub SaveSettings
	savevalue cGameName, "Credits", Credits
	savevalue cGameName, "BallsPerCredit", BallsPerCredit
	savevalue cGameName, "ScoreMatchIsActive", ScoreMatchIsActive
	savevalue cGameName, "HSA1", HSA1
	savevalue cGameName, "HSA2", HSA2
	savevalue cGameName, "HSA3", HSA3
	savevalue cGameName, "HighScore", HighScore
	savevalue cGameName, "FlipperShadows", FlipperShadows
	savevalue cGameName, "BallShadows", BallShadows
'	savevalue cGameName, "Score P1", PlayerScore(1) 'Gaby
End Sub


'******************************************
'***			Sounds		 			***
'******************************************
Sub triDrainSoundLeft_Hit()
	PlaySoundAt "fx_ball_drain_outlane", kicDrain
End Sub

Sub triDrainSoundRight_Hit()
	PlaySoundAt "fx_ball_drain_outlane", kicDrain
End Sub


Sub gatLaneGuard_Hit()
	gatLaneGuard.TimerEnabled = 1
	ligShootAgainPF.State = LightStateOff
	if Table1.ShowDT = True then ligShootAgainDT.visible = False
	if B2SOn then Controller.B2SSetShootAgain 36, 0
End Sub

Sub gatLaneGuard_Timer()
	priGateFlap.Roty = gatLaneGuard.CurrentAngle * 0.5
End Sub

Sub gatLaneGuardLeft_Hit()
	gatLaneGuard.TimerEnabled = 1
End Sub

Sub gatLaneGuardLeft_Timer()
	priGateFlapLeft.Roty = gatLaneGuardLeft.CurrentAngle * -0.5
End Sub



'*********************************************************************
'                 Positional Sound Playback Functions
'*********************************************************************
 
' Play a sound, depending on the X,Y position of the table element (especially cool for surround speaker setups, otherwise stereo panning only)
' parameters (defaults): loopcount (1), volume (1), randompitch (0), pitch (0), useexisting (0), restart (1))
' Note that this will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position
Sub PlayXYSound(soundname, tableobj, loopcount, volume, randompitch, pitch, useexisting, restart)
    PlaySound soundname, loopcount, volume, AudioPan(tableobj), randompitch, pitch, useexisting, restart, AudioFade(tableobj)
End Sub
 
' Similar subroutines that are less complicated to use (e.g. simply use standard parameters for the PlaySound call)
Sub PlaySoundAt(soundname, tableobj)
    PlaySound soundname, 1, 1, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub
 
Sub PlaySoundAtBall(soundname)
    PlaySoundAt soundname, ActiveBall
End Sub
 
'**************************************************************************
'                 Additional Positional Sound Playback Functions by DJRobX
'**************************************************************************
 
Sub PlaySoundAtVol(sound, tableobj, Vol)
        PlaySound sound, 1, Vol, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub
 
'Set position at table object, vol, and loops manually.
 
Sub PlaySoundAtVolLoops(sound, tableobj, Vol, Loops)
        PlaySound sound, Loops, Vol, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub
 

'*********************************************************************
'                     Supporting Ball & Sound Functions
'*********************************************************************
 
Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.y * 2 / Table1.height-1
    If tmp > 0 Then
        AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function
 
Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / table1.width - 1
    If tmp > 0 Then
        AudioPan = Csng(tmp ^10)
    Else
        AudioPan = Csng(-((- tmp) ^10) )
    End If
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
 


'*****************************************
'      JP's VP10 Rolling Sounds
'*****************************************

Const tnob = 5 ' total number of balls
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingTimer_Timer()
    Dim BOT, b
    BOT = GetBalls

	' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
    Next

	' exit the sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub

	' play the rolling sound for each ball
    For b = 0 to UBound(BOT)
        If BallVel(BOT(b) ) > 1 AND BOT(b).z < 30 Then
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If

		'***Ball Drop Sounds***

'		If and BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
'			PlaySound "fx_ball_drop" & b, 0, ABS(BOT(b).velz)/17, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, 'AudioFade(BOT(b))
'		End If

    Next
End Sub



'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub


'*****************************************
'	ninuzzu's	FLIPPER SHADOWS
'*****************************************

sub timFlipper_Timer()
	if FlipperShadows = True Then
		priFlipperShadowLeft.RotZ = flpLeft.currentangle - 5
		priFlipperShadowCenterRight.RotZ = flpCenterRight.currentangle - 5
		priFlipperShadowCenterLeft.RotZ = flpCenterLeft.currentangle + 5
		priFlipperShadowRight.RotZ = flpRight.currentangle + 5
	End If
End Sub


''*****************************************
''	ninuzzu's	BALL SHADOW
''*****************************************

Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5)

Sub BallShadowUpdate_Timer()
    Dim BOT, b
    BOT = GetBalls
    ' hide shadow of deleted ballsq
    If UBound(BOT)<(tnob-1) Then
        For b = (UBound(BOT) + 1) to (tnob-1)
            BallShadow(b).visible = 0
        Next
    End If
    ' exit the Sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub
    ' render the shadow for each ball
    For b = 0 to UBound(BOT)
        If BOT(b).X < table1.Width/2 Then
            BallShadow(b).X = ((BOT(b).X) - (cBallSize/6) + ((BOT(b).X - (table1.Width/2))/7)) + 6
        Else
            BallShadow(b).X = ((BOT(b).X) + (cBallSize/6) + ((BOT(b).X - (table1.Width/2))/7)) - 6
        End If
        ballShadow(b).Y = BOT(b).Y + 12
        If BOT(b).Z > 20 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
    Next
End Sub

'************************************
' What you need to add to your table
'************************************
 
' a timer called RollingTimer. With a fast interval, like 10
' one collision sound, in this script is called fx_collide
' as many sound files as max number of balls, with names ending with 0, 1, 2, 3, etc
' for ex. as used in this script: fx_ballrolling0, fx_ballrolling1, fx_ballrolling2, fx_ballrolling3, etc
 

'******************************************
' Explanation of the rolling sound routine
'******************************************
 
' sounds are played based on the ball speed and position
 
' the routine checks first for deleted balls and stops the rolling sound.
 
' The For loop goes through all the balls on the table and checks for the ball speed and
' if the ball is on the table (height lower than 30) then then it plays the sound
' otherwise the sound is stopped, like when the ball has stopped or is on a ramp or flying.
 
' The sound is played using the VOL, PAN and PITCH functions, so the volume and pitch of the sound
' will change according to the ball speed, and the PAN function will change the stereo position according
' to the position of the ball on the table.
 
 
'**************************************
' Explanation of the collision routine
'**************************************
 
' The collision is built in VP.
' You only need to add a Sub OnBallBallCollision(ball1, ball2, velocity) and when two balls collide they
' will call this routine. What you add in the sub is up to you. As an example is a simple Playsound with volume and paning
' depending of the speed of the collision.
 
Sub colBumpers_Hit (idx)
	RandomSoundBumper()
End Sub

Sub colTargets_Hit (idx)
	PlaySound "target", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub


Sub colGates_Hit (idx)
	PlaySound "fx_gate", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub colSlingshots_Slingshot(idx)
	PlaySound "fx_leafswitch_hit", 0, 1, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	PlaySound "fx_slingshot_hit", 0, 1, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub colPosts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		PlaySound "fx_rubber_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub colSwitches_SlingShot()
	PlaySoundAtVol "fx_leafswitch_hit", ActiveBall, 0.6
	Call AddToScore(10,1)
End Sub

Sub colMetalThick_Hit()
	PlaySoundAtVol "fx_metal_hit_thick", ActiveBall, 1
End Sub

Sub colRubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySound "fx_rubber_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub flpLeft_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub flpRight_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub flpCenterLeft_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub flpCenterRight_Collide(parm)
 	RandomSoundFlipper()
End Sub


Sub RandomSoundBumper()
	PlaySound "fx_bumper_" & cstr(Int(Rnd*4)+1), 0, 1, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub RandomSoundRubber()
	PlaySound "fx_rubber_hit_" & cstr(Int(Rnd*3)+1), 0, 1, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub RandomSoundFlipper()
	PlaySound "fx_flipper_hit_" & cstr(Int(Rnd*3)+1), 0, 1, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub kicLaunch_Hit()
	kicLaunch.kick 67,14
	PlaySoundAt "fx_ball_kickout", kicLaunch
	If B2SOn Then DOF DOFKickerBottom, DOFStatePulse
End Sub

sub timNextBallDelay_timer()
	timNextBallDelay.Enabled=false
	Nextball
end sub

'==========================================================================================================================================
'============================================================= START OF HIGH SCORES ROUTINES =============================================================
'==========================================================================================================================================
'
'ADD LINE TO TABLE_KEYDOWN SUB WITH THE FOLLOWING:    If HSEnterMode Then HighScoreProcessKey(keycode) AFTER THE STARTGAME ENTRY
'ADD: And Not HSEnterMode=true TO IF KEYCODE=STARTGAMEKEY
'TO SHOW THE SCORE ON POST-IT ADD LINE AT RELEVENT LOCATION THAT HAS:  UpdatePostIt
'TO INITIATE ADDING INITIALS ADD LINE AT RELEVENT LOCATION THAT HAS:  HighScoreEntryInit()
'ADD THE FOLLOWING LINES TO TABLE_INIT TO SETUP POSTIT
'	if HSA1="" then HSA1=25
'	if HSA2="" then HSA2=25
'	if HSA3="" then HSA3=25
'	UpdatePostIt
'ADD HSA1, HSA2 AND HSA3 TO SAVE AND LOAD VALUES FOR TABLE
'ADD A TIMER NAMED HighScoreFlashTimer WITH INTERVAL 100 TO TABLE
'SET HSSSCOREX BELOW TO WHATEVER VARIABLE YOU USE FOR HIGH SCORE.
'ADD OBJECTS TO PLAYFIELD (EASIEST TO JUST COPY FROM THIS TABLE)
'IMPORT POST-IT IMAGES


Dim HSA1, HSA2, HSA3
Dim HSEnterMode, hsLetterFlash, hsEnteredDigits(3), hsCurrentDigit, hsCurrentLetter
Dim HSArray  
Dim HSScoreM,HSScore100k, HSScore10k, HSScoreK, HSScore100, HSScore10, HSScore1, HSScorex	'Define 6 different score values for each reel to use
HSArray = Array("Postit0","postit1","postit2","postit3","postit4","postit5","postit6","postit7","postit8","postit9","postitBL","postitCM","Tape")
Const hsFlashDelay = 4

' ***********************************************************
'  HiScore DISPLAY 
' ***********************************************************

Sub UpdatePostIt
	dim tempscore
	HSScorex = HighScore
	TempScore = HSScorex
	HSScore1 = 0
	HSScore10 = 0
	HSScore100 = 0
	HSScoreK = 0
	HSScore10k = 0
	HSScore100k = 0
	HSScoreM = 0
	if len(TempScore) > 0 Then
		HSScore1 = cint(right(Tempscore,1))
	end If
	if len(TempScore) > 1 Then
		TempScore = Left(TempScore,len(TempScore)-1)
		HSScore10 = cint(right(Tempscore,1))
	end If
	if len(TempScore) > 1 Then
		TempScore = Left(TempScore,len(TempScore)-1)
		HSScore100 = cint(right(Tempscore,1))
	end If
	if len(TempScore) > 1 Then
		TempScore = Left(TempScore,len(TempScore)-1)
		HSScoreK = cint(right(Tempscore,1))
	end If
	if len(TempScore) > 1 Then
		TempScore = Left(TempScore,len(TempScore)-1)
		HSScore10k = cint(right(Tempscore,1))
	end If
	if len(TempScore) > 1 Then
		TempScore = Left(TempScore,len(TempScore)-1)
		HSScore100k = cint(right(Tempscore,1))
	end If
	if len(TempScore) > 1 Then
		TempScore = Left(TempScore,len(TempScore)-1)
		HSScoreM = cint(right(Tempscore,1))
	end If
	Pscore6.image = HSArray(HSScoreM):If HSScorex<1000000 Then PScore6.image = HSArray(10)
	Pscore5.image = HSArray(HSScore100K):If HSScorex<100000 Then PScore5.image = HSArray(10)
	PScore4.image = HSArray(HSScore10K):If HSScorex<10000 Then PScore4.image = HSArray(10)
	PScore3.image = HSArray(HSScoreK):If HSScorex<1000 Then PScore3.image = HSArray(10)
	PScore2.image = HSArray(HSScore100):If HSScorex<100 Then PScore2.image = HSArray(10)
	PScore1.image = HSArray(HSScore10):If HSScorex<10 Then PScore1.image = HSArray(10)
	PScore0.image = HSArray(HSScore1):If HSScorex<1 Then PScore0.image = HSArray(10)
	if HSScorex<1000 then
		PComma.image = HSArray(10)
	else
		PComma.image = HSArray(11)
	end if
	if HSScorex<1000000 then
		PComma2.image = HSArray(10)
	else
		PComma2.image = HSArray(11)
	end if
	HSName1.image = ImgFromCode(HSA1, 1)
	HSName2.image = ImgFromCode(HSA2, 2)
	HSName3.image = ImgFromCode(HSA3, 3)
End Sub

Function ImgFromCode(code, digit)
	Dim Image
	if (HighScoreFlashTimer.Enabled = True and hsLetterFlash = 1 and digit = hsCurrentLetter) then
		Image = "postitBL"
	elseif (code + ASC("A") - 1) >= ASC("A") and (code + ASC("A") - 1) <= ASC("Z") then
		Image = "postit" & chr(code + ASC("A") - 1)
	elseif code = 27 Then
		Image = "PostitLT"
    elseif code = 0 Then
		image = "PostitSP"
    Else
      msgbox("Unknown display code: " & code)
	end if
	ImgFromCode = Image
End Function

Sub HighScoreEntryInit()
	HSA1=0:HSA2=0:HSA3=0
	HSEnterMode = True
	hsCurrentDigit = 0
	hsCurrentLetter = 1:HSA1=1
	HighScoreFlashTimer.Interval = 250
	HighScoreFlashTimer.Enabled = True
	hsLetterFlash = hsFlashDelay
End Sub

Sub HighScoreFlashTimer_Timer()
	hsLetterFlash = hsLetterFlash-1
	UpdatePostIt
	If hsLetterFlash=0 then 'switch back
		hsLetterFlash = hsFlashDelay
	end if
End Sub


' ***********************************************************
'  HiScore ENTER INITIALS 
' ***********************************************************

Sub HighScoreProcessKey(keycode)
    If keycode = LeftFlipperKey Then
		hsLetterFlash = hsFlashDelay
		Select Case hsCurrentLetter
			Case 1:
				HSA1=HSA1-1:If HSA1=-1 Then HSA1=26 'no backspace on 1st digit
				UpdatePostIt
			Case 2:
				HSA2=HSA2-1:If HSA2=-1 Then HSA2=27
				UpdatePostIt
			Case 3:
				HSA3=HSA3-1:If HSA3=-1 Then HSA3=27
				UpdatePostIt
		 End Select
    End If

	If keycode = RightFlipperKey Then
		hsLetterFlash = hsFlashDelay
		Select Case hsCurrentLetter
			Case 1:
				HSA1=HSA1+1:If HSA1>26 Then HSA1=0
				UpdatePostIt
			Case 2:
				HSA2=HSA2+1:If HSA2>27 Then HSA2=0
				UpdatePostIt
			Case 3:
				HSA3=HSA3+1:If HSA3>27 Then HSA3=0
				UpdatePostIt
		 End Select
	End If
	
    If keycode = StartGameKey Then
		Select Case hsCurrentLetter
			Case 1:
				hsCurrentLetter=2 'ok to advance
				HSA2=HSA1 'start at same alphabet spot
			Case 2:
				If HSA2=27 Then 'bksp
					HSA2=0
					hsCurrentLetter=1
				Else
					hsCurrentLetter=3 'enter it
					HSA3=HSA2 'start at same alphabet spot
				End If
			Case 3:
				If HSA3=27 Then 'bksp
					HSA3=0
					hsCurrentLetter=2
				Else
					SaveSettings 'enter it
					HighScoreFlashTimer.Enabled = False
					HSEnterMode = False
				End If
		End Select
		UpdatePostIt
    End If
End Sub




' ******************************
' **** Tilt Bobber Simulator ***
' ******************************
'
'  This routine is called each time game is Nudged
'  to Check to see if Player has tilted the game.

Sub CheckTilt()											'Called when table is nudged
	TiltCount=TiltCount+1										'Add to tilt count (hit lasts 1 second)
	If TiltCount>TiltSensitivity And Tilted=False Then		'If more than Allowed Counts then TILTED
	   Tilted=True
       TiltedMode (True) 							'Disable slings, bumpers etc
	End If
End Sub
'
' Tilt Timer Cycles every 250ms (1/4 sec) when game is running
'
Sub TiltTimer_Timer()				'Used to simulate a tilt bob
	If TiltCount>0 Then
       TiltCount=TiltCount-0.25			'Subtract .25 every 250ms
    End If
End Sub


Sub TiltedMode(TiltState)
	If TiltState = True Then
 		For i = 1 to 2
			PlaySound "fx_knocker"
			If B2SOn Then DOF DOFKnock, DOFStatePulse
		Next 
       If Table1.ShowDT then ligTiltDT.visible = True
       If B2SOn then Controller.B2SSetTilt 1
		bmpBumperTopLeft.HasHitEvent = False
		bmpBumperTopRight.HasHitEvent = False
		bmpBumperCenter.HasHitEvent = False
		flpLeft.RotateToStart
		flpCenterRight.RotateToStart
		flpCenterLeft.RotateToStart
		flpRight.RotateToStart
		walSlingshotLowerLeft.Disabled = True
		walSlingshotLowerRight.Disabled = True
	Else
       If Table1.ShowDT then ligTiltDT.visible = False
       If B2SOn then Controller.B2SSetTilt 0
		bmpBumperTopLeft.HasHitEvent = True
		bmpBumperTopRight.HasHitEvent = True
		bmpBumperCenter.HasHitEvent = True
		walSlingshotLowerLeft.Disabled = False
		walSlingshotLowerRight.Disabled = False
	End If
End Sub





Function IIF (ByVal expr, ByVal retval1, ByVal retval2)
		IIF = retval2
		IF expr Then IIF = retval1
		debug.print "IIF return value = " & IIF
End Function

Sub kicBonus0_Timer()
	
End Sub