'/////////////////////////////////////////////////////////
'			   THE FOG Gottlieb (1979)                   /
'                Created by: HiRez00                     /
'                                                        /
'                                                        /
' Original VPX table: Space Walk by BorgDog              /
'                                                        /
' Special Thanks to: Rascal, BorgDog, and Loserman76     /
'                                                        /
' DOF by Arngrim                                         /
' SSF by Thalamus                                        /
'                                                        /
'/////////////////////////////////////////////////////////


Option Explicit
Randomize

'+++++++++++++++++++++++
'+     TABLE NOTES     +
'+++++++++++++++++++++++
'
' There and 13 LUT brightness levels built into this table.
' Hold down LEFT MAGNASAVE and then press RIGHT MAGNASAVE to adjust / cycle through the different LUT brightness levels.
' The LUT you choose will be automatically saved when you exit the table.

' While table is active during gameplay, you can change the music track by pressing the RIGHT MAGNASAVE.
' You can press the LEFT MAGNASAVE at any time to TURN OFF the current music track completely.
' You can also use the TABLE OPTIONS below to customize when music tracks will play during the game.

' Hold doown the LEFT FLIPPER for 2-3 seconds to access the table's OPTIONS MENU.

'+++++++++++++++++++++++
'+    TABLE OPTIONS    +
'+++++++++++++++++++++++
'--------------------------------------------------------------------------------------------------------------------
MusicOption1 = 1	'1 = Attract Audio ON    0 = Attract Audio OFF
MusicOption2 = 1	'1 = Gameplay Audio ON   0 = Gameplay Audio OFF
MusicOption3 = 1	'1 = Game Over Audio ON  0 = Game Over Audio OFF
'--------------------------------------------------------------------------------------------------------------------
alternateflippers = False	'True enables alternative green flippers, False sets flippers back to factory flippers
'--------------------------------------------------------------------------------------------------------------------
AddBallShadow = 1	'1 = Ball Shadows ON  0 = Ball Shawdows OFF - Turn OFF if using Ambient Occlusion
'--------------------------------------------------------------------------------------------------------------------
AddFlipperShadows = 1    '1 = Flipper Shadows ON  0 = Flipper Shawdows OFF - Turn OFF if using Ambient Occlusion
'--------------------------------------------------------------------------------------------------------------------
EndGameFog = True    'True = Fog Animation at End ON  False = Fog Animation at End OFF - Set to FALSE for slow CPUS
'--------------------------------------------------------------------------------------------------------------------
BackGlassBreak = True    'True = Glass Break Effect on B2S Backglass ON  False = Glass Break Effect on B2S Backglass OFF
'--------------------------------------------------------------------------------------------------------------------

'----- Phsyics Mods -----
Const RubberizerEnabled = 0			'0 = normal flip rubber, 1 = more lively rubber for flips, 2 = a different rubberizer
Const FlipperCoilRampupMode = 0   	'0 = fast, 1 = medium, 2 = slow (tap passes should work)
Const TargetBouncerEnabled = 1 		'0 = normal standup targets, 1 = bouncy targets, 2 = orig TargetBouncer
Const TargetBouncerFactor = 1 	'Level of bounces. Recommmended value of 0.7 when TargetBouncerEnabled=1, and 1.1 when TargetBouncerEnabled=2

'+++++++++++++++++++++++
'+  END TABLE OPTIONS  +
'+++++++++++++++++++++++


' Thalamus 2018-07-24
' Added/Updated "Positional Sound Playback Functions" and "Supporting Ball & Sound Functions"
' Thalamus 2018-11-01 : Improved directional sounds
' !! NOTE : Table not verified yet !!

' Options
' Volume devided by - lower gets higher sound

Const VolDiv = 2000    ' Lower number, louder ballrolling/collition sound
Const VolCol = 10      ' Ball collition divider ( voldiv/volcol )

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
Const VolSpin   = 1.5  ' Spinners volume.
Const VolFlip   = 1    ' Flipper volume.


Const cGameName = "the_fog_1979"
Const Ballsize = 50
'Const Ballmass = 1

Const HSFileName="The_Fog_1979.txt"

dim award
Dim balls
Dim ebcount
Dim credit, freeplay
Dim score(2)
Dim sreels(2)
Dim player, players, maxplayers
Dim Add10, Add100, Add1000
Dim state
Dim tilt, tiltsens
Dim Replay1Table(3)
Dim Replay2Table(3)
Dim Replay3Table(3)
dim replay1, replay2, replay3, replays
dim hisc, hiscstate, showhisc
Dim bumperlitscore
Dim bumperoffscore
Dim Bonus, dbonus
Dim Bonuslight(19)
Dim ballinplay
Dim matchnumb
dim plungeball
dim tflash
dim eflash
dim fivek
Dim rep(2)
Dim eg
Dim starstate
Dim abonus(2,4)
Dim OperatorMenu, options, objekt
Dim i,j,e,t,a,light
Dim alternateflippers
Dim AddBallShadow
Dim AddFlipperShadows
Dim MusicOption1
Dim MusicOption2
Dim MusicOption3
Dim EndGameFog
Dim BackGlassBreak
Dim FileObj
Dim ScoreFile
Dim TextStr,TextStr2
Dim TargetIsDropped(15)

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "Can't open controller.vbs"
On Error Goto 0


Sub Table1_Init()

	LoadEM
    If MusicOption1 = 1 then AttractMusic: End If
	maxplayers=2
	Replay1Table(1)=90000
	Replay1Table(2)=120000
	Replay1Table(3)=140000
	Replay2Table(1)=120000
	Replay2Table(2)=140000
	Replay2Table(3)=180000
	Replay3Table(1)=150000
	Replay3Table(2)=180000
	Replay3Table(3)=200000
	set sreels(1) = ScoreReel1
	set sreels(2) = ScoreReel2
	set abonus(1,1)=ABonus1
	set abonus(1,2)=ABonus2
	set abonus(1,3)=ABonus3
	set abonus(1,4)=ABonus4
	set abonus(2,1)=ABonus4
	set abonus(2,2)=ABonus3
	set abonus(2,3)=ABonus2
	set abonus(2,4)=ABonus1
	set bonuslight(1)=bonus1
	set bonuslight(2)=bonus2
	set bonuslight(3)=bonus3
	set bonuslight(4)=bonus4
	set bonuslight(5)=bonus5
	set bonuslight(6)=bonus6
	set bonuslight(7)=bonus7
	set bonuslight(8)=bonus8
	set bonuslight(9)=bonus9
	set bonuslight(10)=bonus10
	set bonuslight(11)=bonus11
	set bonuslight(12)=bonus12
	set bonuslight(13)=bonus13
	set bonuslight(14)=bonus14
	set bonuslight(15)=bonus15
	set bonuslight(16)=bonus16
	set bonuslight(17)=bonus17
	set bonuslight(18)=bonus18
	set bonuslight(19)=bonus19
	turnoff
	hideoptions
    credit=0
	hisc=150000
    matchnumb=5
    score(1)=000000
    score(2)=000000
	replays=2
	balls=5
	freeplay=0
	HSA1=8
	HSA2=18
	HSA3=26
	showhisc=1
	loadhs
	bipreel.setvalue 0
	creditreel.setvalue credit
	Replay1=Replay1Table(Replays)
	Replay2=Replay2Table(Replays)
	Replay3=Replay3Table(Replays)
	player=1
	plungeball=0
	if balls=3 then
		bumperlitscore=1000
		bumperoffscore=1000
		InstCard.image="InstCard3balls"
	  else
		bumperlitscore=100
		bumperoffscore=100
		InstCard.image="InstCard5balls"
	end if
	OptionBalls.imageA="OptionsBalls"&Balls
	OptionReplays.imageA="OptionsReplays"&replays
	OptionFreeplay.imageA="OptionsFreeplay"&freeplay
	OptionShowhisc.imageA="OptionsYN"&showhisc
	RepCard.image = "ReplayCard"&replays
	If showhisc=0 then
		for each objekt in hiscStuff: objekt.visible = 0: next
	  else
		UpdatePostIt
	end if
	If B2SOn then
		'for each objekt in backdropstuff : objekt.visible = 0 : next
    Else
		'for each objekt in backdropstuff : objekt.visible = 1 : next
	End If
    scorereel1.setvalue(score(1))
    scorereel2.setvalue(score(2))
	tilt=false
	startGame.enabled=true
    Drain.CreateBall
	dim j : for j = 0 to 15
		TargetIsDropped(j) = false
	next
    LoadLUT
End Sub

'*************
'   JP'S LUT
'*************

Dim bLutActive, LUTImage
Dim x
Sub LoadLUT
	bLutActive = False
    x = LoadValue(cGameName, "LUTImage")
    If(x <> "") Then LUTImage = x Else LUTImage = 0
	UpdateLUT
End Sub

Sub SaveLUT
    SaveValue cGameName, "LUTImage", LUTImage
End Sub

Sub NextLUT: LUTImage = (LUTImage +1 ) MOD 13: UpdateLUT: SaveLUT: End Sub

Sub UpdateLUT
Select Case LutImage
Case 0: Table1.ColorGradeImage = "LUT0"
Case 1: Table1.ColorGradeImage = "LUT1"
Case 2: Table1.ColorGradeImage = "LUT2"
Case 3: Table1.ColorGradeImage = "LUT3"
Case 4: Table1.ColorGradeImage = "LUT4"
Case 5: Table1.ColorGradeImage = "LUT5"
Case 6: Table1.ColorGradeImage = "LUT6"
Case 7: Table1.ColorGradeImage = "LUT7"
Case 8: Table1.ColorGradeImage = "LUT8"
Case 9: Table1.ColorGradeImage = "LUT9"
Case 10: Table1.ColorGradeImage = "LUT10"
Case 11: Table1.ColorGradeImage = "LUT11"
Case 12: Table1.ColorGradeImage = "LUT12"
End Select
End Sub

sub startGame_timer
	playsound "poweron"
	lightdelay.enabled=true
	me.enabled=false
end sub

sub lightdelay_timer
	If credit > 0 or freeplay=1 Then DOF 121, DOFOn
	for i = 1 to maxplayers
		if score(i)>99999 then EVAL("p100k"&i).setvalue 1 'int(score(i)/100000)
	next
	gamov.text="Game Over"
	gamov.timerenabled=1
	tilttxt.timerenabled=1
	For each light in GIlights:light.state=1:Next
	bumperpulse.Enabled= True 'TURN ON BUMPERPULSE TIMER FOR FADING FLASHER EFFECT ON BUMPER, SEE SCRIPT NEAR BOTTOM - RASCAL
	BumperLight1.state=1
	if matchnumb=0 then
		matchtxt.text="00"
	  else
		matchtxt.text=matchnumb*10
	end if
	if B2SOn then
		for i = 1 to 2
			Controller.B2SSetScorePlayer i, Score(i) MOD 100000
			if score(i)>99999 then Controller.B2SSetScoreRollover 24+i, 1
		next
		Controller.B2SSetData 6,1
		Controller.B2ssetCredits Credit
		Controller.B2ssetMatch 34, Matchnumb*10
		Controller.B2SSetGameOver 35,1
	end if
	me.enabled=0
end Sub

sub gamov_timer
	if state=false then
		If B2SOn then Controller.B2SSetGameOver 35,0
		gamov.text=""
		gtimer.enabled=true
	end if
	gamov.timerenabled=0
end sub

sub gtimer_timer
	if state=false then
		gamov.text="Game Over"
		If B2SOn then Controller.B2SSetGameOver 35,1
		gamov.timerenabled=1
		gamov.timerinterval= (INT (RND*10)+5)*100
	end if
	me.enabled=0
end sub

sub tilttxt_timer
	if state=false then
		tilttxt.text=""
		If B2SOn then Controller.B2SSetTilt 33,0
		ttimer.enabled=true
	end if
	tilttxt.timerenabled=0
end sub

sub ttimer_timer
	if state=false then
		tilttxt.text="TILT"
		If B2SOn then Controller.B2SSetTilt 33,1
		tilttxt.timerenabled=1
		tilttxt.timerinterval= (INT (RND*10)+5)*100
	end if
	me.enabled=0
end sub

Sub Table1_KeyDown(ByVal keycode)

	if keycode=AddCreditKey then
		     playsoundAtVol "coin3", drain, 1
		     coindelay.enabled=true
    end if

	If keycode = LeftMagnaSave Then bLutActive = True: EndMusic
	If keycode = RightMagnaSave Then 
		If bLutActive Then NextLUT: End If
        If bLutActive = False and state=true Then NextTrack: End If
        If bLutActive = False and state=false and eg=1 and EndDelay3.Enabled = False Then OutroMusic: End If
    End If

    if keycode=StartGameKey and EndDelay3.Enabled = False and (credit>0 or freeplay=1) And Not HSEnterMode=true and FogFlasher.Visible = False and startGame.enabled=0 and lightdelay.enabled=0 then
        if state=false then
		state=true
		if freeplay=0 then credit=credit-1
		If credit < 1 and freeplay=0 Then DOF 121, DOFOff
		if freeplay=0 then creditreel.setvalue credit
		ballinplay=1
		If B2SOn Then
            Controller.B2SSetData 88,0
            Controller.B2SSetData 80,0
            Controller.B2SSetData 81,0
			Controller.B2ssetCredits Credit
			Controller.B2sStartAnimation "Startup"
			Controller.B2ssetballinplay 32, Ballinplay
			Controller.B2ssetplayerup 30, 1
		    Controller.B2SSetData 80,1
            Controller.B2SSetData 85,1
			Controller.B2ssetcanplay 31, 1
			Controller.B2SSetGameOver 0
		End If
		Pup1.setvalue(1)
		tilt=false
		playsound "initialize"
		players=1
        EndMusic
        NextTrack
		canplayreel.setvalue(players)
		newgame.enabled=true
	  else if state=true and players < 2 and Ballinplay=1 then
		if freeplay=0 then credit=credit-1
		If credit < 0 and freeplay=0 Then DOF 121, DOFOff
		players=players+1
		if freeplay=0 then creditreel.setvalue credit
		If B2SOn then
			Controller.B2ssetCredits Credit
			Controller.B2ssetcanplay 31, 2
		End If
		canplayreel.setvalue(players)
		playsound "click"
	   end if
	  end if
	end if

	If HSEnterMode Then HighScoreProcessKey(keycode)

	If keycode = PlungerKey Then
		Plunger.PullBack
		PlaySoundAtVol "plungerpull", plunger, 1
	End If

	If keycode=LeftFlipperKey and State = false and OperatorMenu=0 And Not HSEnterMode=true then
		OperatorMenuTimer.Enabled = true
	end if

	If keycode=LeftFlipperKey and State = false and OperatorMenu=1 then
		Options=Options+1
		If Options=6 then Options=1
		playsound "drop1"
		Select Case (Options)
			Case 1:
				Option1.visible=true
				Option5.visible=False
			Case 2:
				Option2.visible=true
				Option1.visible=False
			Case 3:
				Option3.visible=true
				Option2.visible=False
			Case 4:
				Option4.visible=true
				Option3.visible=False
			Case 5:
				Option5.visible=true
				Option4.visible=False
		End Select
	end if

	If keycode=RightFlipperKey and State = false and OperatorMenu=1 then
	  PlaySound "cluper"
	  Select Case (Options)
		Case 1:
			if Balls=3 then
				Balls=5
				InstCard.image="InstCard5balls"
			  else
				Balls=3
				InstCard.image="InstCard3balls"
			end if
			OptionBalls.imageA = "OptionsBalls"&Balls
		Case 2:
			if Freeplay=0 then
				Freeplay=1
			  else
				Freeplay=0
			end if
			OptionFreeplay.imageA = "OptionsFreeplay"&freeplay
		Case 3:
			if showhisc=0 then
				showhisc=1
				for each objekt in hiscStuff: objekt.visible = 1: next
				UpdatePostIt
			  else
				for each objekt in hiscStuff: objekt.visible = 0: next
				showhisc=0
			end if
			OptionShowhisc.imageA = "OptionsYN"&showhisc
		Case 4:
			Replays=Replays+1
			if Replays>3 then
				Replays=1
			end if
			Replay1=Replay1Table(Replays)
			Replay2=Replay2Table(Replays)
			Replay3=Replay3Table(Replays)
			OptionReplays.imageA = "OptionsReplays"&replays
			repcard.image = "replaycard"&replays
		Case 5:
			OperatorMenu=0
			savehs
			HideOptions
	  End Select
	End If

  if tilt=false and state=true then
	If keycode = LeftFlipperKey Then
'		LeftFlipper.RotateToEnd
'		LeftFlipper1.RotateToEnd
		FlipperActivate LeftFlipper, LFPress
		SolLFlipper True
		if state=true then TLLFlip.state=1
		PlaySoundAtVol SoundFXDOF("flipperup",101,DOFOn,DOFFlippers), LeftFlipper, VolFlip
		PlaySoundAtVol "flipperup", LeftFlipper1, VolFlip
		PlaySoundAtVol "Buzz", LeftFlipper, VolFlip
		PlaySoundAtVol "Buzz", LeftFlipper1, VolFlip
	End If

	If keycode = RightFlipperKey Then
'		RightFlipper.RotateToEnd
'		RightFlipper1.RotateToEnd
		FlipperActivate RightFlipper, RFPress
		SolRFlipper True
		if state=true then TLRFlip.state=1
		PlaySoundAtVol SoundFXDOF("flipperup",102, DOFOn, DOFFlippers), RightFlipper, VolFlip
		PlaySoundAtVol "flipperup", RightFlipper1, VolFlip
		PlaySoundAtVol "Buzz1", RightFlipper, VolFlip
		PlaySoundAtVol "Buzz1", RightFlipper1, VolFlip
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

  end if
End Sub

Sub OperatorMenuTimer_Timer
	OperatorMenu=1
	Displayoptions
	Options=1
End Sub

Sub DisplayOptions
	OptionsBack.visible = true
	Option1.visible = True
	OptionBalls.visible = True
    OptionReplays.visible = True
    OptionFreeplay.visible = True
	OptionShowhisc.visible = true
End Sub

Sub HideOptions
	for each objekt In OptionMenu
		objekt.visible = false
	next
End Sub


Sub Table1_KeyUp(ByVal keycode)

	If keycode = PlungerKey Then
		Plunger.Fire
		if PlungeBall=1 then
			playsoundAtVol "plunger",plunger,1
		  else
			playsoundAtVol "plungerreleasefree",plunger,1
		end if
	End If

    If keycode = LeftMagnaSave Then bLutActive = False

	if keycode = LeftFlipperKey then
		OperatorMenuTimer.Enabled = false
	end if

   if tilt=false and state=true then
	If keycode = LeftFlipperKey Then
'		LeftFlipper.RotateToStart
'		LeftFlipper1.RotateToStart
		FlipperDeActivate LeftFlipper, LFPress
		SolLFlipper False	

		TLLFlip.state=0
		PlaySoundAtVol SoundFXDOF("flipperdown", 101, DOFOff, DOFContactors),LeftFlipper, VolFlip
		PlaySoundAtVol "flipperdown",LeftFlipper1, VolFlip
		StopSound "Buzz"
	End If

	If keycode = RightFlipperKey Then
'		RightFlipper.RotateToStart
'		RightFlipper1.RotateToStart
		FlipperDeActivate RightFlipper, RFPress
		SolRFlipper False

		TLRFlip.state=0
		PlaySoundAtVol SoundFXDOF("flipperdown", 102, DOFOff, DOFContactors), RightFlipper, VolFlip
		PlaySoundAtVol "flipperdown", RightFlipper, VolFlip
		StopSound "Buzz1"
	End If
  end if
End Sub


'*******************************************
'  Flippers
'*******************************************

Const ReflipAngle = 20

' Flipper Solenoid Callbacks (these subs mimics how you would handle flippers in ROM based tables)
Sub SolLFlipper(Enabled)
	If Enabled Then
		LF.Fire  'leftflipper.rotatetoend
		LeftFlipper1.RotateToEnd
'		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
'			RandomSoundReflipUpLeft LeftFlipper
'		Else 
'			SoundFlipperUpAttackLeft LeftFlipper
'			RandomSoundFlipperUpLeft LeftFlipper
'		End If		
	Else
		LeftFlipper.RotateToStart
		LeftFlipper1.RotateToStart
'		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
'			RandomSoundFlipperDownLeft LeftFlipper
'		End If
'		FlipperLeftHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub SolRFlipper(Enabled)
	If Enabled Then
		RF.Fire 'rightflipper.rotatetoend
		RightFlipper1.RotateToEnd
'		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
'			RandomSoundReflipUpRight RightFlipper
'		Else 
'			SoundFlipperUpAttackRight RightFlipper
'			RandomSoundFlipperUpRight RightFlipper
'		End If
	Else
		RightFlipper.RotateToStart
		RightFlipper1.RotateToStart
'		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
'			RandomSoundFlipperDownRight RightFlipper
'		End If	
'		FlipperRightHitParm = FlipperUpSoundLevel
	End If
End Sub


'****************************************
'*         Attract Mod: HiRez00         *
'****************************************

Sub AttractMusic
If MusicOption1 = 1 then
    Dim t
    t = INT(3 * RND(1) )
    If t = 0 then PlayMusic"fog/tf-attract_01.mp3" End If
    If t = 1 then PlayMusic"fog/tf-attract_02.mp3" End If
    If t = 2 then PlayMusic"fog/tf-attract_03.mp3" End If
End If

End Sub

'**************************************
'*         Music Mod: HiRez00         *
'**************************************

Dim musicNum
Sub NextTrack
If MusicOption2 = 1 then
    If musicNum = 0 Then PlayMusic "fog/tf-pl-reel-9-2.mp3" End If
    If musicNum = 1 Then PlayMusic "fog/tf-pl-antonio-bay.mp3" End If
    If musicNum = 2 Then PlayMusic "fog/tf-pl-seagrass.mp3" End If
    If musicNum = 3 Then PlayMusic "fog/tf-pl-ghost-ship.mp3" End If
    If musicNum = 4 Then PlayMusic "fog/tf-pl-journal.mp3" End If
    If musicNum = 5 Then PlayMusic "fog/tf-pl-ghost-story.mp3" End If
    If musicNum = 6 Then PlayMusic "fog/tf-pl-lighthouse.mp3" End If
    If musicNum = 7 Then PlayMusic "fog/tf-pl-reel-9-1.mp3" End If
    If musicNum = 8 Then PlayMusic "fog/tf-pl-blake.mp3" End If
    If musicNum = 09 Then PlayMusic "fog/tf-pl-dane.mp3" End If
    If musicNum = 10 Then PlayMusic "fog/tf-pl-trouble.mp3" End If
    If musicNum = 11 Then PlayMusic "fog/tf-pl-revenge.mp3" End If
    If musicNum = 12 Then PlayMusic "fog/tf-pl-fog-reprise.mp3" End If
    If musicNum = 13 Then PlayMusic "fog/tf-pl-fog.mp3" End If

musicNum = (musicNum + 1) mod 14

End If

End Sub

Sub Table1_MusicDone
        If state=false Then 
          OutroMusic
        Else
          NextTrack
        End If
End Sub

'**************************************************************
'*         Outro Mod + Non-Repeat Randomizer: HiRez00         *
'**************************************************************

Sub OutroMusic
If MusicOption3 = 1 then
    Dim m
    Dim OldMusic
    m = INT(8 * RND(1) )
    If m = (OldMusic) then OutroMusic: End If
    If m = 0 then PlayMusic"fog/tf-game-over-1.mp3": OldMusic = (m): End If
    If m = 1 then PlayMusic"fog/tf-game-over-2.mp3": OldMusic = (m): End If
    If m = 2 then PlayMusic"fog/tf-game-over-3.mp3": OldMusic = (m): End If
    If m = 3 then PlayMusic"fog/tf-game-over-4.mp3": OldMusic = (m): End If
    If m = 4 then PlayMusic"fog/tf-game-over-5.mp3": OldMusic = (m): End If
    If m = 5 then PlayMusic"fog/tf-game-over-6.mp3": OldMusic = (m): End If
    If m = 6 then PlayMusic"fog/tf-game-over-7.mp3": OldMusic = (m): End If
    If m = 7 then PlayMusic"fog/tf-game-over-8.mp3": OldMusic = (m): End If
End If

End Sub


sub newgame_timer
    bumper1.hashitevent=1
	fivek=0
	player=1
    pup1.setvalue(1)
	scorereel1.resettozero
	scorereel2.resettozero
	for i = 1 to maxplayers
	    score(i)=0
		rep(i)=0
		EVAL("p100k"&i).setvalue 0
	next
	ebcount=0
	If B2SOn then
		Controller.B2SSetScore 1, score(1)
		Controller.B2SSetScore 2, score(2)
		Controller.B2SSetData 3,0
		Controller.B2SSetData 4,0
		Controller.B2SSetScoreRollover 25, 0
		Controller.B2SSetScoreRollover 26, 0
	End If
    eg=0
    gamov.text=" "
    tilttxt.text=" "
    If B2SOn then
		Controller.B2SSetGameOver 35,0
		Controller.B2SSetTilt 33,0
		Controller.B2SSetMatch 34,0
		Controller.B2ssetdata 11, 0
		Controller.B2ssetdata 7,0
		Controller.B2ssetdata 8,0
	End If
	bipreel.setvalue 1
	matchtxt.text=" "
	me.enabled=false
    ballreltimer.enabled=true
end sub

sub nextball
    if tilt=true then
	  bumper1.hashitevent=1
      tilt=false
      tilttxt.text=" "
		If B2SOn then
			Controller.B2SSetTilt 33,0
			Controller.B2ssetdata 1, 1
		End If
    end if
	if player=1 then ballinplay=ballinplay+1
	if ballinplay>balls then
		playsound "endgame"
		eg=1
        If EndGameFog = True Then
            EndDelay1.Enabled = True
            EndDelay2.Enabled = True
            EndDelay3.Enabled = True
            FogRandomSfx
        Else
            EndDelay1.Enabled = False
            EndDelay2.Enabled = False
            EndDelay3.Enabled = False
        End If
		ballreltimer.enabled=true
	  else
		if state=true and tilt=false then
		  ebcount=0
		  ballreltimer.enabled=true
		end if
		bipreel.setvalue ballinplay
		If B2SOn then Controller.B2ssetballinplay 32, Ballinplay
	end if
End Sub

sub ballreltimer_timer
  for each light in bonuslights: light.state=0: next
  if eg=1 then
	  turnoff
      matchnum
	  bipreel.setvalue 0
	  state=false
	  gamov.text="GAME OVER"
	  gamov.timerenabled=1
	  tilttxt.timerenabled=1
	  canplayreel.setvalue(0)
	  pup1.setvalue(0)
	  pup2.setvalue(0)
	  hiscstate=0
	  for i=1 to maxplayers
		if score(i)>hisc then
			hisc=score(i)
			hiscstate=1
		end if
		EVAL("Pup"&i).setvalue 0
	  next
	  if hiscstate=1 and showhisc=1 then
		HighScoreEntryInit()
		UpdatePostIt
		savehs
	  end if
	  If B2SOn then
        Controller.B2SSetGameOver 35,1
        Controller.B2ssetballinplay 32, 0
	    Controller.B2sStartAnimation "EOGame"
	    Controller.B2ssetcanplay 31, 0
	    Controller.B2ssetplayerup 30, 0
		Controller.B2SSetData 80,0
		Controller.B2SSetData 81,0
		Controller.B2SSetData 85,0
        If BackGlassBreak = True Then
		    Controller.B2SSetData 88,1
            PlaySound "Glass-Break"
        End If
	  End If
	  ballreltimer.enabled=false
      EndMusic
      If EndGameFog = False Then
          OutroMusic
  End If

  else

	L5k.state=0
	L1k.state=0
	LTrigLStar.state=0
	Special.state=0
	shootagain.state=0
	ExtraBall.state=0
	bonus=0
	starstate=0
	BumperLight1.State=1
	resetDT.enabled=1
	for i = 1 to 4
		abonus(1,i).state=0
	next
	if ballinplay=balls then bonusx2.state=1
	playsoundAtVol "drainkick", drain, 1
 	Drain.kick 60,45,0
	DOF 118, 2
    ballreltimer.enabled=false
  end if
end sub

sub matchnum
	matchnumb=INT (RND*10)
	if matchnumb=0 then
		matchtxt.text="00"
	  else
		matchtxt.text=matchnumb*10
	end if
  If B2SOn then Controller.B2SSetMatch 34,Matchnumb*10
  For i=1 to players
    if (matchnumb*10)=(score(i) mod 100) then
		addcredit
		playsound SoundFXDOF("knocker", 117, DOFPulse, DOFKnocker)
		DOF 116, DOFPulse
	end if
  next
end sub

sub flippertimer_timer()
	LFlip.RotY = LeftFlipper.CurrentAngle
	LFlip1.RotY = LeftFlipper.CurrentAngle-90
	RFlip.RotY = RightFlipper.CurrentAngle
	RFlip1.RotY = RightFlipper.CurrentAngle-90
    FlipperLSh.RotZ = LeftFlipper.currentangle
    FlipperRSh.RotZ = RightFlipper.currentangle
	PGate.Rotz = (gate1.CurrentAngle*.75) + 25
	BumperLightA1.state=bumperlight1.state
	Special1.state = Special.state
	L5k1.state = L5k.state
	L5k2.state = L5k.state
	L5k3.state = L5k.state
	L5k4.state = L5k.state
	L5k5.state = L5k.state
	L1k1.state = L1k.state
	LTrigRStar.state = LTrigLStar.state
end sub

sub coindelay_timer
	addcredit
    coindelay.enabled=false
end sub

Sub UpperKicker_Hit
	addscore 500
	addbonus
	if extraball.state=1 then
		playsound "ding1",0,.5
		if ebcount=0 then
			shootagain.state=lightstateon
			extraball.state=0
			ebcount=1
            If B2SOn then Controller.B2SSetShootAgain 36,1
		 end if
	end if
	me.uservalue=1
	me.timerenabled=1
End Sub

Sub UpperKicker_timer
	select case UpperKicker.uservalue
	  case 4:
		UpperKicker.Kick 173, 15
		PlaySoundAtVol SoundFXDOF("holekick",115,DOFPulse,DOFContactors), UpperKicker, VolKick
		DOF 116, DOFPulse
		Pkickarm.rotz=15
	  case 6:
		Pkickarm.rotz=0
		me.timerenabled=0
	end Select
	Upperkicker.uservalue=Upperkicker.uservalue+1
End Sub

Sub Drain_Hit()
	PlaySoundAtVol "drain", drain, 1
	DOF 119, DOFPulse
	if l5k.state=1 then
		fivek=1
	  else
		fivek=0
	end if
	if bonusx3.state=1 then
		dbonus=3
	  elseif bonusx2.state=1 then
			dbonus=2
		  else
			dbonus=1
    end if
	bonus=bonus*dbonus
	scorebonus.enabled=1
End Sub

sub ballhome_hit
	plungeball=1
	shootagain.state=lightstateoff
	If B2SOn then Controller.B2SSetShootAgain 36,0
end sub

sub ballhome_unhit
	DOF 120, DOFPulse
	plungeball=0
end sub

sub scorebonus_timer
   if tilt=true then bonus=0
	 'Else
		if bonus>0 then
			if bonus/2 = int(bonus/2) then
				BumperLight1.State=0
				if fivek=1 then
					l5k.state=0
				end if
			  else
				BumperLight1.State=1
				if fivek=1 then
					l5k.state=1
				end if
			end if
			if bonus/dbonus = int(bonus/dbonus) then
				if bonus/dbonus<19 then bonuslight((bonus/dbonus)+1).state=0
				if bonus > dbonus-1 then bonuslight((bonus/dbonus)).state=1
				bonus=bonus-1
				addpoints 1000
			  else
				bonus=bonus-1
				addpoints 1000
			  end if
		   else
			bonus=0
		end if
		if bonus=0 then
		  fivek=0
		  if shootagain.state=lightstateon then
			  ballreltimer.enabled=true
		   else
			  if players=1 or player=2 then
				player=1
				If B2SOn then 
				Controller.B2ssetplayerup 30, 1
                Controller.B2SSetData 80,1
                Controller.B2SSetData 81,0
		        Controller.B2SSetData 85,1
				End If
				pup1.setvalue(1)
				pup2.setvalue(0)
				nextball
			  else
				player=2
				If B2SOn then 
				Controller.B2ssetplayerup 30, 2
                Controller.B2SSetData 80,0
                Controller.B2SSetData 81,1
		        Controller.B2SSetData 85,1
				End If
				pup2.setvalue(1)
				pup1.setvalue(0)
				nextball
			  end if
		  end if
		  scorebonus.enabled=false
		end if
	'end if
End sub

sub awardcheck
	award=0
	for a = 4 to 11
		'if Targets(a).isdropped then award=award+1
		if TargetIsDropped(a) then award=award+1
	next
	if award = 8 then
		bonusx2.state=1
		extraball.state=1
		l1k.state=1
		l5k.state=1
		if ballinplay=balls then bonusx3.state=1
	end if
	award=0
	for a = 0 to 3
		'if targets(a).isdropped then award=award+1
		if TargetIsDropped(a) then award=award+1
	next
	if award = 4 then
		bonusx2.state=1
		l1k.state=1
		l5k.state=1
		if extraball.state=1 then special.state=1
		if ballinplay=balls then bonusx3.state=1
	end if
	award=0
	for a = 12 to 15
		'if Targets(a).isdropped then award=award+1
		if TargetIsDropped(a) then award=award+1
	next
	if award = 4 then
		bonusx2.state=1
		l1k.state=1
		l5k.state=1
		if extraball.state=1 then special.state=1
		if ballinplay=balls then bonusx3.state=1
	end if
end sub

'**** Bumper

Sub Bumper1_Hit
   if tilt=false then
    PlaySoundAtVol SoundFXDOF("fx_bumper4",107,DOFPulse,DOFContactors), Bumper1, VolBump
	DOF 108, DOFPulse
	if BumperLight1.state=1 then
		addscore bumperlitscore
	  else
		addscore bumperoffscore
	end if
	FlashLevel9 = 1 : FlasherFlash9_Timer:FlashLevel25 = 1 : FlasherFlash25_Timer
   end if
End Sub

sub FlashBumper
	BumperLight1.duration 0, 500, 1
end sub


'**** Wire Triggers

sub TrigLUpper_hit
	DOF 113, DOFPulse
	addscore 500
	addbonus
end sub

sub TrigRUpper_hit
	DOF 114, DOFPulse
	addscore 500
	addbonus
end sub

sub TrigLeftLane_hit
	DOF 110, DOFPulse
	if L1k.state=1 then
		addscore 1000
	  else
		addscore 100
	end if
end sub

sub TrigRightLane_hit
	DOF 111, DOFPulse
	if L1k.state=1 then
		addscore 1000
	  else
		addscore 100
	end if
end sub

sub TrigLOut_hit
	DOF 109, DOFPulse
	addbonus
	if L5k.state=1 then
		addscore 5000
	  else
		addscore 500
	end if
end sub

sub TrigROut_hit
	DOF 112, DOFPulse
	addbonus
	if L5k.state=1 then
		addscore 5000
	  else
		addscore 500
	end if
end sub

'**** Star Triggers

sub TrigRStar_hit
	if LTrigRStar.state=1 then
		addbonus
	  else
		starstate=starstate+1
		if starstate=5 then
			LTrigLStar.state=1
			abonus(player,4).state=0
		  else
			abonus(player,starstate).state=1
			if (starstate-1)>0 then abonus(player,(starstate-1)).state=0
		end if
	end if
end sub

sub TrigLStar_hit
	if LTrigLStar.state=1 then
		addbonus
	  else
		starstate=starstate+1
		if starstate=5 then
			LTrigLStar.state=1
			abonus(player,4).state=0
		  else
			abonus(player,starstate).state=1
			if (starstate-1)>0 then abonus(player,(starstate-1)).state=0
		end if
	end if
end sub


'**** Drop Targets

Sub Targets_Hit (idx)
	'debug.print "Target_Hit " & idx
	DTHit idx
	PlaySound "target", 0, Vol(ActiveBall)*VolTarg, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Targets_Drop(idx)
	'debug.print "Target_Drop " & idx
	Select Case idx

		Case 0: 'DTred1_dropped
		  if tilt=false then
			TargetIsDropped(0) = true
			playsoundAtVol SoundFXDOF("DropTarget_Down",103,DOFPulse,DOFContactors), DTred1p, VolTarg
			flashbumper
			addbonus
			if L5K.state=1 then
				addscore 5000
			  else
				addscore 500
			end if
			AwardCheck
			TLred1.state = 1
			TLred1b.state = 1
		  end if

		Case 1: 'DTred2_dropped
		  if tilt=false then
			TargetIsDropped(1) = true
			playsoundAtVol SoundFXDOF("DropTarget_Down",103,DOFPulse,DOFContactors), DTred2p, VolTarg
			flashbumper
			addbonus
			if L5K.state=1 then
				addscore 5000
			  else
				addscore 500
			end if
			AwardCheck
			TLred2.state=1
			TLred2b.state=1
		  end if

		Case 2: 'DTred3_dropped
		  if tilt=false then
			TargetIsDropped(2) = true
			playsoundAtVol SoundFXDOF("DropTarget_Down",103,DOFPulse,DOFContactors), DTred3p, VolTarg
			flashbumper
			addbonus
			if L5K.state=1 then
				addscore 5000
			  else
				addscore 500
			end if
			AwardCheck
			TLred3.state=1
		  end if

		Case 3: 'DTred4_dropped
		  if tilt=false then
			TargetIsDropped(3) = true
			playsoundAtVol SoundFXDOF("DropTarget_Down",103,DOFPulse,DOFContactors), DTred4p, VolTarg
			flashbumper
			addbonus
			if L5K.state=1 then
				addscore 5000
			  else
				addscore 500
			end if
			AwardCheck
				TLred4.state=1
		  end if


		Case 4: 'DTblack1_dropped
		  if tilt=false then
			TargetIsDropped(4) = true
			playsoundAtVol SoundFXDOF("DropTarget_Down",105,DOFPulse,DOFContactors), DTblack1p, VolTarg
			flashbumper
			addbonus
			if L5K.state=1 then
				addscore 5000
			  else
				addscore 500
			end if
			AwardCheck
			TLblack1.state=1
		  end if

		Case 5: 'DTblack2_dropped
		  if tilt=false then
			TargetIsDropped(5) = true
			playsoundAtVol SoundFXDOF("DropTarget_Down",105,DOFPulse,DOFContactors), DTblack2p, VolTarg
			flashbumper
			addbonus
			if L5K.state=1 then
				addscore 5000
			  else
				addscore 500
			end if
			AwardCheck
			TLblack2.state=1
		  end if

		Case 6: 'DTblack3_dropped
		  if tilt=false then
			TargetIsDropped(6) = true
			playsoundAtVol SoundFXDOF("DropTarget_Down",105,DOFPulse,DOFContactors), DTblack3p, VolTarg
			flashbumper
			addbonus
			if L5K.state=1 then
				addscore 5000
			  else
				addscore 500
			end if
			AwardCheck
			TLblack3.state=1
		  end if

		Case 7: 'DTblack4_dropped
		  if tilt=false then
			TargetIsDropped(7) = true
			playsoundAtVol SoundFXDOF("DropTarget_Down",105,DOFPulse,DOFContactors), DTblack4p, VolTarg
			flashbumper
			addbonus
			if L5K.state=1 then
				addscore 5000
			  else
				addscore 500
			end if
			AwardCheck
			TLblack4.state=1
		  end if

		Case 8: 'DTblack5_dropped
		  if tilt=false then
			TargetIsDropped(8) = true
			playsoundAtVol SoundFXDOF("DropTarget_Down",106,DOFPulse,DOFContactors), DTblack5p, VolTarg
			flashbumper
			addbonus
			if L5K.state=1 then
				addscore 5000
			  else
				addscore 500
			end if
			AwardCheck
			TLblack5.state=1
		  end if

		Case 9: 'DTblack6_dropped
		  if tilt=false then
			TargetIsDropped(9) = true
			playsoundAtVol SoundFXDOF("DropTarget_Down",106,DOFPulse,DOFContactors), DTblack6p, VolTarg
			flashbumper
			addbonus
			if L5K.state=1 then
				addscore 5000
			  else
				addscore 500
			end if
			AwardCheck
			TLblack6.state=1
		  end if

		Case 10: 'DTblack7_dropped
		  if tilt=false then
			TargetIsDropped(10) = true
			playsoundAtVol SoundFXDOF("DropTarget_Down",106,DOFPulse,DOFContactors), DTblack7p, VolTarg
			flashbumper
			addbonus
			if L5K.state=1 then
				addscore 5000
			  else
				addscore 500
			end if
			AwardCheck
			TLblack7.state=1
		  end if

		Case 11: 'DTblack8_dropped
		  if tilt=false then
			TargetIsDropped(11) = true
			playsoundAtVol SoundFXDOF("DropTarget_Down",106,DOFPulse,DOFContactors), DTblack8p, VolTarg
			flashbumper
			addbonus
			if L5K.state=1 then
				addscore 5000
			  else
				addscore 500
			end if
			AwardCheck
			TLblack8.state=1
		  end if

		Case 12: 'DTblue1_hit
		  if tilt=false then
			TargetIsDropped(12) = true
			playsoundAtVol SoundFXDOF("DropTarget_Down",104,DOFPulse,DOFContactors), DTblue1p, VolTarg
			flashbumper
			addbonus
			if L5K.state=1 then
				addscore 5000
			  else
				addscore 500
			end if
			AwardCheck
			TLblue1.state=1
		  end if

		Case 13: 'DTblue2_hit
		  if tilt=false then
			TargetIsDropped(13) = true
			playsoundAtVol SoundFXDOF("DropTarget_Down",104,DOFPulse,DOFContactors), DTblue2p, VolTarg
			flashbumper
			addbonus
			if L5K.state=1 then
				addscore 5000
			  else
				addscore 500
			end if
			AwardCheck
			TLblue2.state=1
		  end if

		Case 14: 'DTblue3_hit
		  if tilt=false then
			TargetIsDropped(14) = true
			playsoundAtVol SoundFXDOF("DropTarget_Down",104,DOFPulse,DOFContactors), DTblue3p, VolTarg
			flashbumper
			addbonus
			if L5K.state=1 then
				addscore 5000
			  else
				addscore 500
			end if
			AwardCheck
			TLblue3.state=1
			TLblue3b.state=1
		  end if

		Case 15: 'DTblue4_hit
		  if tilt=false then
			TargetIsDropped(15) = true
			playsoundAtVol SoundFXDOF("DropTarget_Down",104,DOFPulse,DOFContactors), DTblue4p, VolTarg
			flashbumper
			addbonus
			if L5K.state=1 then
				addscore 5000
			  else
				addscore 500
			end if
			AwardCheck
			TLblue4.state=1
			TLblue4b.state=1
		  end if

	End Select
End Sub




' Thalamus, not access to this table at the moment, kind of guessing that the targets
' reset only once all are down.

Sub resetDT_timer
	playsoundAtVol SoundFXDOF("bankreset",126,DOFPulse,DOFContactors), DTred4p, VolTarg
  PlaySoundAtVol "bankreset", DTblue4p, VolTarg
  PlaySoundAtVol "bankreset", DTblack4p, VolTarg
  PlaySoundAtVol "bankreset", DTblack8p, VolTarg
	for j = 0 to 15
		DTRaise j
		TargetIsDropped(j) = false
		'targets(j).isdropped = false
	next
	for each light in dtlights:light.state=0:next
	me.enabled=0
end sub

'********Rubber walls

sub DingwallI_Hit   'top left rubber by kicker
	if state=true and tilt=false then
		addscore 10
		if LTrigLStar.state=0 then
			starstate=starstate+1
			if starstate=5 then
				LTrigLStar.state=1
				abonus(player,4).state=0
			  else
				abonus(player,starstate).state=1
				if (starstate-1)>0 then abonus(player,(starstate-1)).state=0
			end if
		end if
	END IF
	SlingI.visible=0
	SlingG.visible=0
	SlingI1.visible=1
	me.uservalue=1
	Me.timerenabled=1
end sub

sub dingwallI_timer									'default 50 timer
	select case dingwallI.uservalue
		Case 1: SlingI1.visible=0: SlingI.visible=1
		case 2:	SlingI.visible=0: SlingI2.visible=1
		Case 3: SlingI2.visible=0: SlingI.visible=1: SlingG.visible=1: Me.timerenabled=0
	end Select
	me.uservalue=me.uservalue+1
end sub

sub DingwallJ_Hit    ' top right rubber by kicker
	if state=true and tilt=false then
		addscore 10
		if LTrigLStar.state=0 then
			starstate=starstate+1
			if starstate=5 then
				LTrigLStar.state=1
				abonus(player,4).state=0
			  else
				abonus(player,starstate).state=1
				if (starstate-1)>0 then abonus(player,(starstate-1)).state=0
			end if
		end if
	END IF
	SlingJ.visible=0
	SlingH.visible=0
	SlingJ1.visible=1
	me.uservalue=1
	Me.timerenabled=1
end sub

sub dingwallJ_timer									'default 50 timer
	select case dingwallJ.uservalue
		Case 1: SlingJ1.visible=0: SlingJ.visible=1
		case 2:	SlingJ.visible=0: SlingJ2.visible=1
		Case 3: SlingJ2.visible=0: SlingJ.visible=1: SlingH.visible=1: Me.timerenabled=0
	end Select
	me.uservalue=me.uservalue+1
end sub

sub dingwalla_hit
	if state=true and tilt=false then addscore 10
	SlingA.visible=0
	SlingA1.visible=1
	me.uservalue=1
	Me.timerenabled=1
end sub

sub dingwalla_timer									'default 50 timer
	select case dingwalla.uservalue
		Case 1: SlingA1.visible=0: SlingA.visible=1
		case 2:	SlingA.visible=0: SlingA2.visible=1
		Case 3: SlingA2.visible=0: SlingA.visible=1: Me.timerenabled=0
	end Select
	me.uservalue=me.uservalue+1
end sub

sub dingwallb_hit
	if state=true and tilt=false then addscore 10
	SlingB.visible=0
	SlingB1.visible=1
	me.uservalue=1
	Me.timerenabled=1
end sub

sub dingwallb_timer									'default 50 timer
	select case DingwallB.uservalue
		Case 1: Slingb1.visible=0: SlingB.visible=1
		case 2:	SlingB.visible=0: Slingb2.visible=1
		Case 3: Slingb2.visible=0: SlingB.visible=1: Me.timerenabled=0
	end Select
	DingwallB.uservalue=DingwallB.uservalue+1
end sub

sub dingwallc_hit
	if state=true and tilt=false then addscore 10
	Slingc.visible=0
	Slingc1.visible=1
	me.uservalue=1
	Me.timerenabled=1
end sub

sub dingwallc_timer									'default 50 timer
	select case DingwallC.uservalue
		Case 1: SlingC1.visible=0: SlingC.visible=1
		case 2:	SlingC.visible=0: SlingC2.visible=1
		Case 3: SlingC2.visible=0: SlingC.visible=1: Me.timerenabled=0
	end Select
	DingwallC.uservalue=DingwallC.uservalue+1
end sub

sub dingwallD_hit
	if state=true and tilt=false then addscore 10
	SlingD.visible=0
	SlingD1.visible=1
	DingwallD.uservalue=1
	Me.timerenabled=1
end sub

sub dingwallD_timer									'default 50 timer
	select case DingwallD.uservalue
		Case 1: SlingD1.visible=0: SlingD.visible=1
		case 2:	SlingD.visible=0: SlingD2.visible=1
		Case 3: SlingD2.visible=0: SlingD.visible=1: Me.timerenabled=0
	end Select
	DingwallD.uservalue=DingwallD.uservalue+1
end sub

sub dingwallE_hit
	if state=true and tilt=false then addscore 10
	SlingE.visible=0
	SlingE1.visible=1
	Me.uservalue=1
	Me.timerenabled=1
end sub

sub dingwallE_timer									'default 50 timer
	select case DingwallE.uservalue
		Case 1: SlingE1.visible=0: SlingE.visible=1
		case 2:	SlingE.visible=0: SlingE2.visible=1
		Case 3: SlingE2.visible=0: SlingE.visible=1: Me.timerenabled=0
	end Select
	DingwallE.uservalue=DingwallE.uservalue+1
end sub

sub dingwallF_hit
	if state=true and tilt=false then addscore 10
	SlingF.visible=0
	SlingF1.visible=1
	Me.uservalue=1
	Me.timerenabled=1
end sub

sub dingwallF_timer									'default 50 timer
	select case DingwallF.uservalue
		Case 1: SlingF1.visible=0: SlingF.visible=1
		case 2:	SlingF.visible=0: SlingF2.visible=1
		Case 3: SlingF2.visible=0: SlingF.visible=1: Me.timerenabled=0
	end Select
	DingwallF.uservalue=DingwallF.uservalue+1
end sub

sub dingwallG_hit
	if state=true and tilt=false then addscore 10
	SlingG.visible=0
	SlingI.visible=0
	SlingG1.visible=1
	Me.uservalue=1
	Me.timerenabled=1
end sub

sub dingwallG_timer									'default 50 timer
	select case DingwallG.uservalue
		Case 1: SlingG1.visible=0: SlingG.visible=1
		case 2:	SlingG.visible=0: SlingG2.visible=1
		Case 3: SlingG2.visible=0: SlingG.visible=1: SlingI.visible=1: Me.timerenabled=0
	end Select
	DingwallG.uservalue=DingwallG.uservalue+1
end sub

sub dingwallH_hit
	if state=true and tilt=false then addscore 10
	SlingH.visible=0
	SlingJ.visible=0
	SlingH1.visible=1
	Me.uservalue=1
	Me.timerenabled=1
end sub

sub dingwallH_timer									'default 50 timer
	select case DingwallH.uservalue
		Case 1: SlingH1.visible=0: SlingH.visible=1
		case 2:	SlingH.visible=0: SlingH2.visible=1
		Case 3: SlingH2.visible=0: SlingH.visible=1: SlingJ.visible=1: Me.timerenabled=0
	end Select
	DingwallH.uservalue=DingwallH.uservalue+1
end sub

'******************Scoring

sub addscore(points)
  if tilt=false and state=true then
    If Points < 100 and AddScore10Timer.enabled = false Then
        Add10 = Points \ 10
        AddScore10Timer.Enabled = TRUE
      ElseIf Points < 1000 and AddScore100Timer.enabled = false Then
        Add100 = Points \ 100
        AddScore100Timer.Enabled = TRUE
      ElseIf AddScore1000Timer.enabled = false Then
        Add1000 = Points \ 1000
        AddScore1000Timer.Enabled = TRUE
    End If
  end if
End Sub

Sub AddScore10Timer_Timer()
    if Add10 > 0 then
        AddPoints 10
        Add10 = Add10 - 1
    Else
        Me.Enabled = FALSE
    End If
End Sub

Sub AddScore100Timer_Timer()
    if Add100 > 0 then
        AddPoints 100
        Add100 = Add100 - 1
    Else
        Me.Enabled = FALSE
    End If
End Sub

Sub AddScore1000Timer_Timer()
    if Add1000 > 0 then
        AddPoints 1000
        Add1000 = Add1000 - 1
    Else
        Me.Enabled = FALSE
    End If
End Sub

Sub AddPoints(Points)
    score(player)=score(player)+points
	sreels(player).addvalue(points)
	If B2SOn Then Controller.B2SSetScorePlayer player, score(player) MOD 100000

    ' Sounds: there are 3 sounds: tens, hundreds and thousands
    If Points = 100 AND(Score(player) MOD 1000) \ 100 = 0 Then  'New 1000 reel
        PlaySound SoundFXDOF("bell1000",125,DOFPulse,DOFChimes)
      ElseIf Points = 10 AND(Score(player) MOD 100) \ 10 = 0 Then 'New 100 reel
        PlaySound SoundFXDOF("bell100",124,DOFPulse,DOFChimes)
      ElseIf points = 1000 Then
        PlaySound SoundFXDOF("bell1000",125,DOFPulse,DOFChimes)
	  elseif Points = 100 Then
        PlaySound SoundFXDOF("bell100",124,DOFPulse,DOFChimes)
      Else
        PlaySound SoundFXDOF("bell10",123,DOFPulse,DOFChimes)

    End If
	checkreplay
end sub

sub checkreplay
	if score(player)>99999 then
		If B2SOn then Controller.B2SSetScoreRollover 24 + player, 1
		EVAL("p100k"&player).setvalue 1   'int(score(player)/100000)
	End if
    if score(player)=>replay1 and rep(player)=0 then
		addreplay
		rep(player)=1
    end if
    if score(player)=>replay2 and rep(player)=1 then
		addreplay
		rep(player)=2
    end if
    if score(player)=>replay3 and rep(player)=2 then
		addreplay
		rep(player)=3
    end if
end sub

Sub addreplay
	addcredit
	PlaySound SoundFXDOF("knocker",117,DOFPulse,DOFKnocker)
	DOF 116, DOFPulse
End sub

Sub CheckTilt
	If Tilttimer.Enabled = True Then
	 TiltSens = TiltSens + 1
	 if TiltSens = 3 Then GameTilted
	Else
	 TiltSens = 0
	 Tilttimer.Enabled = True
	End If
End Sub

Sub GameTilted
	Tilt = True
	tilttxt.text="TILT"
    If B2SOn Then Controller.B2SSetTilt 33,1
    If B2SOn Then Controller.B2ssetdata 1, 0
	playsound "tilt"
	turnoff
End Sub

Sub Tilttimer_Timer()
	Tilttimer.Enabled = False
End Sub

sub turnoff
    bumper1.hashitevent=0
	LeftFlipper.RotateToStart
	LeftFlipper1.RotateToStart
	TLLFlip.state=0
	DOF 101, DOFOff
	StopSound "Buzz"
	RightFlipper.RotateToStart
	RightFlipper1.RotateToStart
	TLRFlip.state=0
	DOF 102, DOFOff
	StopSound "Buzz1"
end sub

Sub addbonus
	if state=true and tilt=false then
		bonus=bonus+1
		if bonus>19 then bonus=19
		if  bonus = 1 then
			bonuslight(bonus).state=1
		  else
			bonuslight(bonus).state=1
			bonuslight(bonus-1).state=0
			if bonus>10 then bonuslight(10).state=1
		End if
	end if
End sub

sub addcredit
	if freeplay=0 then
	  credit=credit+1
	  DOF 121, DOFOn
      if credit>15 then credit=15
	  creditreel.setvalue credit
	  If B2SOn Then Controller.B2ssetCredits Credit
	end if
end sub

sub savehs
	Dim FileObj
	Dim ScoreFile
	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		Exit Sub
	End if
	Set ScoreFile=FileObj.CreateTextFile(UserDirectory & HSFileName,True)
    ScoreFile.WriteLine credit
    ScoreFile.WriteLine hisc
    ScoreFile.WriteLine matchnumb
    ScoreFile.WriteLine score(1)
    ScoreFile.WriteLine score(2)
	ScoreFile.WriteLine replays
	ScoreFile.WriteLine balls
	ScoreFile.WriteLine freeplay
	ScoreFile.WriteLine HSA1
	ScoreFile.WriteLine HSA2
	ScoreFile.WriteLine HSA3
    ScoreFile.WriteLine showhisc
    ScoreFile.Close
	Set ScoreFile=Nothing
	Set FileObj=Nothing
end sub

sub loadhs
    Dim FileObj
	Dim ScoreFile
    dim temp1
    dim temp2
    dim temp3
    dim temp4
    dim temp5
    dim temp6
    dim temp7
    dim temp8
    dim temp9
    dim temp10
    dim temp11
    dim temp12
    Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		Exit Sub
	End if
	If Not FileObj.FileExists(UserDirectory & HSFileName) then
		Exit Sub
	End if
	Set ScoreFile=FileObj.GetFile(UserDirectory & HSFileName)
	Set TextStr=ScoreFile.OpenAsTextStream(1,0)
		If (TextStr.AtEndOfStream=True) then
			Exit Sub
        End if
		temp1=textStr.readLine
		temp2=textstr.readline
		temp3=textstr.readline
		temp4=textstr.readline
		temp5=textstr.readline
		temp6=textstr.readline
		temp7=textstr.readline
		temp8=textStr.readLine
		temp9=textstr.readline
		temp10=textstr.readline
		temp11=textstr.readline
		temp12=textstr.readline
	TextStr.Close
     credit = CDbl(temp1)
     hisc = CDbl(temp2) 
     matchnumb = CDbl(temp3)
     score(1) = CDbl(temp4)
     score(2) = CDbl(temp5)
     replays = CDbl(temp6)
     balls = CDbl(temp7)
     freeplay = CDbl(temp8)
     HSA1 = CDbl(temp9)
     HSA2 = CDbl(temp10)
     HSA3 = CDbl(temp11)
     showhisc = CDbl(temp12)
     Set ScoreFile=Nothing
	 Set FileObj=Nothing
end sub

Sub Table1_Exit()
	turnoff
	Savehs
	If B2SOn Then Controller.stop
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

Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall)*VolPi, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

'Sub Targets_Hit (idx)
'	PlaySound "target", 0, Vol(ActiveBall)*VolTarg, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
'End Sub

Sub Metals_Thin_Hit (idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall)*VolMetal, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Medium_Hit (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall)*VolMetal, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals2_Hit (idx)
	PlaySound "metalhit2", 0, Vol(ActiveBall)*VolMetal, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Gates_Hit (idx)
	PlaySound "gate4", 0, Vol(ActiveBall)*VolGates, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Spinner_Spin
	PlaySoundAtVol "fx_spinner", Spinner, VolSpin
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then
		PlaySound "fx_rubber2", 0, Vol(ActiveBall)*VolRH, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then
		PlaySound "fx_rubber2", 0, Vol(ActiveBall)*VolPo, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall)*VolRH, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall)*VolRH, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall)*VolRH, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

Sub LeftFlipper_Collide(parm)
 	RandomSoundFlipper()
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	'LeftFlipperCollide parm
	if RubberizerEnabled = 1 then Rubberizer(parm)
	if RubberizerEnabled = 2 then Rubberizer2(parm)
End Sub

Sub RightFlipper_Collide(parm)
 	RandomSoundFlipper()
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
	'RightFlipperCollide parm
	if RubberizerEnabled = 1 then Rubberizer(parm)
	if RubberizerEnabled = 2 then Rubberizer2(parm)
End Sub

Sub LeftFlipper1_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RightFlipper1_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall)*VolRH, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall)*VolRH, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall)*VolRH, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

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
	HSScorex = hisc
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
'	if showhisc=1 and showhiscnames=1 then
'		for each objekt in hiscname:objekt.visible=1:next
		HSName1.image = ImgFromCode(HSA1, 1)
		HSName2.image = ImgFromCode(HSA2, 2)
		HSName3.image = ImgFromCode(HSA3, 3)
'	  else
'		for each objekt in hiscname:objekt.visible=0:next
'	end if
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
'				EMReelHSName1.SetValue HSA1:EMReelHSName2.SetValue HSA2
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
					savehs 'enter it
					HighScoreFlashTimer.Enabled = False
					HSEnterMode = False
				End If
		End Select
		UpdatePostIt
    End If
End Sub

' *******************************************************************************************************
' Positional Sound Playback Functions by DJRobX
' PlaySound sound, 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
' *******************************************************************************************************

' Play a sound, depending on the X,Y position of the table element (especially cool for surround speaker setups, otherwise stereo panning only)
' parameters (defaults): loopcount (1), volume (1), randompitch (0), pitch (0), useexisting (0), restart (1))
' Note that this will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position

Sub PlayXYSound(soundname, tableobj, loopcount, volume, randompitch, pitch, useexisting, restart)
  PlaySound soundname, loopcount, volume, AudioPan(tableobj), randompitch, pitch, useexisting, restart, AudioFade(tableobj)
End Sub

' Set position as table object (Use object or light but NOT wall) and Vol to 1

Sub PlaySoundAt(soundname, tableobj)
  PlaySound soundname, 1, 1, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'Set all as per ball position & speed.

Sub PlaySoundAtBall(soundname)
  PlaySoundAt soundname, ActiveBall
End Sub

'Set position as table object and Vol manually.

Sub PlaySoundAtVol(sound, tableobj, Volum)
  PlaySound sound, 1, Volum, Pan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'Set all as per ball position & speed, but Vol Multiplier may be used eg; PlaySoundAtBallVol "sound",3

Sub PlaySoundAtBallVol(sound, VolMult)
  PlaySound sound, 0, Vol(ActiveBall) * VolMult, Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub

'Set position as bumperX and Vol manually.

Sub PlaySoundAtBumperVol(sound, tableobj, Vol)
  PlaySound sound, 1, Vol, Pan(tableobj), 0,0,1, 1, AudioFade(tableobj)
End Sub

'*********************************************************************
'                     Supporting Ball & Sound Functions
'*********************************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "SpaceWalk" is the name of the table
  Dim tmp
  tmp = tableobj.y * 2 / Table1.height-1
  If tmp > 0 Then
    AudioFade = Csng(tmp ^10)
  Else
    AudioFade = Csng(-((- tmp) ^10) )
  End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "SpaceWalk" is the name of the table
  Dim tmp
  tmp = tableobj.x * 2 / Table1.width-1
  If tmp > 0 Then
    AudioPan = Csng(tmp ^10)
  Else
    AudioPan = Csng(-((- tmp) ^10) )
  End If
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "SpaceWalk" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / Table1.width-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioFade(ball) ' Can this be together with the above function ?
  Dim tmp
  tmp = ball.y * 2 / Table1.height-1
  If tmp > 0 Then
    AudioFade = Csng(tmp ^10)
  Else
    AudioFade = Csng(-((- tmp) ^10) )
  End If
End Function

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
  Vol = Csng(BallVel(ball) ^2 / VolDiv)
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

Const tnob = 2 ' total number of balls
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
      If BallVel(BOT(b) ) > 1 Then
        rolling(b) = True
        if BOT(b).z < 30 Then ' Ball on playfield
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) ), Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
        Else ' Ball on raised ramp
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )*.5, Pan(BOT(b) ), 0, Pitch(BOT(b) )+50000, 1, 0, AudioFade(BOT(b) )
        End If
      Else
        If rolling(b) = True Then
          StopSound("fx_ballrolling" & b)
          rolling(b) = False
        End If
      End If
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / (VolDiv/VolCol), Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

'**********************************************
'STUFF ADDED BY RASCAL WILL BE BELOW THIS LINE
'**********************************************

'Detect if table is NOT in desktop mode and turn off backdrop displays when in FS mode
Dim DTMode
If Table1.ShowDT = False Then
	For Each DTMode in dtdisplay
		DTMode.Visible = False
	Next
Else
	For Each DTMode in dtdisplay
		DTMode.Visible = True
	Next
End If
	
'The following timer code does the bumper flasher fading pulse
Dim bumperfade
Flasher001.Opacity=0
Sub bumperpulse_Timer()
	If Flasher001.Opacity=1500 Then bumperfade=-1
	If Flasher001.Opacity=0 Then bumperfade=1
	Flasher001.Opacity = Flasher001.Opacity + bumperfade
End Sub

Flasherlight9.IntensityScale = 0
'*** yellow flasher ***
Dim FlashLevel9, FlashLevel25
Sub FlasherFlash9_Timer()
	dim flashx3, matdim
	If not Flasherflash9.TimerEnabled Then 
		Flasherflash9.TimerEnabled = True
		Flasherflash9.visible = 1
		Flasherlit9.visible = 1
	End If
	flashx3 = FlashLevel9 * FlashLevel9 * FlashLevel9
	Flasherflash9.opacity = 8000 * flashx3
	Flasherlit9.BlendDisableLighting = 10 * flashx3
	Flasherbase9.BlendDisableLighting =  flashx3
	Flasherlight9.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel9)
	Flasherlit9.material = "domelit" & matdim
	FlashLevel9 = FlashLevel9 * 0.85 - 0.01
	If FlashLevel9 < 0.15 Then
		Flasherlit9.visible = 0
	Else
		Flasherlit9.visible = 1
	end If
	If FlashLevel9 < 0 Then
		Flasherflash9.TimerEnabled = False
		Flasherflash9.visible = 0
	End If
End Sub

'*** Red flasher ***
Sub FlasherFlash25_Timer()
	dim flashx3, matdim
	If not Flasherflash25.TimerEnabled Then 
		Flasherflash25.TimerEnabled = True
		Flasherflash25.visible = 1
		Flasherlit25.visible = 1
	End If
	flashx3 = FlashLevel25 * FlashLevel25 * FlashLevel25
	Flasherflash25.opacity = 1500 * flashx3
	Flasherlit25.BlendDisableLighting = 10 * flashx3
	Flasherbase25.BlendDisableLighting =  flashx3
	Flasherlight25.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel25)
	Flasherlit25.material = "domelit" & matdim
	FlashLevel25 = FlashLevel25 * 0.9 - 0.01
	If FlashLevel25 < 0.15 Then
		Flasherlit25.visible = 0
	Else
		Flasherlit25.visible = 1
	end If
	If FlashLevel25 < 0 Then
		Flasherflash25.TimerEnabled = False
		Flasherflash25.visible = 0
	End If
End Sub
'------------------------------------------------------------------------------------------


changeflippers
Sub changeflippers
	If alternateflippers=True Then
		Lflip.Image="flipper_gottlieb_left2-green"
		Rflip.Image="flipper_gottlieb_right2-green"
        Lflip1.Image="gottlieb_miniflipper-L-green"
        Rflip1.Image="gottlieb_miniflipper-R-green"
	Else
		Lflip.Image="flipper_gottlieb_left"
		Rflip.Image="flipper_gottlieb_right"
        Lflip1.Image="gottlieb_miniflipper_left"
        Rflip1.Image="gottlieb_miniflipper_right"
	End If
End Sub

'*********** NUNIZZU'S BALL SHADOW ************

Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2)

Sub BallShadowUpdate_timer()
Dim BOT, b
BOT = GetBalls

      If AddBallShadow=1 Then
         If UBound(BOT)<(tnob-1) Then
         For b = (UBound(BOT) + 1) to (tnob-1)
         BallShadow(b).visible = 0
         Next
         End If
         If UBound(BOT) = -1 Then Exit Sub
         For b = 0 to UBound(BOT)
         If BOT(b).X < Table1.Width/2 Then
         BallShadow(b).X = ((BOT(b).X) - (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) + 10
         Else
         BallShadow(b).X = ((BOT(b).X) + (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) - 10
         End If
         ballShadow(b).Y = BOT(b).Y + 20
         If BOT(b).Z > 20 Then
         BallShadow(b).visible = 1
         Else
         BallShadow(b).visible = 0
         End If
        Next
       End If
End Sub

'*********** FLIPPER SHADOWS ************

EnableFlipperShadows
Sub EnableFlipperShadows
       if AddFlipperShadows = 1 Then
        FlipperLSh.visible=true
		FLipperRSh.visible=true
       Else
        FlipperLSh.visible=false
		FLipperRSh.visible=false
End If
End Sub






'******************************************************
'****  FLIPPER CORRECTIONS by nFozzy
'******************************************************
'
' There are several steps for taking advantage of nFozzy's flipper solution.  At a high level well need the following:
'	1. flippers with specific physics settings
'	2. custom triggers for each flipper (TriggerLF, TriggerRF)
'	3. an object or point to tell the script where the tip of the flipper is at rest (EndPointLp, EndPointRp)
'	4. and, special scripting
'
' A common mistake is incorrect flipper length.  A 3-inch flipper with rubbers will be about 3.125 inches long.  
' This translates to about 147 vp units.  Therefore, the flipper start radius + the flipper length + the flipper end 
' radius should  equal approximately 147 vp units. Another common mistake is is that sometimes the right flipper
' angle was set with a large postive value (like 238 or something). It should be using negative value (like -122).
'
' The following settings are a solid starting point for various eras of pinballs.
' |                    | EM's           | late 70's to mid 80's | mid 80's to early 90's | mid 90's and later |
' | ------------------ | -------------- | --------------------- | ---------------------- | ------------------ |
' | Mass               | 1              | 1                     | 1                      | 1                  |
' | Strength           | 500-1000 (750) | 1400-1600 (1500)      | 2000-2600              | 3200-3300 (3250)   |
' | Elasticity         | 0.88           | 0.88                  | 0.88                   | 0.88               |
' | Elasticity Falloff | 0.15           | 0.15                  | 0.15                   | 0.15               |
' | Fricition          | 0.8-0.9        | 0.9                   | 0.9                    | 0.9                |
' | Return Strength    | 0.11           | 0.09                  | 0.07                   | 0.055              |
' | Coil Ramp Up       | 2.5            | 2.5                   | 2.5                    | 2.5                |
' | Scatter Angle      | 0              | 0                     | 0                      | 0                  |
' | EOS Torque         | 0.3            | 0.3                   | 0.275                  | 0.275              |
' | EOS Torque Angle   | 4              | 4                     | 6                      | 6                  |
'


'******************************************************
' Flippers Polarity (Select appropriate sub based on era) 
'******************************************************

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity

InitPolarity


'*******************************************
' Late 70's to early 80's

Sub InitPolarity()
        dim x, a : a = Array(LF, RF)
        for each x in a
                x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
                x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
                x.enabled = True
                x.TimeDelay = 80
        Next

        AddPt "Polarity", 0, 0, 0
        AddPt "Polarity", 1, 0.05, -2.7        
        AddPt "Polarity", 2, 0.33, -2.7
        AddPt "Polarity", 3, 0.37, -2.7        
        AddPt "Polarity", 4, 0.41, -2.7
        AddPt "Polarity", 5, 0.45, -2.7
        AddPt "Polarity", 6, 0.576,-2.7
        AddPt "Polarity", 7, 0.66, -1.8
        AddPt "Polarity", 8, 0.743, -0.5
        AddPt "Polarity", 9, 0.81, -0.5
        AddPt "Polarity", 10, 0.88, 0

        addpt "Velocity", 0, 0,         1
        addpt "Velocity", 1, 0.16, 1.06
        addpt "Velocity", 2, 0.41,         1.05
        addpt "Velocity", 3, 0.53,         1'0.982
        addpt "Velocity", 4, 0.702, 0.968
        addpt "Velocity", 5, 0.95,  0.968
        addpt "Velocity", 6, 1.03,         0.945

        LF.Object = LeftFlipper        
        LF.EndPoint = EndPointLp
        RF.Object = RightFlipper
        RF.EndPoint = EndPointRp
End Sub



' Flipper trigger hit subs
Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub




'******************************************************
'  FLIPPER CORRECTION FUNCTIONS
'******************************************************

Sub AddPt(aStr, idx, aX, aY)        'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LF, RF)
	dim x : for each x in a
		x.addpoint aStr, idx, aX, aY
	Next
End Sub

Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt        'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay        'delay before trigger turns off and polarity is disabled TODO set time!
	private Flipper, FlipperStart,FlipperEnd, FlipperEndY, LR, PartialFlipCoef
	Private Balls(20), balldata(20)

	dim PolarityIn, PolarityOut
	dim VelocityIn, VelocityOut
	dim YcoefIn, YcoefOut
	Public Sub Class_Initialize 
		redim PolarityIn(0) : redim PolarityOut(0) : redim VelocityIn(0) : redim VelocityOut(0) : redim YcoefIn(0) : redim YcoefOut(0)
		Enabled = True : TimeDelay = 50 : LR = 1:  dim x : for x = 0 to uBound(balls) : balls(x) = Empty : set Balldata(x) = new SpoofBall : next 
	End Sub

	Public Property let Object(aInput) : Set Flipper = aInput : StartPoint = Flipper.x : End Property
	Public Property Let StartPoint(aInput) : if IsObject(aInput) then FlipperStart = aInput.x else FlipperStart = aInput : end if : End Property
	Public Property Get StartPoint : StartPoint = FlipperStart : End Property
	Public Property Let EndPoint(aInput) : FlipperEnd = aInput.x: FlipperEndY = aInput.y: End Property
	Public Property Get EndPoint : EndPoint = FlipperEnd : End Property        
	Public Property Get EndPointY: EndPointY = FlipperEndY : End Property

	Public Sub AddPoint(aChooseArray, aIDX, aX, aY) 'Index #, X position, (in) y Position (out) 
		Select Case aChooseArray
			case "Polarity" : ShuffleArrays PolarityIn, PolarityOut, 1 : PolarityIn(aIDX) = aX : PolarityOut(aIDX) = aY : ShuffleArrays PolarityIn, PolarityOut, 0
			Case "Velocity" : ShuffleArrays VelocityIn, VelocityOut, 1 :VelocityIn(aIDX) = aX : VelocityOut(aIDX) = aY : ShuffleArrays VelocityIn, VelocityOut, 0
			Case "Ycoef" : ShuffleArrays YcoefIn, YcoefOut, 1 :YcoefIn(aIDX) = aX : YcoefOut(aIDX) = aY : ShuffleArrays YcoefIn, YcoefOut, 0
		End Select
		if gametime > 100 then Report aChooseArray
	End Sub 

	Public Sub Report(aChooseArray)         'debug, reports all coords in tbPL.text
		if not DebugOn then exit sub
		dim a1, a2 : Select Case aChooseArray
			case "Polarity" : a1 = PolarityIn : a2 = PolarityOut
			Case "Velocity" : a1 = VelocityIn : a2 = VelocityOut
			Case "Ycoef" : a1 = YcoefIn : a2 = YcoefOut 
				case else :tbpl.text = "wrong string" : exit sub
		End Select
		dim str, x : for x = 0 to uBound(a1) : str = str & aChooseArray & " x: " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		tbpl.text = str
	End Sub

	Public Sub AddBall(aBall) : dim x : for x = 0 to uBound(balls) : if IsEmpty(balls(x)) then set balls(x) = aBall : exit sub :end if : Next  : End Sub

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

	Public Sub Fire() 
		Flipper.RotateToEnd
		processballs
	End Sub

	Public Property Get Pos 'returns % position a ball. For debug stuff.
		dim x : for x = 0 to uBound(balls)
			if not IsEmpty(balls(x) ) then
				pos = pSlope(Balls(x).x, FlipperStart, 0, FlipperEnd, 1)
			End If
		Next                
	End Property

	Public Sub ProcessBalls() 'save data of balls in flipper range
		FlipAt = GameTime
		dim x : for x = 0 to uBound(balls)
			if not IsEmpty(balls(x) ) then
				balldata(x).Data = balls(x)
			End If
		Next
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))
		PartialFlipCoef = abs(PartialFlipCoef-1)
	End Sub
	Private Function FlipperOn() : if gameTime < FlipAt+TimeDelay then FlipperOn = True : End If : End Function        'Timer shutoff for polaritycorrect

	Public Sub PolarityCorrect(aBall)
		if FlipperOn() then 
			dim tmp, BallPos, x, IDX, Ycoef : Ycoef = 1

			'y safety Exit
			if aBall.VelY > -8 then 'ball going down
				RemoveBall aBall
				exit Sub
			end if

			'Find balldata. BallPos = % on Flipper
			for x = 0 to uBound(Balls)
				if aBall.id = BallData(x).id AND not isempty(BallData(x).id) then 
					idx = x
					BallPos = PSlope(BallData(x).x, FlipperStart, 0, FlipperEnd, 1)
					if ballpos > 0.65 then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)                                'find safety coefficient 'ycoef' data
				end if
			Next

			If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
				BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
				if ballpos > 0.65 then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)                                                'find safety coefficient 'ycoef' data
			End If

			'Velocity correction
			if not IsEmpty(VelocityIn(0) ) then
				Dim VelCoef
				VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)

				if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)

				if Enabled then aBall.Velx = aBall.Velx*VelCoef
				if Enabled then aBall.Vely = aBall.Vely*VelCoef
			End If

			'Polarity Correction (optional now)
			if not IsEmpty(PolarityIn(0) ) then
				If StartPoint > EndPoint then LR = -1        'Reverse polarity if left flipper
				dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR

				if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
			End If
		End If
		RemoveBall aBall
	End Sub
End Class

'******************************************************
'  FLIPPER POLARITY AND RUBBER DAMPENER SUPPORTING FUNCTIONS 
'******************************************************

' Used for flipper correction and rubber dampeners
Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
	dim x, aCount : aCount = 0
	redim a(uBound(aArray) )
	for x = 0 to uBound(aArray)        'Shuffle objects in a temp array
		if not IsEmpty(aArray(x) ) Then
			if IsObject(aArray(x)) then 
				Set a(aCount) = aArray(x)
			Else
				a(aCount) = aArray(x)
			End If
			aCount = aCount + 1
		End If
	Next
	if offset < 0 then offset = 0
	redim aArray(aCount-1+offset)        'Resize original array
	for x = 0 to aCount-1                'set objects back into original array
		if IsObject(a(x)) then 
			Set aArray(x) = a(x)
		Else
			aArray(x) = a(x)
		End If
	Next
End Sub

' Used for flipper correction and rubber dampeners
Sub ShuffleArrays(aArray1, aArray2, offset)
	ShuffleArray aArray1, offset
	ShuffleArray aArray2, offset
End Sub

' Used for flipper correction, rubber dampeners, and drop targets
Function BallSpeed(ball) 'Calculates the ball speed
	BallSpeed = SQR(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

' Used for flipper correction and rubber dampeners
Function PSlope(Input, X1, Y1, X2, Y2)        'Set up line via two points, no clamping. Input X, output Y
	dim x, y, b, m : x = input : m = (Y2 - Y1) / (X2 - X1) : b = Y2 - m*X2
	Y = M*x+b
	PSlope = Y
End Function

' Used for flipper correction
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

' Used for flipper correction and rubber dampeners
Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	dim y 'Y output
	dim L 'Line
	dim ii : for ii = 1 to uBound(xKeyFrame)        'find active line
		if xInput <= xKeyFrame(ii) then L = ii : exit for : end if
	Next
	if xInput > xKeyFrame(uBound(xKeyFrame) ) then L = uBound(xKeyFrame)        'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )

	if xInput <= xKeyFrame(lBound(xKeyFrame) ) then Y = yLvl(lBound(xKeyFrame) )         'Clamp lower
	if xInput >= xKeyFrame(uBound(xKeyFrame) ) then Y = yLvl(uBound(xKeyFrame) )        'Clamp upper

	LinearEnvelope = Y
End Function


'******************************************************
'  FLIPPER TRICKS 
'******************************************************

RightFlipper.timerinterval=1
Rightflipper.timerenabled=True

sub RightFlipper_timer()
	FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
	FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState
	FlipperNudge RightFlipper, RFEndAngle, RFEOSNudge, LeftFlipper, LFEndAngle
	FlipperNudge LeftFlipper, LFEndAngle, LFEOSNudge,  RightFlipper, RFEndAngle
end sub

Dim LFEOSNudge, RFEOSNudge

Sub FlipperNudge(Flipper1, Endangle1, EOSNudge1, Flipper2, EndAngle2)
	Dim b, BOT
	BOT = GetBalls

	If Flipper1.currentangle = Endangle1 and EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		'debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
		If Flipper2.currentangle = EndAngle2 Then 
			For b = 0 to Ubound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper1) Then
					'Debug.Print "ball in flip1. exit"
					exit Sub
				end If
			Next
			For b = 0 to Ubound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper2) Then
					BOT(b).velx = BOT(b).velx / 1.3
					BOT(b).vely = BOT(b).vely - 0.5
				end If
			Next
		End If
	Else 
		If Flipper1.currentangle <> EndAngle1 then 
			EOSNudge1 = 0
		end if
	End If
End Sub

'*****************
' Maths
'*****************
Dim PI: PI = 4*Atn(1)

Function dSin(degrees)
	dsin = sin(degrees * Pi/180)
End Function

Function dCos(degrees)
	dcos = cos(degrees * Pi/180)
End Function

Function Atn2(dy, dx)
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

'*************************************************
'  Check ball distance from Flipper for Rem
'*************************************************

Function Distance(ax,ay,bx,by)
	Distance = SQR((ax - bx)^2 + (ay - by)^2)
End Function

Function DistancePL(px,py,ax,ay,bx,by) ' Distance between a point and a line where point is px,py
	DistancePL = ABS((by - ay)*px - (bx - ax) * py + bx*ay - by*ax)/Distance(ax,ay,bx,by)
End Function

Function Radians(Degrees)
	Radians = Degrees * PI /180
End Function

Function AnglePP(ax,ay,bx,by)
	AnglePP = Atn2((by - ay),(bx - ax))*180/PI
End Function

Function DistanceFromFlipper(ballx, bally, Flipper)
	DistanceFromFlipper = DistancePL(ballx, bally, Flipper.x, Flipper.y, Cos(Radians(Flipper.currentangle+90))+Flipper.x, Sin(Radians(Flipper.currentangle+90))+Flipper.y)
End Function

Function FlipperTrigger(ballx, bally, Flipper)
	Dim DiffAngle
	DiffAngle  = ABS(Flipper.currentangle - AnglePP(Flipper.x, Flipper.y, ballx, bally) - 90)
	If DiffAngle > 180 Then DiffAngle = DiffAngle - 360

	If DistanceFromFlipper(ballx,bally,Flipper) < 48 and DiffAngle <= 90 and Distance(ballx,bally,Flipper.x,Flipper.y) < Flipper.Length Then
		FlipperTrigger = True
	Else
		FlipperTrigger = False
	End If        
End Function


'*************************************************
'  End - Check ball distance from Flipper for Rem
'*************************************************

dim LFPress, RFPress, LFCount, RFCount
dim LFState, RFState
dim EOST, EOSA,Frampup, FElasticity,FReturn
dim RFEndAngle, LFEndAngle

LFState = 1
RFState = 1
EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
'Const EOSTnew = 1 'EM's to late 80's
Const EOSTnew = 0.8 '90's and later
Const EOSAnew = 1
Const EOSRampup = 0
Dim SOSRampup
Select Case FlipperCoilRampupMode 
	Case 0:
		SOSRampup = 2.5
	Case 1:
		SOSRampup = 6
	Case 2:
		SOSRampup = 8.5
End Select

Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
'Const EOSReturn = 0.055  'EM's
Const EOSReturn = 0.045  'late 70's to mid 80's
'Const EOSReturn = 0.035  'mid 80's to early 90's
'Const EOSReturn = 0.025  'mid 90's and later

LFEndAngle = Leftflipper.endangle
RFEndAngle = RightFlipper.endangle

Sub FlipperActivate(Flipper, FlipperPress)
	FlipperPress = 1
	Flipper.Elasticity = FElasticity

	Flipper.eostorque = EOST         
	Flipper.eostorqueangle = EOSA         
End Sub

Sub FlipperDeactivate(Flipper, FlipperPress)
	FlipperPress = 0
	Flipper.eostorqueangle = EOSA
	Flipper.eostorque = EOST*EOSReturn/FReturn


	If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 0.1 Then
		Dim b, BOT
		BOT = GetBalls

		For b = 0 to UBound(BOT)
			If Distance(BOT(b).x, BOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
				If BOT(b).vely >= -0.4 Then BOT(b).vely = -0.4
			End If
		Next
	End If
End Sub

Sub FlipperTricks (Flipper, FlipperPress, FCount, FEndAngle, FState) 
	Dim Dir
	Dir = Flipper.startangle/Abs(Flipper.startangle)        '-1 for Right Flipper

	If Abs(Flipper.currentangle) > Abs(Flipper.startangle) - 0.05 Then
		If FState <> 1 Then
			Flipper.rampup = SOSRampup 
			Flipper.endangle = FEndAngle - 3*Dir
			Flipper.Elasticity = FElasticity * SOSEM
			FCount = 0 
			FState = 1
		End If
	ElseIf Abs(Flipper.currentangle) <= Abs(Flipper.endangle) and FlipperPress = 1 then
		if FCount = 0 Then FCount = GameTime

		If FState <> 2 Then
			Flipper.eostorqueangle = EOSAnew
			Flipper.eostorque = EOSTnew
			Flipper.rampup = EOSRampup                        
			Flipper.endangle = FEndAngle
			FState = 2
		End If
	Elseif Abs(Flipper.currentangle) > Abs(Flipper.endangle) + 0.01 and FlipperPress = 1 Then 
		If FState <> 3 Then
			Flipper.eostorque = EOST        
			Flipper.eostorqueangle = EOSA
			Flipper.rampup = Frampup
			Flipper.Elasticity = FElasticity
			FState = 3
		End If

	End If
End Sub

Const LiveDistanceMin = 30  'minimum distance in vp units from flipper base live catch dampening will occur
Const LiveDistanceMax = 114  'maximum distance in vp units from flipper base live catch dampening will occur (tip protection)

Sub CheckLiveCatch(ball, Flipper, FCount, parm) 'Experimental new live catch
	Dim Dir
	Dir = Flipper.startangle/Abs(Flipper.startangle)    '-1 for Right Flipper
	Dim LiveCatchBounce                                                                                                                        'If live catch is not perfect, it won't freeze ball totally
	Dim CatchTime : CatchTime = GameTime - FCount

	if CatchTime <= LiveCatch and parm > 6 and ABS(Flipper.x - ball.x) > LiveDistanceMin and ABS(Flipper.x - ball.x) < LiveDistanceMax Then
		if CatchTime <= LiveCatch*0.5 Then                                                'Perfect catch only when catch time happens in the beginning of the window
			LiveCatchBounce = 0
		else
			LiveCatchBounce = Abs((LiveCatch/2) - CatchTime)        'Partial catch when catch happens a bit late
		end If

		If LiveCatchBounce = 0 and ball.velx * Dir > 0 Then ball.velx = 0
		ball.vely = LiveCatchBounce * (32 / LiveCatch) ' Multiplier for inaccuracy bounce
		ball.angmomx= 0
		ball.angmomy= 0
		ball.angmomz= 0
	End If
End Sub


'******************************************************
'****  END FLIPPER CORRECTIONS
'******************************************************








'******************************************************
'****  PHYSICS DAMPENERS
'******************************************************
'
' These are data mined bounce curves, 
' dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
' Requires tracking ballspeed to calculate COR



Sub dPosts_Hit(idx) 
	RubbersD.dampen Activeball
	TargetBouncer Activeball, 1
End Sub

Sub dSleeves_Hit(idx) 
	SleevesD.Dampen Activeball
	TargetBouncer Activeball, 0.7
End Sub


dim RubbersD : Set RubbersD = new Dampener        'frubber
RubbersD.name = "Rubbers"
RubbersD.debugOn = False        'shows info in textbox "TBPout"
RubbersD.Print = False        'debug, reports in debugger (in vel, out cor)
'cor bounce curve (linear)
'for best results, try to match in-game velocity as closely as possible to the desired curve
'RubbersD.addpoint 0, 0, 0.935        'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 0.96        'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.96
RubbersD.addpoint 2, 5.76, 0.967        'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64        'there's clamping so interpolate up to 56 at least

dim SleevesD : Set SleevesD = new Dampener        'this is just rubber but cut down to 85%...
SleevesD.name = "Sleeves"
SleevesD.debugOn = False        'shows info in textbox "TBPout"
SleevesD.Print = False        'debug, reports in debugger (in vel, out cor)
SleevesD.CopyCoef RubbersD, 0.85

Class Dampener
	Public Print, debugOn 'tbpOut.text
	public name, Threshold         'Minimum threshold. Useful for Flippers, which don't have a hit threshold.
	Public ModIn, ModOut
	Private Sub Class_Initialize : redim ModIn(0) : redim Modout(0): End Sub 

	Public Sub AddPoint(aIdx, aX, aY) 
		ShuffleArrays ModIn, ModOut, 1 : ModIn(aIDX) = aX : ModOut(aIDX) = aY : ShuffleArrays ModIn, ModOut, 0
		if gametime > 100 then Report
	End Sub

	public sub Dampen(aBall)
		if threshold then if BallSpeed(aBall) < threshold then exit sub end if end if
		dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id)+0.0000001)
		coef = desiredcor / realcor 
		if debugOn then str = name & " in vel:" & round(cor.ballvel(aBall.id),2 ) & vbnewline & "desired cor: " & round(desiredcor,4) & vbnewline & _
		"actual cor: " & round(realCOR,4) & vbnewline & "ballspeed coef: " & round(coef, 3) & vbnewline 
		if Print then debug.print Round(cor.ballvel(aBall.id),2) & ", " & round(desiredcor,3)

		aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
		if debugOn then TBPout.text = str
	End Sub

	Public Sub CopyCoef(aObj, aCoef) 'alternative addpoints, copy with coef
		dim x : for x = 0 to uBound(aObj.ModIn)
			addpoint x, aObj.ModIn(x), aObj.ModOut(x)*aCoef
		Next
	End Sub


	Public Sub Report()         'debug, reports all coords in tbPL.text
		if not debugOn then exit sub
		dim a1, a2 : a1 = ModIn : a2 = ModOut
		dim str, x : for x = 0 to uBound(a1) : str = str & x & ": " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		TBPout.text = str
	End Sub

End Class



'******************************************************
'  TRACK ALL BALL VELOCITIES
'  FOR RUBBER DAMPENER AND DROP TARGETS
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

Sub Rdampen_timer
	Cor.Update
	DoDTAnim
End Sub



'******************************************************
'****  END PHYSICS DAMPENERS
'******************************************************




'*******************************************
'  VPW Rubberizer by Iaakki
'*******************************************

' iaakki Rubberizer
sub Rubberizer(parm)
	if parm < 10 And parm > 2 And Abs(activeball.angmomz) < 10 then
		'debug.print "parm: " & parm & " momz: " & activeball.angmomz &" vely: "& activeball.vely
		activeball.angmomz = activeball.angmomz * 1.2
		activeball.vely = activeball.vely * 1.2
		'debug.print ">> newmomz: " & activeball.angmomz&" newvely: "& activeball.vely
	Elseif parm <= 2 and parm > 0.2 and activeball.vely < 0 Then
		'debug.print "* parm: " & parm & " momz: " & activeball.angmomz &" vely: "& activeball.vely
		activeball.angmomz = activeball.angmomz * -1.1
		activeball.vely = activeball.vely * 1.4
		'debug.print "**** >> newmomz: " & activeball.angmomz&" newvely: "& activeball.vely
	end if
end sub


' apophis Rubberizer
sub Rubberizer2(parm)
	if parm < 10 And parm > 2 And Abs(activeball.angmomz) < 10 then
		'debug.print "parm: " & parm & " momz: " & activeball.angmomz &" vely: "& activeball.vely
		activeball.angmomz = -activeball.angmomz * 2
		activeball.vely = activeball.vely * 1.2
		'debug.print ">> newmomz: " & activeball.angmomz&" newvely: "& activeball.vely
	Elseif parm <= 2 and parm > 0.2 and activeball.vely < 0 Then
		'debug.print "* parm: " & parm & " momz: " & activeball.angmomz &" vely: "& activeball.vely
		activeball.angmomz = -activeball.angmomz * 0.5
		activeball.vely = activeball.vely * (1.2 + rnd(1)/3 )
		'debug.print "**** >> newmomz: " & activeball.angmomz&" newvely: "& activeball.vely
	end if
end sub



'******************************************************
' VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

sub TargetBouncer(aBall,defvalue)
    dim zMultiplier, vel, vratio
    if TargetBouncerEnabled = 1 and aball.z < 30 then
        'debug.print "velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
        vel = BallSpeed(aBall)
        if aBall.velx = 0 then vratio = 1 else vratio = aBall.vely/aBall.velx
        Select Case Int(Rnd * 6) + 1
            Case 1: zMultiplier = 0.2*defvalue
			Case 2: zMultiplier = 0.25*defvalue
            Case 3: zMultiplier = 0.3*defvalue
			Case 4: zMultiplier = 0.4*defvalue
            Case 5: zMultiplier = 0.45*defvalue
            Case 6: zMultiplier = 0.5*defvalue
        End Select
        aBall.velz = abs(vel * zMultiplier * TargetBouncerFactor)
        aBall.velx = sgn(aBall.velx) * sqr(abs((vel^2 - aBall.velz^2)/(1+vratio^2)))
        aBall.vely = aBall.velx * vratio
        'debug.print "---> velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
        'debug.print "conservation check: " & BallSpeed(aBall)/vel
    elseif TargetBouncerEnabled = 2 and aball.z < 30 then
		'debug.print "velz: " & activeball.velz
		Select Case Int(Rnd * 4) + 1
			Case 1: zMultiplier = defvalue+1.1
			Case 2: zMultiplier = defvalue+1.05
			Case 3: zMultiplier = defvalue+0.7
			Case 4: zMultiplier = defvalue+0.3
		End Select
		aBall.velz = aBall.velz * zMultiplier * TargetBouncerFactor
		'debug.print "----> velz: " & activeball.velz
		'debug.print "conservation check: " & BallSpeed(aBall)/vel
	end if
end sub



'******************************************************
'             Fog Animation and Sound FX
'******************************************************

Dim FogCnt
FogCnt = 0
FogAnimation.Interval=17*2  '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)



Sub FogAnimation_Timer
	'Initialize the animation
	If FogFlasher.visible = False Then 
		FogFlasher.visible = True
		FogCnt = 0
	End If

	'Select the correct frame
	If FogCnt > 99 Then
		FogFlasher.imageA = "fog_" & FogCnt
	Elseif FogCnt > 9 Then
		FogFlasher.imageA = "fog_0" & FogCnt
	Else
		FogFlasher.imageA = "fog_00" & FogCnt
	End If
	FogCnt = FogCnt + 1

	'Finish animation
	If FogCnt > 271 Then 
		FogCnt = 0
		FogAnimation.Enabled = False
		FogFlasher.visible = False
	End If
End Sub

Dim FogDelay1
FogDelay1 = 0

Sub EndDelay1_Timer
    If EndDelay1.Enabled = True Then
       If FogDelay1 > 48 Then
         FogAnimation.Enabled = True
         EndDelay1.Enabled = False
         FogDelay1 = 0
       Else
    FogDelay1 = FogDelay1 + 1
   End If
 End If
End Sub


Dim FogDelay2
FogDelay2 = 0

Sub EndDelay2_Timer
 If EndDelay2.Enabled = True Then
    If FogDelay2 > 48 Then
     OutroMusic
     EndDelay2.Enabled = False
     FogDelay2 = 0
    Else
    FogDelay2 = FogDelay2 + 1
   End If
 End If
End Sub

Dim FogDelay3
FogDelay3 = 0

Sub EndDelay3_Timer
 If EndDelay3.Enabled = True Then
    If FogDelay3 > 54 Then
     EndDelay3.Enabled = False
     FogDelay3 = 0
    Else
    FogDelay3 = FogDelay3 + 1
   End If
 End If
End Sub

Sub FogRandomSfx
If EndGameFog = True then
    Dim fg
    fg = INT(2 * RND(1) )
    If fg = 0 then PlaySound "Fog-End-1" End If
    If fg = 1 then PlaySound "Fog-End-2" End If
End If

End Sub






'******************************************************
'****  DROP TARGETS by Rothbauerw
'******************************************************
' This solution improves the physics for drop targets to create more realistic behavior. It allows the ball 
' to move through the target enabling the ability to score more than one target with a well placed shot.
' It also handles full drop target animation, including deflection on hit and a slight lift when the drop 
' targets raise, switch handling, bricking, and popping the ball up if it's over the drop target when it raises.
'
' For each drop target, we'll use two wall objects for physics calculations and one primitive for visuals and   
' animation. We will not use target objects.  Place your drop target primitive the same as you would a VP drop target. 
' The primitive should have it's pivot point centered on the x and y axis and at or just below the playfield 
' level on the z axis. Orientation needs to be set using Rotz and bending deflection using Rotx. You'll find a hooded 
' target mesh in this table's example. It uses the same texture map as the VP drop targets.


'******************************************************
'  DROP TARGETS INITIALIZATION
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
Dim DTR1, DTR2, DTR3, DTR4
Dim DTBk1, DTBk2, DTBk3, DTBk4, DTBk5, DTBk6, DTBk7, DTBk8
Dim DTBu1, DTBu2, DTBu3, DTBu4

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
'       isDropped:  Boolean which determines whether a drop target is dropped. Set to false if they are initially raised, true if initially dropped.


Set DTR1= (new DropTarget)(DTRed1, DTRed1a, DTRed1p, 0, 0, false)
Set DTR2= (new DropTarget)(DTRed2, DTRed2a, DTRed2p, 1, 0, false)
Set DTR3= (new DropTarget)(DTRed3, DTRed3a, DTRed3p, 2, 0, false)
Set DTR4= (new DropTarget)(DTRed4, DTRed4a, DTRed4p, 3, 0, false)

Set DTBk1= (new DropTarget)(DTBlack1, DTBlack1a, DTBlack1p, 4, 0, false)
Set DTBk2= (new DropTarget)(DTBlack2, DTBlack2a, DTBlack2p, 5, 0, false)
Set DTBk3= (new DropTarget)(DTBlack3, DTBlack3a, DTBlack3p, 6, 0, false)
Set DTBk4= (new DropTarget)(DTBlack4, DTBlack4a, DTBlack4p, 7, 0, false)
Set DTBk5= (new DropTarget)(DTBlack5, DTBlack5a, DTBlack5p, 8, 0, false)
Set DTBk6= (new DropTarget)(DTBlack6, DTBlack6a, DTBlack6p, 9, 0, false)
Set DTBk7= (new DropTarget)(DTBlack7, DTBlack7a, DTBlack7p, 10, 0, false)
Set DTBk8= (new DropTarget)(DTBlack8, DTBlack8a, DTBlack8p, 11, 0, false)

Set DTBu1= (new DropTarget)(DTBlue1, DTBlue1a, DTBlue1p, 12, 0, false)
Set DTBu2= (new DropTarget)(DTBlue2, DTBlue2a, DTBlue2p, 13, 0, false)
Set DTBu3= (new DropTarget)(DTBlue3, DTBlue3a, DTBlue3p, 14, 0, false)
Set DTBu4= (new DropTarget)(DTBlue4, DTBlue4a, DTBlue4p, 15, 0, false)


Dim DTArray
DTArray = Array(DTR1,DTR2,DTR3,DTR4,DTBk1,DTBk2,DTBk3,DTBk4,DTBk5,DTBk6,DTBk7,DTBk8,DTBu1,DTBu2,DTBu3,DTBu4)

'Configure the behavior of Drop Targets.
Const DTDropSpeed = 110 'in milliseconds
Const DTDropUpSpeed = 40 'in milliseconds
Const DTDropUnits = 44 'VP units primitive drops so top of at or below the playfield
Const DTDropUpUnits = 10 'VP units primitive raises above the up position on drops up
Const DTMaxBend = 8 'max degrees primitive rotates when hit
Const DTDropDelay = 20 'time in milliseconds before target drops (due to friction/impact of the ball)
Const DTRaiseDelay = 40 'time in milliseconds before target drops back to normal up position after the solenoid fires to raise the target
Const DTBrickVel = 30 'velocity at which the target will brick, set to '0' to disable brick

Const DTEnableBrick = 0 'Set to 0 to disable bricking, 1 to enable bricking
Const DTHitSound = "" 'Drop Target Hit sound
Const DTDropSound = "DropTarget_Down" 'Drop Target Drop sound
Const DTResetSound = "DropTarget_Up" 'Drop Target reset sound

Const DTMass = 0.2 'Mass of the Drop Target (between 0 and 1), higher values provide more resistance


'******************************************************
'  DROP TARGETS FUNCTIONS
'******************************************************

Sub DTHit(switch)
	Dim i
	i = DTArrayID(switch)
	DTArray(i).animate =  DTCheckBrick(Activeball,DTArray(i).prim)
	If DTArray(i).animate = 1 or DTArray(i).animate = 3 or DTArray(i).animate = 4 Then
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

	If perpvel > 0 and  perpvelafter <= 0 Then
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
End Function


Sub DoDTAnim()
	Dim i
	For i=0 to Ubound(DTArray)
		DTArray(i).animate = DTAnimate(DTArray(i).primary,DTArray(i).secondary,DTArray(i).prim,DTArray(i).sw,DTArray(i).animate)
	Next
End Sub

Function DTAnimate(primary, secondary, prim, switch,  animate)
	dim transz, switchid
	Dim animtime, rangle
	switchid = switch

	rangle = prim.rotz * PI / 180

	DTAnimate = animate

	if animate = 0  Then
		primary.uservalue = 0
		DTAnimate = 0
		Exit Function
	Elseif primary.uservalue = 0 then 
		primary.uservalue = gametime
	end if

	animtime = gametime - primary.uservalue

	If (animate = 1 or animate = 4) and animtime < DTDropDelay Then
		primary.collidable = 0
	If animate = 1 then secondary.collidable = 1 else secondary.collidable= 0
		prim.rotx = DTMaxBend * cos(rangle)
		prim.roty = DTMaxBend * sin(rangle)
		DTAnimate = animate
		Exit Function
		elseif (animate = 1 or animate = 4) and animtime > DTDropDelay Then
		primary.collidable = 0
		If animate = 1 then secondary.collidable = 1 else secondary.collidable= 0
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
			Targets_Drop(Switchid) 'controller.Switch(Switchid) = 1
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
				If InRotRect(BOT(b).x,BOT(b).y,prim.x, prim.y, prim.rotz, -25,-10,25,-10,25,25,-25,25) and BOT(b).z < prim.z+DTDropUnits+25 Then
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
		'controller.Switch(Switchid) = 0

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


'******************************************************
'  DROP TARGET
'  SUPPORTING FUNCTIONS 
'******************************************************


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

Function InRotRect(ballx,bally,px,py,angle,ax,ay,bx,by,cx,cy,dx,dy)
    Dim rax,ray,rbx,rby,rcx,rcy,rdx,rdy
    Dim rotxy
    rotxy = RotPoint(ax,ay,angle)
    rax = rotxy(0)+px : ray = rotxy(1)+py
    rotxy = RotPoint(bx,by,angle)
    rbx = rotxy(0)+px : rby = rotxy(1)+py
    rotxy = RotPoint(cx,cy,angle)
    rcx = rotxy(0)+px : rcy = rotxy(1)+py
    rotxy = RotPoint(dx,dy,angle)
    rdx = rotxy(0)+px : rdy = rotxy(1)+py

    InRotRect = InRect(ballx,bally,rax,ray,rbx,rby,rcx,rcy,rdx,rdy)
End Function

Function RotPoint(x,y,angle)
    dim rx, ry
    rx = x*dCos(angle) - y*dSin(angle)
    ry = x*dSin(angle) + y*dCos(angle)
    RotPoint = Array(rx,ry)
End Function


'******************************************************
'****  END DROP TARGETS
'******************************************************

